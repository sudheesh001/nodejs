package sbtassembly

import sbt._
import Keys._
import scala.collection.mutable
import scala.io.Source
import Project.Initialize
import java.io.{ PrintWriter, FileOutputStream, File }
import java.security.MessageDigest

object Plugin extends sbt.Plugin {
  import AssemblyKeys._

  // Keep track of the source package of mappings that come from a jar, so we can
  // sha1 the jar instead of the unpacked packages when determining whether to rebuild
  case class MappingSet( sourcePackage : Option[File], mappings : Seq[(File, String)] )
  {
    def dependencyFiles = sourcePackage match
    {
      case Some(f)  => Seq(f)
      case None     => mappings.map(_._1)
    }
  }
    
  object AssemblyKeys {
    lazy val assembly = TaskKey[File]("assembly", "Builds a single-file deployable jar.")
    lazy val packageScala      = TaskKey[File]("assembly-package-scala", "Produces the scala artifact.")
    lazy val packageDependency = TaskKey[File]("assembly-package-dependency", "Produces the dependency artifact.")
  
    lazy val assembleArtifact  = SettingKey[Boolean]("assembly-assemble-artifact", "Enables (true) or disables (false) assembling an artifact.")
    lazy val assemblyOption    = SettingKey[AssemblyOption]("assembly-option")
    lazy val jarName           = TaskKey[String]("assembly-jar-name")
    lazy val defaultJarName    = TaskKey[String]("assembly-default-jar-name")
    lazy val outputPath        = TaskKey[File]("assembly-output-path")
    lazy val excludedFiles     = SettingKey[Seq[File] => Seq[File]]("assembly-excluded-files")
    lazy val excludedJars      = TaskKey[Classpath]("assembly-excluded-jars")
    lazy val assembledMappings = TaskKey[File => Seq[MappingSet]]("assembly-assembled-mappings")
    lazy val mergeStrategy     = SettingKey[String => MergeStrategy]("assembly-merge-strategy", "mapping from archive member path to merge strategy")
    lazy val assemblyDirectory = SettingKey[File]("assembly-directory")
    
    lazy val assemblyCacheOutput    = SettingKey[Boolean]("assembly-cache-output")
    lazy val assemblyCacheUnzip     = SettingKey[Boolean]("assembly-cache-unzip", "Cache the results of unzipping dependency jars from run to run")
  }
  
  /**
   * MergeStrategy is invoked if more than one source file is mapped to the 
   * same target path. Its arguments are the tempDir (which is deleted after
   * packaging) and the sequence of source files, and it shall return the
   * file to be included in the assembly (or throw an exception).
   */
  abstract class MergeStrategy extends Function1[(File, String, Seq[File]), Either[String, Seq[(File, String)]]] {
    def name: String
    def notifyThreshold = 2
    def detailLogLevel = Level.Warn
    def summaryLogLevel = Level.Warn
  }

  // (File, Seq[File]) => (Either[String, File], String)
  object MergeStrategy {
    val first: MergeStrategy = new MergeStrategy {
      val name = "first"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] =
        Right(Seq(args._3.head -> args._2))
    }
    val last: MergeStrategy = new MergeStrategy {
      val name = "last"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] =
        Right(Seq(args._3.last -> args._2))
    }
    val singleOrError: MergeStrategy = new MergeStrategy {
      val name = "singleOrError"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] =
        if (args._3.size == 1) Right(Seq(args._3.head -> args._2))
        else Left("found multiple files for same target path:" +
          filenames(args._1, args._3).mkString("\n", "\n", ""))
    }
    val concat: MergeStrategy = new MergeStrategy {
      val name = "concat"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] = {
        val file = File.createTempFile("sbtMergeTarget", ".tmp", args._1)
        val out = new FileOutputStream(file)
        try {
          args._3 foreach (f => IO.transfer(f, out))
          Right(Seq(file -> args._2))
        } finally {
          out.close()
        }
      }
    }
    val filterDistinctLines: MergeStrategy = new MergeStrategy {
      val name = "filterDistinctLines"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] = {
        val lines = args._3 flatMap (IO.readLines(_, IO.utf8))
        val unique = (Vector.empty[String] /: lines)((v, l) => if (v contains l) v else v :+ l)
        val file = File.createTempFile("sbtMergeTarget", ".tmp", args._1)
        IO.writeLines(file, unique, IO.utf8)
        Right(Seq(file -> args._2))
      }
    }
    val deduplicate: MergeStrategy = new MergeStrategy {
      val name = "deduplicate"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] =
        if (args._3.size == 1) Right(Seq(args._3.head -> args._2))
        else {
          val fingerprints = Set() ++ (args._3 map (sha1content))
          if (fingerprints.size == 1) Right(Seq(args._3.head -> args._2))
          else Left("different file contents found in the following:" +
              filenames(args._1, args._3).mkString("\n", "\n", ""))
        }
      override def detailLogLevel = Level.Debug
      override def summaryLogLevel = Level.Info
    }
    val rename: MergeStrategy = new MergeStrategy {
      val name = "rename"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] =
        Right(args._3 flatMap { f =>
          if(!f.exists) Seq.empty
          else if(f.isDirectory && (f ** "*.class").get.nonEmpty) Seq(f -> args._2)
          else AssemblyUtils.sourceOfFileForMerge(args._1, f) match {
            case (dir, base, path, false) => Seq(f -> args._2)
            case (jar, base, path, true) =>
              val dest = new File(f.getParent, appendJarName(f.getName, jar))
              IO.move(f, dest)
              val result = Seq(dest -> appendJarName(args._2, jar))
              if (dest.isDirectory) ((dest ** (-DirectoryFilter))) x relativeTo(base)
              else result
          }
        })

      def appendJarName(source: String, jar: File): String =
        FileExtension.replaceFirstIn(source, "") +
          "_" + FileExtension.replaceFirstIn(jar.getName, "") +
          FileExtension.findFirstIn(source).getOrElse("")

      override def notifyThreshold = 1
    }
    val discard: MergeStrategy = new MergeStrategy {
      val name = "discard"
      def apply(args: (File, String, Seq[File])): Either[String, Seq[(File, String)]] =
        Right(Nil)   
      override def notifyThreshold = 1
    }
  }
  
  private val FileExtension = """([.]\w+)$""".r

  private def filenames(tempDir: File, fs: Seq[File]): Seq[String] =
    for(f <- fs) yield {
      AssemblyUtils.sourceOfFileForMerge(tempDir, f) match {
        case (path, base, subDirPath, false) => subDirPath
        case (jar, base, subJarPath, true) => jar + ":" + subJarPath
      }
    }

  private def assemblyTask(out: File, po: Seq[PackageOption], mappings: File => Seq[MappingSet],
      strats: String => MergeStrategy, tempDir: File, cacheOutput: Boolean, cacheDir: File, cacheUnzip: Boolean, log: Logger): File =
    Assembly(out, po, mappings, strats, tempDir, cacheOutput, cacheDir, cacheUnzip, log)

  object Assembly {
    def apply(out: File, po: Seq[PackageOption], mappings: File => Seq[MappingSet],
        strats: String => MergeStrategy, tempDir: File, cacheOutput: Boolean, cacheDir: File, cacheUnzip: Boolean, log: Logger): File = {
      import Tracked.{inputChanged, outputChanged}
      import Types.:+:
      import Cache._
      import FileInfo.{hash, exists}

      if ( !cacheUnzip ) IO.delete( tempDir )
      if ( !tempDir.exists ) tempDir.mkdir()
      
      val mappingSets = mappings(tempDir)
      val ms : Seq[(File, String)] = applyStrategies(mappingSets, strats, tempDir, log)
      def makeJar {
        val config = new Package.Configuration(ms, out, po)
        Package(config, cacheDir, log)
      }
      val cachedMakeJar = inputChanged(cacheDir / "assembly-inputs") { (inChanged, inputs: Seq[Byte]) =>
        outputChanged(cacheDir / "assembly-outputs") { (outChanged, jar: PlainFileInfo) => 
          if (inChanged) {
            log.info("SHA-1: " + inputs.map( b => "%02x".format(b) ).mkString)
          } // if
          if (inChanged || outChanged) makeJar
          else log.info("Assembly up to date: " + jar.file)        
        }
      }

      lazy val inputs = sha1.digest((mappingSets flatMap { _.dependencyFiles } map {hash.apply}).toString.getBytes("UTF-8")).toSeq
      if (cacheOutput) {
        log.info("Checking every *.class/*.jar file's SHA-1.")
        cachedMakeJar(inputs)(() => exists(out))  
      }
      else makeJar
      out
    }
    def applyStrategies(srcSets: Seq[MappingSet], strats: String => MergeStrategy,
        tempDir: File, log: Logger): Seq[(File, String)] = {
      val srcs = srcSets.flatMap( _.mappings )
      val counts = scala.collection.mutable.Map[MergeStrategy, Int]().withDefaultValue(0)
      def applyStrategy(strategy: MergeStrategy, name: String, files: Seq[(File, String)]): Seq[(File, String)] = {
        if (files.size >= strategy.notifyThreshold) {
          log.log(strategy.detailLogLevel, "Merging '%s' with strategy '%s'".format(name, strategy.name))
          counts(strategy) += 1
        }
        strategy((tempDir, name, files map (_._1))) match {
          case Right(f)  => f
          case Left(err) => throw new RuntimeException(strategy.name + ": " + err)
        }
      }
      val renamed = srcs.groupBy(_._2).flatMap { case (name, files) =>
        val strategy = strats(name)
        if (strategy == MergeStrategy.rename) applyStrategy(strategy, name, files)
        else files
      } (scala.collection.breakOut)
      // this step is necessary because some dirs may have been renamed above
      val cleaned: Seq[(File, String)] = renamed filter { pair =>
        (!pair._1.isDirectory) && pair._1.exists
      }
      val mod: Seq[(File, String)] = cleaned.groupBy(_._2).flatMap { case (name, files) =>
        val strategy = strats(name)
        if (strategy != MergeStrategy.rename) applyStrategy(strategy, name, files)
        else files
      } (scala.collection.breakOut)
      counts.keysIterator.toList.sortBy(_.name) foreach { strat =>
        val count = counts(strat)
        log.log(strat.summaryLogLevel, "Strategy '%s' was applied to ".format(strat.name) + (count match {
          case 1 => "a file"
          case n => n.toString + " files"
        }) + (strat.detailLogLevel match {
          case Level.Debug => " (Run the task at debug level to see details)"
          case _ => ""
        })) 
      }
      mod
    }
  }

  private def assemblyExcludedFiles(bases: Seq[File]): Seq[File] = Nil  
  private def sha1 = MessageDigest.getInstance("SHA-1")
  private def sha1content(f: File): String =
    Vector(sha1.digest(IO.readBytes(f)): _*) map {"%02x".format(_)} mkString
  private def sha1name(f: File): String = 
    Vector(sha1.digest(f.getCanonicalPath.getBytes): _*) map {"%02x".format(_)} mkString

  // even though fullClasspath includes deps, dependencyClasspath is needed to figure out
  // which jars exactly belong to the deps for packageDependency option.
  private def assemblyAssembledMappings(tempDir: File, classpath: Classpath, dependencies: Classpath,
      ao: AssemblyOption, ej: Classpath, cacheUnzip: Boolean, log: Logger) = {
    import sbt.classpath.ClasspathUtilities

    val (libs, dirs) = classpath.map(_.data).sorted.partition(ClasspathUtilities.isArchive)
    val depLibs = dependencies.map(_.data).sorted.partition(ClasspathUtilities.isArchive)._1
    val excludedJars = ej map {_.data}
    val libsFiltered = libs flatMap {
      case jar if excludedJars contains jar.asFile => None
      case jar if List("scala-library.jar", "scala-compiler.jar") contains jar.asFile.getName =>
        if (ao.includeScala) Some(jar) else None
      case jar if depLibs contains jar.asFile =>
        if (ao.includeDependency) Some(jar) else None
      case jar =>
        if (ao.includeBin) Some(jar) else None
    }
    val dirsFiltered = dirs flatMap {
      case dir if depLibs contains dir.asFile =>
        if (ao.includeDependency) Some(dir)
        else None
      case dir =>
        if (ao.includeBin) Some(dir)
        else None
    } map { dir =>
      val hash = sha1name(dir)
      IO.write(tempDir / (hash + "_dir.dir"), dir.getCanonicalPath, IO.utf8, false)
      val dest = tempDir / (hash + "_dir")
      dest.mkdir()
      IO.copyDirectory(dir, dest)
      dest
    }
    
    val jarDirs = for(jar <- libsFiltered.par) yield {
      val jarName = jar.asFile.getName
      
      val hash = sha1name(jar) + "_" + sha1content(jar)
      val jarNamePath = tempDir / (hash + ".jarName")
      val dest = tempDir / hash
      // If the jar name path does not exist, or is not for this jar, unzip the jar
      if ( !cacheUnzip || !jarNamePath.exists || IO.read(jarNamePath) != jar.getCanonicalPath )
      {
        log.info("Including: %s".format(jarName))
        IO.delete(dest)
        dest.mkdir()
        IO.unzip(jar, dest)
        IO.delete(ao.exclude(Seq(dest)))
        
        // Write the jarNamePath at the end to minimise the chance of having a
        // corrupt cache if the user aborts the build midway through
        IO.write(jarNamePath, jar.getCanonicalPath, IO.utf8, false)
      }
      else log.info("Including from cache: %s".format(jarName))
      
      
      (dest, jar)
    }


    val base : Seq[File] = dirsFiltered ++ jarDirs.map( _._1 )
    
    def getMappings( rootDir : File ) =
    {
      val descendendants = ((rootDir ** "*") --- ao.exclude(base) --- base).get filter { _.exists }
      
      descendendants x relativeTo(base)
    }
    
    dirsFiltered.map( d => MappingSet( None, getMappings(d) ) ) ++ jarDirs.map { case (d, j) => MappingSet( Some(j), getMappings(d) ) }

  }

  private val LicenseFile = """(license|licence|notice|copying)([.]\w+)?$""".r
  private def isLicenseFile(fileName: String): Boolean =
    fileName.toLowerCase match {
      case LicenseFile(_, ext) if ext != ".class" => true // DISLIKE
      case _ => false
    }

  private val ReadMe = """(readme)([.]\w+)?$""".r
  private def isReadme(fileName: String): Boolean =
    fileName.toLowerCase match {
      case ReadMe(_, ext) if ext != ".class" => true
      case _ => false
    }

  object PathList {
    private val sysFileSep = System.getProperty("file.separator")
    def unapplySeq(path: String): Option[Seq[String]] = {
      val split = path.split(if (sysFileSep.equals( """\""")) """\\""" else sysFileSep)
      if (split.size == 0) None
      else Some(split.toList)
    }
  }

  val defaultMergeStrategy: String => MergeStrategy = { 
    case "reference.conf" | "rootdoc.txt" =>
      MergeStrategy.concat
    case PathList(ps @ _*) if isReadme(ps.last) || isLicenseFile(ps.last) =>
      MergeStrategy.rename
    case PathList("META-INF", xs @ _*) =>
      (xs map {_.toLowerCase}) match {
        case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
          MergeStrategy.discard
        case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
          MergeStrategy.discard
        case "plexus" :: xs =>
          MergeStrategy.discard
        case "services" :: xs =>
          MergeStrategy.filterDistinctLines
        case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
          MergeStrategy.filterDistinctLines
        case _ => MergeStrategy.deduplicate
      }
    case _ => MergeStrategy.deduplicate
  }

  lazy val baseAssemblySettings: Seq[sbt.Project.Setting[_]] = Seq(
    assembly <<= (test in assembly, outputPath in assembly, packageOptions in assembly,
        assembledMappings in assembly, mergeStrategy in assembly,
        assemblyDirectory in assembly, assemblyCacheOutput in assembly, cacheDirectory, assemblyCacheUnzip in assembly, streams) map {
      (test, out, po, am, ms, tempDir, co, cacheDir, acu, s) =>
        assemblyTask(out, po, am, ms, tempDir, co, cacheDir, acu, s.log) },
    
    assemblyCacheOutput in assembly := true,
    
    assemblyCacheUnzip in assembly  := true,

    assembledMappings in assembly <<= (assemblyOption in assembly, fullClasspath in assembly, dependencyClasspath in assembly,
        excludedJars in assembly, assemblyCacheUnzip in assembly, streams) map {
      (ao, cp, deps, ej, acu, s) => (tempDir: File) => assemblyAssembledMappings(tempDir, cp, deps, ao, ej, acu, s.log) },
      
    mergeStrategy in assembly := defaultMergeStrategy,

    packageScala <<= (outputPath in packageScala, packageOptions,
        assembledMappings in packageScala, mergeStrategy in assembly,
        assemblyDirectory in assembly, assemblyCacheOutput in assembly, cacheDirectory, assemblyCacheUnzip in assembly, streams) map {
      (out, po, am, ms, tempDir, co, cacheDir, acu, s) => assemblyTask(out, po, am, ms, tempDir, co, cacheDir, acu, s.log) },

    assembledMappings in packageScala <<= (assemblyOption in packageScala, fullClasspath in assembly, dependencyClasspath in assembly,
        excludedJars in assembly, assemblyCacheUnzip in assembly, streams) map {
      (ao, cp, deps, ej, acu, s) => (tempDir: File) =>
        assemblyAssembledMappings(tempDir, cp, deps, ao, ej, acu, s.log) },

    packageDependency <<= (outputPath in packageDependency, packageOptions in assembly,
        assembledMappings in packageDependency, mergeStrategy in assembly,
        assemblyDirectory in assembly, assemblyCacheOutput in assembly, cacheDirectory, assemblyCacheUnzip in assembly, streams) map {
      (out, po, am, ms, tempDir, co, cacheDir, acu, s) => assemblyTask(out, po, am, ms, tempDir, co, cacheDir, acu, s.log) },
    
    assembledMappings in packageDependency <<= (assemblyOption in packageDependency, fullClasspath in assembly, dependencyClasspath in assembly,
        excludedJars in assembly, assemblyCacheUnzip in assembly, streams) map {
      (ao, cp, deps, ej, acu, s) => (tempDir: File) =>
        assemblyAssembledMappings(tempDir, cp, deps, ao, ej, acu, s.log) },

    test <<= test or (test in Test),
    test in assembly <<= (test in Test),
    
    assemblyOption in assembly <<= (assembleArtifact in packageBin,
        assembleArtifact in packageScala, assembleArtifact in packageDependency, excludedFiles in assembly) {
      (includeBin, includeScala, includeDeps, exclude) =>   
      AssemblyOption(includeBin, includeScala, includeDeps, exclude) 
    },
    assemblyOption in packageDependency <<= (assemblyOption in assembly) { opt =>
      opt.copy(includeBin = false, includeScala = true, includeDependency = true)
    },
    assemblyOption in packageScala <<= (assemblyOption in assembly) { opt =>
      opt.copy(includeBin = false, includeScala = true, includeDependency = false)
    },
    
    packageOptions in assembly <<= (packageOptions in (Compile, packageBin), mainClass in assembly) map { (os, mainClass) =>
      mainClass map { s =>
        Package.MainClass(s) +: (os filterNot {_.isInstanceOf[Package.MainClass]})
      } getOrElse {os}
    },
    
    assemblyDirectory in assembly <<= cacheDirectory / "assembly",
    outputPath in assembly <<= (target in assembly, jarName in assembly) map { (t, s) => t / s },
    outputPath in packageScala <<= (target in assembly, jarName in packageScala) map { (t, s) => t / s },
    outputPath in packageDependency <<= (target in assembly, jarName in packageDependency) map { (t, s) => t / s },
    target in assembly <<= crossTarget,

    jarName in assembly <<= (jarName in assembly) or (defaultJarName in assembly),
    jarName in packageScala <<= (jarName in packageScala) or (defaultJarName in packageScala),
    jarName in packageDependency <<= (jarName in packageDependency) or (defaultJarName in packageDependency),

    defaultJarName in packageScala <<= (scalaVersion) map { (scalaVersion) => "scala-library-" + scalaVersion + "-assembly.jar" },
    defaultJarName in packageDependency <<= (name, version) map { (name, version) => name + "-assembly-" + version + "-deps.jar" },
    defaultJarName in assembly <<= (name, version) map { (name, version) => name + "-assembly-" + version + ".jar" },
    
    mainClass in assembly <<= mainClass or (mainClass in Runtime),
    
    fullClasspath in assembly <<= fullClasspath or (fullClasspath in Runtime),
    
    dependencyClasspath in assembly <<= dependencyClasspath or (dependencyClasspath in Runtime),
    
    excludedFiles in assembly := assemblyExcludedFiles _,
    excludedJars in assembly := Nil,
    assembleArtifact in packageBin := true,
    assembleArtifact in packageScala := true,
    assembleArtifact in packageDependency := true    
  )
  
  lazy val assemblySettings: Seq[sbt.Project.Setting[_]] = baseAssemblySettings
}

case class AssemblyOption(includeBin: Boolean,
  includeScala: Boolean,
  includeDependency: Boolean,
  exclude: Seq[File] => Seq[File])
