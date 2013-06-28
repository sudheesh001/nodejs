#!/usr/bin/env node
var fs = require('fs');
var output="hello.txt";
var out="A startup is a business built to grow rapidly.\n";
fs.writeFileSync(output, out);
console.log("Script: "+ __filename + "\nWrote: "+out + "To: "+output);
