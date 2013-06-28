#!/usr/bin/env node
var fs = require('fs');
var output = "prime.txt";
var count=0;
var firsthundredprime=function(k)
{
	var i=2;
	var arr = [];
	while(count<=k)
	{
		var x=2;var flag=0;
		while(x<i && flag==0)
		{
			if(i%x==0)
			{
				flag=1;
			}
			x++;
		}
		if(flag==0)
		{
			count++;
			arr.push(i);
		}
		i++;
	}
	return arr;
}
var fmt = function(arr) {
fs.writeFileSync(output, arr);
    return arr.join(" ");
};
var k=100;
console.log(fmt(firsthundredprime(k)));
