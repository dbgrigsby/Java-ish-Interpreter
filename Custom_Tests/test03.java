var x = 3;
var y = 6;
var z = 3;
var ret = 0;
var ret2 = 0;
if (x == y)
	ret = ret + 1;
else
	ret = ret - 1;

if (x == z)
	ret2 = ret2 - 1;
else
	ret2 = ret2 + 1;

return ret + ret2;
