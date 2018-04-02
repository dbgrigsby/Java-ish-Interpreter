function divide(x, y) {
  if (y == 0)
    throw 1000000;
  return x / y;
}

function main() {
  var y = 0;
  if(true) {
	  var x = 100;
	  while (x >= 0) {
		  x = x - 1;
		  try {
		  	divide(10,0);
		  }
		  catch (e2) {
		  	y = y + e2;
		  }
	  }
  }
  var x = 10;
  return x + y;
}
