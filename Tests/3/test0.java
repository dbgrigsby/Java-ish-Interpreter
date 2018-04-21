class A {

function divide(x, y) {
  if (y == 0)
    throw 1000000;
  return x / y;
}

function main() {
  var x = 0;
  try {
   if(true){
   var y = 15;
     if(true){
       if(true){if(true)if(true)if(true)if(true)throw 10;}
     }
   }
  }
  catch (e){}
  finally{
    x = x + 1;
  }
  return x;
}

}
