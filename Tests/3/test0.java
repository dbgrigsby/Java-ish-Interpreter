function divide(y) {
  throw y;
}

function main() {
  var x = 5;

  try {
    x = divide(5);
  }
  catch(e) {
    x = e;
  }
  finally {
    x = x + 100;
  }
  return x;
}
