class A {

function divide(x, y) {
  if (y == 0)
    throw y;
  return x / y;
}

static function main() {
  var x;

  try {
    x = divide(10, 5) * 10;
    x = x + divide(5, 0);
  }
  catch(e) {
    x = e;
  }
  finally {
    x = x + 100;
  }
  return x;
}

}
