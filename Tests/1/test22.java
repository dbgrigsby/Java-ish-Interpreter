class A {

static function main() {
var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;
}

}
