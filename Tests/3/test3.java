class A {

var x = 1;
var y = 10;
var r = 0;

static function main() {
  while (x < y) {
     r = r + x;
     x = x + 1;
  }
  return r;
}

}
