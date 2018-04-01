function test(a) {
  return test2(a) + 1;
}
function test2(b) {
 return b + 1;
}
function main() {
  var x = test(3);
  return test(x);
}

