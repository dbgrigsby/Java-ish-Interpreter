class A {
  var a = 0;

  function X() {
     a = a + 1;
     return this;
  }

  function Y() {
     a = a + 1;
     return this;
  }

  static function main() {
    var obj = new A();
    obj.X().Y();

    return obj.a;
  }
}
