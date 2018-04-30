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
  function Start() {
    return X().Y();
  }

  static function main() {
    var obj = new A();
    obj.Start();

    return obj.a;
  }
}
