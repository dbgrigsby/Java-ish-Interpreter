class A {

  var x = 100;

  function add(x) {
    return this.x + x;
  }

  static function main() {
    var a = new A();
    return a.add(25);
  }
}

class B extends A {

  static var a = 50;

}