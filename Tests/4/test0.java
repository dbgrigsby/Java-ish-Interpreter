class A {

  static var x = 100;
  var y = 3;
  function add(x) {
    y = 120;
    return A.x + x;
  }

  static function main() {
    var a = new A();
    a.add(3);
    A.x = 3;
    var z = 10;
  }
}

class B extends A {

  static var a = 50;

}

class C extends A {

  static var a = 50;

}


inside a: ‘((Add y) ((code) 3))

Give a.func scope (a’s scope + static classes and shit)
At the end take updated a’s scope and set it to inside a
(((z)(10)) ((.instance Add y) (a (code) 3)) ((A B) (stuff stuff)))
                ^ slice this part out when done

construct a.add’s state with a’s scope and the scope of the classes

In the the constructed state, a’s scope should have a .instance value set to the variable name which happens to be a

So when we return the state from a.add, we easily pluck out the scope with the .instance variable, and then we can have some replace scope instance function which replaces a’s original scope with the updated scope.

‘((main x) ((code) 100))