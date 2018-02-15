(load "interpreter.scm")

; General testing framework
(define test
  (lambda (filename expected-output)
    (if (eq? (interpret filename) expected-output)
        (string-append "Passed " filename)
        (string-append "Failed " filename ". Output was " (interpret filename) ", but should have been " expected-output))))

; Tests 1
(test "Tests/test1.java" 150) ;A has main
(test "Tests/test2.java" -4) ;A has main
(test "Tests/test3.java" 10) ;A has main
(test "Tests/test4.java" 16) ;A has main
(test "Tests/test5.java" 220) ;A has main
(test "Tests/test6.java" 5) ;A has main
(test "Tests/test7.java" 6) ;C has main
(test "Tests/test8.java" 10) ;square has main
(test "Tests/test9.java" 9) ;square has main
(test "Tests/test10.java" -39) ;List has main
(test "Tests/test11.java" `error) ;List has main
(test "Tests/test8.java" 10) ;square has main
(test "Tests/test9.java" 9) ;square has main
(test "Tests/test10.java" -39) ;List has main
(test "Tests/test11.java" `error) ;List has main
(test "Tests/test12.java" `error) ;List has main
(test "Tests/test13.java" `error) ;List has main
(test "Tests/test14.java" `error) ;List has main
(test "Tests/test15.java" `true) ;A has main
(test "Tests/test16.java" 100) ;A has main
(test "Tests/test17.java" `false`) ;A has main
(test "Tests/test18.java" true) ;A has main
(test "Tests/test19.java" 128) ;A has main
(test "Tests/test20.java" 12) ;A has main
(test "Tests/test21.java" 30) ;C has main
(test "Tests/test22.java" 11) ;square has main
(test "Tests/test23.java" 1106) ;square has main
(test "Tests/test24.java" 12) ;List has main
(test "Tests/test25.java" 16) ;List has main
(test "Tests/test26.java" 72) ;square has main
(test "Tests/test27.java" 21) ;square has main
(test "Tests/test28.java" 164) ;List has main
