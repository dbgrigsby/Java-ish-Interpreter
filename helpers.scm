; Brett Johnson
; Adam Beck
; Daniel Grigsby
#lang racket
(provide (all-defined-out))

; these class parsings are for debugging purposes
;(define test-state (G-parsed-file-to-state->state '((class A () ((var x 100) (function add (x) ((return (+ (dot this x) x)))) (static-function main () ((var a (new A)) (return (funcall (dot a add) 25))))))
             ;(class B (extends A) ((var x 20) (function add (x) ((return (+ (dot this x) x)))) (static-var a 50)))) initstate))
;(define test-state2 (G-add-empty-scope-to-state->state test-state))

; this state is for debugging purposes
(define debug-state '((() ()) 
  ((B A)
   ((((var x 20) (function add (x) ((return (+ (dot this x) x)))) (static-var a 50)) (superclass A) ((a) (50)))
    (((var x 100) (function add (x) ((return (+ (dot this x) x)))) (static-function main () ((var a (new A)) (return (funcall (dot a add) 25)))))
     (superclass ())
     ((main) ((() ((var a (new A)) (return (funcall (dot a add) 25)))))))))))


; Logic of the scoped-state structure
(define initstate '( (() ()) ))
(define nullreturn '())
(define get-top-scope car)
(define get-tail-scope cdr)



; State logic of of a state in the scoped-state structure
(define get-state-from-pair cadr)
(define get-value-from-pair car)
(define get-variable-section-state car)
(define get-value-section-state cadr)
(define get-variable-section-head car)
(define get-variable-section-tail cdr)
(define get-scope-variable-head caar)
(define get-scope-variable-tail cdar)
(define get-scope-value-head caadr)
(define get-scope-value-tail cdadr)




; Structure of the program arguments section
; Gets the first argument of the list of arguments
(define program-head car)
(define program-tail cdr)
(define arglist-head car)
(define arglist-tail cdr)
(define get-arg1-from-expr cadr)
(define get-arg2-from-expr caddr)



; Individual argument section
; Returns the type of the upcoming statement in an arglist
; (e.g. (var x (+ 1 2)) yields 'var)
(define get-upcoming-statement-name car)



; Assign/Declare statement section
(define get-var-name-from-declare-args cadr)
(define truncate-var-name-from-declare caddr)
(define get-declare-from-assign cddr)



; Return statement section
(define rest-of-return-statement cdr)



; Begin statement section
(define rest-of-begin-statement cdr)



; If statement section
(define get-if-cond cadr)
(define get-if-then caddr)
(define get-args-after-if-else cadddr)
(define get-else-from-if-else cdddr)



; While statement seciton
(define get-while-cond cadr)
(define get-while-statement caddr)



; Try/catch/finally statement section
(define get-inner-catch-statement cdar)
(define get-inner-finally-statement cdar)
(define get-statements-from-try cadr)
(define get-catch-wrapper caddr)
(define get-contents-of-catch cddr)
(define get-finally-wrapper cadddr)
(define get-contents-of-finally cdddr)
(define get-contents-of-throw cadr)
(define get-exception-from-catch caar)
(define get-statements-from-catch cadr)

;functions section
(define get-function-name car)
(define get-function-actual-args cdr)
(define get-function-formal-args cadr)
(define get-function-body caddr)

(define get-funcall-args car)
(define get-funcall-body cadr)


; Class Helpers
(define class-layer-from-state->state (lambda (state) (car (reverse state))))

(define dotted-class-instance car)
(define dotted-class-call cadr)



; Class parsing helper section
(define get-staticscope-section caddr)
(define get-instance-section cadr)
(define get-classname-section cadar)
(define get-superclass-classname-section cadadr)
(define get-closure-section car)
(define get-rest-value-section cdr)
(define get-closure-name caar)
(define next-closure cdr)
(define get-closure-var-contents caddar)
(define get-closure-function-contents cddar)
(define get-closure-variable-contents cadar)
(define get-top-nestedstate-scope car)
(define get-top-valuesection car)
(define get-supercontents-name cadr)
(define empty-supercontents-name '())
(define get-class-closure-value caddr)
(define get-classname-from-contents car)
(define get-superclasscontents-from-contents cadr)
(define next-class cdr)
(define get-top-class car)
(define classname-parse 'class)
(define next-parsedfile cdr)
(define get-top-parsedfile car)
(define get-instance-state cadr)
(define atom?
  (lambda (v)
    (not (list? v))))
(define (debug-func? func . args)
  (cond
    ((func (car args))
     (error "debug equal" args))
    (else (cadr args))))
(define (debug-eq? . args)
  (cond
    ((equal? (car args) (cadr args))
     (error "debug equal" args))
    (else (cadr args))))
