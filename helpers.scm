; Brett Johnson
; Adam Beck
; Daniel Grigsby
#lang racket
(provide (all-defined-out))

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
(define truncate-var-name-from-declare cddr)
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
(define get-function-actual-args cadr)
(define get-function-formal-args cadr)
(define get-function-body caddr)