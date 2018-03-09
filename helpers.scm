; Brett Johnson
; Adam Beck
; Daniel Grigsby
#lang racket
(provide (all-defined-out))


(define initstate '( (() ()) ))
(define nullreturn '())
(define get-top-scope car)
(define get-tail-scope cdr)


; Important section helper functions for abstraction are defined below
(define program-head car)
(define program-tail cdr)


; Returns the type of the upcoming statement in an arglist
; (e.g. (var x (+ 1 2)) yields 'var)
(define get-upcoming-statement-name car)


; Important section helper functions for abstraction are defined below
(define rest-of-return-statement cdr)
(define rest-of-begin-statement cdr)




; Important section helper functions for abstraction are defined below
(define get-if-cond cadr)
(define get-if-then caddr)


(define get-declare-from-assign cddr)




(define inner-argument caar)



(define get-inner-catch-statement cdar)
(define get-inner-finally-statement cdar)
(define get-statements-from-try cadr)

(define get-catch-wrapper caddr)

(define get-contents-of-catch cddr)

(define get-finally-wrapper cadddr)

(define get-contents-of-finally cdddr)

(define get-contents-of-throw cadr)



; Important section helper functions for abstraction are defined below
(define get-while-cond cadr)
(define get-while-statement caddr)


; Important section helper functions for abstraction are defined below
(define get-var-name-from-declare-args cadr)
(define truncate-var-name-from-declare cddr)

(define get-args-after-if-else cadddr)
(define get-else-from-if-else cdddr)





; Important section helper functions for abstraction are defined below
(define arglist-head car)
(define arglist-tail cdr)


(define get-arg1-from-expr cadr)
(define get-arg2-from-expr caddr)
(define get-state-from-pair cadr)
(define get-value-from-pair car)

(define get-variable-section-head car)
(define get-variable-section-tail cdr)

(define get-exception-from-catch caar)
(define get-statements-from-catch cadr)

; The state is stored as a list of two lists
; (e.g. the head of the values for '((a b c) (1 2 3)) is 1, by calling caadr)
; (e.g. the tail of the variables for '((a b c) (1 2 3)) is (b c), by calling cdar)
(define get-state-variable-head caar)
(define get-state-variable-tail cdar)
(define get-state-value-head caadr)
(define get-state-value-tail cdadr)
(define get-variable-section-state car)
(define get-value-section-state cadr)
