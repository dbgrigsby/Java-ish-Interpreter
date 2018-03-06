; Brett Johnson
; Adam Beck
; Daniel Grigsby

#lang racket
(provide (all-defined-out))

(define G-expr?
  (lambda (arglist)
    (cond
      ((math-expr? (get-op-from-expr arglist)) #t)
      ((boolean-expr? (get-op-from-expr arglist)) #t)
      ((compare-expr? (get-op-from-expr arglist)) #t)
      (else #f))))
      
(define get-op-from-expr car)

(define math-expr?
  (lambda (op)
    (cond
      ((eq? op '+) #t)
      ((eq? op '-) #t)
      ((eq? op '*) #t)
      ((eq? op '/) #t)
      ((eq? op '%) #t)
      (else #f))))

(define boolean-expr?
  (lambda (op)
    (cond
      ((eq? op '&&) #t)
      ((eq? op '||) #t)
      ((eq? op '!) #t)
      (else #f))))

(define compare-expr?
  (lambda (op)
    (cond
      ((eq? op '==) #t)
      ((eq? op '!=) #t)
      ((eq? op '<) #t)
      ((eq? op '>) #t)
      ((eq? op '<=) #t)
      ((eq? op '>=) #t)
      (else #f))))



; Determines whether a boolean in java boolean notation was encountered
(define java-boolean?
  (lambda (value)
    (cond
      ((eq? value 'true) #t)
      ((eq? value 'false) #t)
      (else #f))))


(define java-bool-to-scheme-bool
  (lambda (value)
    (cond
      ((eq? value 'true) #t)
      ((eq? value 'false) #f)
      (else (error "not a java boolean")))))

; this function takes a boolean operator (ie: !) and returns the actual function
; this translation is for 1 argument boolean expressions
(define boolean-operator-to-function-uni
  (lambda (op)
    (cond
      ((eq? op '!) (lambda (arg1) (not arg1)))
      (else (error "unsupported unary boolean expression")))))

; this function takes a math operator ie: -> along with a boolean is-int that specifies whether the values are integers
; this translation is for 1 argument math expressions
(define math-operator-to-function-uni
  (lambda (op is-int)
    (cond
      ((eq? op '-) -)
      (else (error "invalid unary math operator")))))

; this function takes a math operator ie: +, ->, * ... along with a boolean is-int that specifies whether the values are integers
; this translation is for 2 argument math expressions
(define math-operator-to-function-multi
  (lambda (op is-int)
    (cond
      ((eq? op '+) +)
      ((eq? op '*) *)
      ((eq? op '-) -)
      ((and (eq? op '/) is-int) quotient)
      ((eq? op '/) /)
      ((and (eq? op '%) is-int) modulo)
      ((eq? op '%) (error "modulo % only works on integers"))
      (else (error "invalid math operator")))))

; this function takes a boolean operator (ie: &&, ||, ...) and returns the actual function
; this translation is for 2 argument boolean expressions
(define boolean-operator-to-function-multi
  (lambda (op)
    (cond
      ((eq? op '&&) (lambda (arg1 arg2)
                      (and arg1 arg2)))
      ((eq? op '||) (lambda (arg1 arg2)
                      (or arg1 arg2)))
      (else (error "unsupported boolean expression")))))

(define compare-operator-to-function-multi
  (lambda (op)
    (cond
      ((eq? op '==) equal?)
      ((eq? op '!=) (lambda (arg1 arg2) (not (equal? arg1 arg2))))
      ((eq? op '<) <)
      ((eq? op '>) >)
      ((eq? op '<=) <=)
      ((eq? op '>=) >=)
      (else (error "invalid comparison operator")))))
