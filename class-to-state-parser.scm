; Brett Johnson
; Adam Beck
; Daniel Grigsby
; This module parses a project-4 file and adds its contents to our state
#lang racket
(provide (all-defined-out))
(require "state-manipulation.scm")
(require "helpers.scm")

; WORKING TEST CASES ---------------------------------------------
; A simple file would be:
(define sf '((class A (extends B) ((var x 5))))) ; testing purposes
; An advanced file would be:
(define af '((class A (extends B) ((static-var x 5) (var y 10) (function foo (a) ((return a))) (static-function main () ()))))) ; testing purposes
; A file with multiple classes would be:
(define mf '((class A (extends B) ((static-var x 5) (var y 10) (function foo (a) ((return a))) (static-function main () ())))
             (class B () ((static-var a 6) (var z 11) (function bar (b) ((return b))) (static-function main () ()))))) ; testing purposes
; A complicated multiple class parse would be:
(define cf '((class A () ((var x 100) (function add (x) ((return (+ (dot this x) x)))) (static-function main () ((var a (new A)) (return (funcall (dot a add) 25))))))
             (class B (extends A) ((static-var a 50)))) ) ; testing purposes

; Parses a parsed file into our state (which initially is our initstate)
(define G-parsed-file-to-state->state
  (lambda (parsedFile state)
    (cond
      ((null? parsedFile) state)
      (else  (G-parsed-file-to-state->state (cdr parsedFile)
                                            (G-add-class-to-state->state (car parsedFile) state))))))

; adds a (class, closure) to the state, as well as its contents
; The contents are: (classname, name), (super, classname), (staticField, value), (staticFunction, value)
(define G-add-class-to-state->state
  (lambda (class state)
    (cond
      ((null? class) (error "Class is empty"))
      ((eq? (car class) 'class) (G-add-class-contents-to-state->state (cdr class) state))
      (else state))))

; Adds a class's contents to the state. This first adds the classname, any exending classes, then calls a helper to add contetns
; contents = '(A (extends B) ((var x 5)))
(define G-add-class-contents-to-state->state
  (lambda (contents state)
    (cond
      ((null? contents) state)
      (else (add-statics-to-state->state (caddr contents)
                                         (push-superclass-to-state->state (cadr contents)
                                                                          (push-classname-to-state->state (car contents) (caddr contents) state)))))))

; Pushes a (classname, name) to the value section of the most recent class in the top scope of the state
; closure = '((var x 5) (var b 3))
(define push-classname-to-state->state
  (lambda (classname closure state)
    (cond
      ((null? classname) (error "No classname"))
      (else (list (push-classname-to-scope->scope classname closure (get-top-scope state)))))))

; helper for push-classname-to-state-state, returns a classname pushed to the first scope's class
(define push-classname-to-scope->scope
  (lambda (name closure scope)
    (merge-scope-sections (add-name-to-scope name scope)
                          (add-closure-to-scope closure scope))))

(define merge-scope-sections
  (lambda (variables values)
    (list variables values)))

(define add-name-to-scope
  (lambda (name scope)
    (cons name (get-variable-section-state scope))))

(define add-closure-to-scope
  (lambda (closure scope)
    (cons (list closure) (get-value-section-state scope))))

(define push-superclass-to-state->state
  (lambda (supercontents state)
    (list (push-superclass-to-scope->scope supercontents (get-top-scope state)))))

(define push-superclass-to-scope->scope
  (lambda (supercontents scope)
    (cond
      ((null? supercontents) (add-superclass-to-scope '() scope))
      (else (add-superclass-to-scope (cadr supercontents) scope)))))

(define add-superclass-to-scope
  (lambda (superclassname scope)
    (merge-scope-sections (get-variable-section-state scope)
                          (append (list (reverse (cons (list 'superclass superclassname)
                                                       (reverse (car (get-value-section-state scope))))))
                                  (cdr (get-value-section-state scope))))))

; Adds static fields and methods to our state
; e.g. '((class A (extends B) ((static-var x 5))))
; closure = ((static-var x 5))
; For each element in the closure, push it to our state, then pop the top scope to get a scope
(define add-statics-to-state->state
  (lambda (closure state)
    (list (add-statics-to-scope->scope closure (car state) initstate))))

; For each element in the closure, push it to a state, then take the top scope, and append it to the classcope
; classcope is '((B A) ((contents1) (contents2)))
(define add-statics-to-scope->scope
  (lambda (closure classcope nestedstate)
    (cond
      ((null? closure); merge nestedstate as an element to our class contents
       (merge-scope-sections (get-variable-section-state classcope)
                             (append (list (reverse (cons (car nestedstate)
                                                          (reverse (car (get-value-section-state classcope))))))
                                     (cdr (get-value-section-state classcope)))))
      ((eq? (caar closure) 'static-var)
       (add-statics-to-scope->scope (cdr closure) classcope (G-push-state->state (cadar closure) (caddar closure) nestedstate)))
      ((eq? (caar closure) 'static-function)
       (add-statics-to-scope->scope (cdr closure) classcope (G-push-state->state (cadar closure) (cddar closure) nestedstate)))
      (else (add-statics-to-scope->scope (cdr closure) classcope nestedstate)))))

; Helper functions for easy access/lookup to our state for class operations
; LOOKUP SECTION ----------------------------------------------------------
(define G-get-staticstate-from-state->staticstate
  (lambda (classname state)
    (cddar (variable-value-lookup classname state))))
      
 
; Helper functions for easy update to our state for class operations
; UPDATE SECTION ---------------------------------------------------