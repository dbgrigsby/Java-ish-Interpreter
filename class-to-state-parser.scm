; Brett Johnson
; Adam Beck
; Daniel Grigsby
; This module parses a project-4 file and adds its contents to our state
#lang racket
(provide (all-defined-out))
(require "state-manipulation.scm")
(require "helpers.scm")

; A basic file:
(define testclasses '((class A (extends B) ())))
(define firstclass (car testclasses))


; A simple file would be:
; '((class A (extends B) ()))
; Parses a parsed file into our state
(define G-parsed-file-to-state->state
  (lambda (parsedFile state)
    (cond
      ((null? parsedFile) state)
      ((G-parsed-file-to-state->state (cdr parsedFile) (G-add-class-to-state->state (car parsedFile) state))))))

; adds a (class, closure) to the state, as well as its contents
; The contents are: (classname, name), (super, classname), (staticField, value), (staticFunction, value)
(define G-add-class-to-state->state
  (lambda (class state)
    (cond
      ((null? class) (error "Class is empty"))
      ((eq? (car class) 'class) (G-add-class-contents-to-state->state (cdr class) initstate))
      (else state))))

; Adds a class's contents to the state. This first adds the classname, any exending classes, then calls a helper to add contetns
(define G-add-class-contents-to-state->state
  (lambda (contents state)
    (cond
      ((null? contents) state)
      (else (push-superclass-to-state->state (cadr contents)
                                      (push-classname-to-state->state (car contents) (cddr contents) state))))))

; Pushes a (classname, name) to the value section of the most recent class in the top scope of the state
(define push-classname-to-state->state
  (lambda (classname closure state)
    (cond
      ((null? classname) (error "No classname"))
      (else (cons (push-classname-to-scope->scope classname closure (get-top-scope state)) '())))))

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
    (cons (push-superclass-to-scope->scope supercontents (get-top-scope state)) '())))

(define push-superclass-to-scope->scope
  (lambda (supercontents scope)
    (cond
      ((null? supercontents) (add-superclass-to-scope '() scope))
      (else (add-superclass-to-scope (cadr supercontents) scope)))))

(define add-superclass-to-scope
  (lambda (superclassname scope)
    (merge-scope-sections (get-variable-section-state scope)
                          (append (list(reverse (cons (append '(superclass) (cons superclassname '()))
                                         (reverse (car (get-value-section-state scope))))))
                                  (cdr (get-variable-section-state scope))))))
