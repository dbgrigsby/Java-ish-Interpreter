; Brett Johnson
; Adam Beck
; Daniel Grigsby
#lang racket
(require "classParser.scm")
;(require "expression-ops.scm")
(require "state-manipulation.scm")
(require "helpers.scm")
(require "state-structs.scm")
(provide (all-defined-out))

; This section reads code from a file, parses it to a list,
; interprets it and returns the return value or error
(define interpret
  (lambda (filename classname)
    (let* ([staticstate (G-parsed-file-to-state->state (parser filename) initstate)])
      (with-handlers ([exn:fail? error->handler])
        (show-parse-tree-output (evaluate-parse-tree->retval_state (get-main-code classname staticstate)
                                                                   staticstate))))))
; display the value from the parse tree output
(define show-parse-tree-output
  (lambda (parse-tree-output)
    (output->formatter (get-value-from-pair parse-tree-output))))

; gets the code for a main method of a class
(define get-main-code
  (lambda (classname staticstate)
    (get-code-from-function-closure (get-main-value classname staticstate))))

; gets the value of main (arglist + code)
(define get-main-value
  (lambda (classname staticstate)
    (get-value-from-pair (G-value-lookup->value_state 'main
                                                      (list (G-get-class-staticscope->staticscope classname staticstate))
                                                      empty-cfuncs))))

; gets the code portion of a function closure (arglist + code)
(define get-code-from-function-closure cadr)

    

(define append-main
  (lambda (program)
    (append program '((return (funcall main))))))

; From (value state) ->> value
; If value is #t or #f, parses to correct string literal
(define output->formatter
  (lambda (toFormat)
    (cond
      ((eq? toFormat #t) `true)
      ((eq? toFormat #f) `false)
      (else toFormat))))

; Error handler, returns `error if error bubbles up
; All errors, despite their text, return 'error for test purposes 
(define error->handler
  (lambda (exception) ; It is correct to not delete the lambda to abstract this out. [exn:fail? error->handler] relies on this format.
    'error))
