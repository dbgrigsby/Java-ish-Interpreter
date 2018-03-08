; Brett Johnson
; Adam Beck
; Daniel Grigsby
#lang racket
(require "simpleParser.scm")
;(require "expression-ops.scm")
(require "state-manipulation.scm")
(provide (all-defined-out))

; This section reads code from a file, parses it to a list,
; interprets it and returns the return value or error
(define interpret
  (lambda (filename)
    (with-handlers ([exn:fail? error->handler])
      (output->formatter (evaluate-parse-tree->retval (parser filename) initstate)))))

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
