#lang racket

(provide (all-defined-out))
(require "class-to-state-parser.scm")
(require "helpers.scm")
(require "state-structs.scm")
(require "state-manipulation.scm")

(define G-eval-class-closure->state
  (lambda (classname state)
    (cond
      ((null? (G-get-class-superclass classname state)) (evaluate-closure->state classname state))
      (else (cons (get-top-scope (evaluate-closure->state classname state)) (G-eval-class-closure->state (G-get-class-superclass classname state) state))))))

(define evaluate-closure->state
  (lambda (classname state)
    (evaluate-closure-statement-list->state (G-get-class-closure classname state) initstate empty-cfuncs)))
                                                 
(define evaluate-closure-statement-list->state
  (lambda (program state cfuncsinstance)
    (cond
      ((null? program) state)
      ((not (list? program)) (error "Invalid program syntax"))
      ((pair? (program-head program))
       (evaluate-closure-statement-list->state
        (program-tail program)
        (evaluate-closure-statement->state (program-head program) state cfuncsinstance)
        cfuncsinstance))
      (else (error "Invalid statement list syntax")))))

(define evaluate-closure-statement->state
  (lambda (arglist state cfuncsinstance)
    (cond
      ((null? arglist) (error "Not a statement"))

      ((eq? 'var (get-upcoming-statement-name arglist))
       (G-evaluate-var-declare-statement->state arglist state cfuncsinstance))

      ((eq? 'static-var (get-upcoming-statement-name arglist)) state)

      ((eq? 'function (get-upcoming-statement-name arglist))
       (G-define-function->state (arglist-tail arglist) state cfuncsinstance))
      
      ((eq? 'static-function (get-upcoming-statement-name arglist)) state)

      (else (error "Not a valid statement" arglist)))))