; Brett Johnson
; Adam Beck
; Daniel Grigsby
#lang racket
(provide (all-defined-out))

(require "expression-ops.scm")
(require "state-structs.scm")
(require "helpers.scm")
(require racket/base)
(require racket/trace)


; State-empty? checks if an empty state is encountered, or if the state contains no more elements (empty)
(define state-empty?
  (lambda (state)
    (cond
      ((or (null? state) (equal? state initstate)) #t)
      (else #f))))

; Interpretation loop section

; Main evaluation of parse tree function
(define evaluate-parse-tree->retval_state
  (lambda (program state)
    (evaluate-parse-tree-with-cfuncs->retval_state program state empty-cfuncs)))


(define evaluate-parse-tree-with-cfuncs->retval_state
  (lambda (program state cfuncsinstance)
    (call/cc
     (lambda (return)
       (cond
         ; not all programs/ segments must end in return
         ; empty list should return the state (ie: at the end of an if statement's statements)
         ((null? program) (error "No program"))
         ((not (list? program)) (error "Invalid program syntax"))
         (else (list '() (evaluate-statement-list->state program state
                                                         (cfuncs-update-return cfuncsinstance return)))))))))



; Evaluates a list of statements
(define evaluate-statement-list->state
  (lambda (program state cfuncsinstance)
    (cond
      ((null? program) state)
      ((not (list? program)) (error "Invalid program syntax"))
      ((pair? (program-head program))
       (evaluate-statement-list->state (program-tail program)
                                       (evaluate-statement->state (program-head program) state cfuncsinstance)
                                       cfuncsinstance))
      (else (error "Invalid statement list syntax")))))

; Returns state updated after evaluating pair
(define evaluate-statement->state
  (lambda (arglist state cfuncsinstance)
    (cond
      ((null? arglist) (error "Not a statement"))

      ((eq? 'return (get-upcoming-statement-name arglist))
       ((cfuncs-return cfuncsinstance)
        (G-eval-atomic-statement->value_state (rest-of-return-statement arglist) state cfuncsinstance)))

      ((eq? 'continue (get-upcoming-statement-name arglist))
       ((cfuncs-continue cfuncsinstance) state))

      ((eq? 'var (get-upcoming-statement-name arglist))
       (G-evaluate-var-declare-statement->state arglist state cfuncsinstance))

      ((eq? 'try (get-upcoming-statement-name arglist))
       (G-evaluate-try-statement->state arglist state cfuncsinstance))

      ((eq? 'throw (get-upcoming-statement-name arglist))
       ((cfuncs-catch cfuncsinstance) state (get-value-from-pair (G-value-lookup->value_state (get-contents-of-throw arglist) state cfuncsinstance))))

      ((eq? 'while (get-upcoming-statement-name arglist))
       (G-evaluate-while-statement->state arglist state cfuncsinstance))

      ((eq? 'if (get-upcoming-statement-name arglist))
       (G-evaluate-if-statement->state arglist state cfuncsinstance))

      ((eq? 'begin (get-upcoming-statement-name arglist))
       (G-remove-scope-from-state->state
        (evaluate-statement-list->state
         (rest-of-begin-statement arglist)
         (G-add-empty-scope-to-state->state state)
         cfuncsinstance)))

      ((eq? 'break (get-upcoming-statement-name arglist))
       ((cfuncs-break cfuncsinstance) (G-remove-scope-from-state->state state)))

      ((eq? 'function (get-upcoming-statement-name arglist))
       (G-define-function->state (arglist-tail arglist) state cfuncsinstance))

      (else (get-state-from-pair (G-eval-atomic-statement->value_state arglist state cfuncsinstance))))))

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

; Function definition section
(define G-define-function->state
  (lambda (arglist state cfuncsinstance)
    (declare-function (get-function-name arglist) (get-function-formal-args arglist) (get-function-body arglist) state)))


; evaluates a declare function
(define declare-function
  (lambda (function-name function-args function-body state)
    (cond
      ((G-declared-in-stack-frame? function-name state)
       (error "function already declared"))
      (else (initialize-var->state function-name
                                   (list function-args function-body)
                                   state)))))


; Evaluates a function
(define G-eval-function->value_state
  (lambda (name args state cfuncsinstance)
    (cond
      ((and (dot-expr? name)
            (eq? (dotted-class-instance (arglist-dot name)) 'this))
       (let* ([evaled-function (eval-function-post-name-eval (dotted-class-call (arglist-dot name))
                                                             args
                                                             state
                                                             state
                                                             (get-base-class state cfuncsinstance)
                                                             #t
                                                             cfuncsinstance)]
              [function-return (get-value-from-pair evaled-function)]
              [function-state (get-state-from-pair evaled-function)])
         (list function-return function-state)))
      ((and (dot-expr? name) (eq? (dotted-class-instance (arglist-dot name)) 'super))
       (eval-function-post-name-eval (dotted-class-call (arglist-dot name))
                                     args
                                     state
                                     state
                                     (get-current-class state cfuncsinstance)
                                     #f
                                     cfuncsinstance))

      ; single case: funcall (funcall (dot obj X))
      ; non-single case:     (funcall (dot (funcall (dot obj X)) Y))
      
      ; dotted-class-instance: 'a (the name of the instance
      ; dottedname: (a f) where f is the name of the call and a is the object
      ((and (dot-expr? name) (list? (dotted-class-instance (arglist-dot name))))
       (let* ([evaled-instance-pair (G-eval-atomic-statement->value_state (dotted-class-instance (arglist-dot name))
                                                                          state
                                                                          cfuncsinstance)]
              [evaled-instance (get-value-from-pair evaled-instance-pair)]
              [evaled-state (push-variable-as-literal->state (find-highest-var name) evaled-instance (get-state-from-pair evaled-instance-pair))]
              [dottedname (list (find-highest-var name) (dotted-class-call (arglist-dot name)))]
              [evaled-function (eval-function-post-name-eval (dotted-class-call dottedname)
                                                             args
                                                             evaled-state
                                                             (construct-dotted-state dottedname evaled-state)
                                                             (get-current-class (construct-dotted-state dottedname evaled-state) cfuncsinstance)
                                                             #f
                                                             cfuncsinstance)]
              [function-return (get-value-from-pair evaled-function)]
              [function-state (get-state-from-pair evaled-function)])
         (cond
           ((eq? (dotted-class-instance dottedname) '.temp) (list function-return (G-merge-states->state state function-state)))
           (else (list function-return
                       (update-class-instance (dotted-class-instance dottedname) (extract-new-class-instance-state function-state) evaled-state))))))
      ((dot-expr? name) 
       (let* ([dottedname (arglist-dot name)]
              [evaled-function (eval-function-post-name-eval (dotted-class-call dottedname)
                                                             args
                                                             state
                                                             (construct-dotted-state dottedname state)
                                                             (get-current-class (construct-dotted-state dottedname state) cfuncsinstance)
                                                             #t
                                                             cfuncsinstance)]
              [function-return (get-value-from-pair evaled-function)]
              [function-state (get-state-from-pair evaled-function)])
         (list function-return (update-class-instance (dotted-class-instance dottedname) (extract-new-class-instance-state function-state) state))))
      
      (else (eval-function-post-name-eval name args state state default-currentclass #t cfuncsinstance)))))


; Finds the highest rightmost variable in a chained dotted expression
(define find-highest-var
  (lambda (dotexpr)
    (cond
      ((atom? dotexpr) '.temp)
      ((list? (dotted-class-instance (arglist-dot dotexpr)))
       (cond
         ((eq? (get-dotted-head (dotted-class-instance (arglist-dot dotexpr))) 'new) '.temp)
         (else (find-highest-var (dotted-class-instance (arglist-dot dotexpr))))))
      ((eq? 'this (dotted-class-instance (arglist-dot dotexpr))) '.temp)
      ((eq? 'super (dotted-class-instance (arglist-dot dotexpr))) '.temp)
      ((eq? 'funcall (arglist-head dotexpr)) '.temp)
      (else (dotted-class-instance (arglist-dot dotexpr))))))

(define get-dotted-head car)

; Gets the case class a state is in
(define get-base-class
  (lambda (state cfuncsinstance)
    (get-value-from-pair (G-value-lookup->value_state '.class state cfuncsinstance))))

; Gets the current class a state is in
(define get-current-class
  (lambda (state cfuncsinstance)
    (cond
      ((G-initialized? '.this state) (get-value-from-pair (G-value-lookup->value_state '.this state cfuncsinstance)))
      (else (get-value-from-pair (G-value-lookup->value_state '.class state cfuncsinstance))))))

; Gets a superclass for a current class in a state
(define get-super-class
  (lambda (currentclass state)
    (cond 
      ((null? state) default-currentclass)
      ((null? currentclass) default-currentclass)
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) '.class)
       (cond
         ((and (eq? (get-value-section-top-head (get-value-section-state (get-top-scope state))) currentclass)
               (G-initialized? '.class (get-tail-scope state)))
          (get-value-from-pair (G-value-lookup->value_state '.class (get-tail-scope state) empty-cfuncs)))
         (else (get-super-class currentclass (get-tail-scope state)))))
      (else (get-super-class currentclass (get-tail-scope state))))))

(define get-value-section-top-head car)


(define default-currentclass '())

; Evaluate the actual function after the name as been evaluated
(define eval-function-post-name-eval
  (lambda (name args state function-state current-class preserve-current-class cfuncsinstance)
    (let* ([super-popped-state (G-add-empty-scope-to-state->state (G-push-stack-divider-to-state->state current-class
                                                                                                        (G-pop-scope-to-function->state name
                                                                                                                                        (if preserve-current-class
                                                                                                                                            current-class
                                                                                                                                            (get-super-class current-class state))
                                                                                                                                        function-state)))]
           [popped-state (push-variable-as-literal->state '.this
                                                          (if preserve-current-class
                                                              current-class
                                                              (get-super-class current-class state))
                                                          (G-add-empty-scope-to-state->state (G-push-stack-divider-to-state->state current-class
                                                                                                                                   (G-pop-scope-to-function-or-class->state name
                                                                                                                                                                            current-class
                                                                                                                                                                            function-state))))]
           [function-in-state (variable-value-lookup name super-popped-state)]
           [evaluate-function-call (evaluate-parse-tree-with-cfuncs->retval_state (get-funcall-body function-in-state)
                                                                                  (G-add-arguments-to-state->state (get-funcall-args function-in-state)
                                                                                                                   (evaluate-actual-args args state cfuncsinstance)
                                                                                                                   popped-state)
                                                                                  (cfuncs-wipe-all-but-catch (cfuncs-update-catch cfuncsinstance
                                                                                                                                  (lambda (s e)
                                                                                                                                    ((cfuncs-catch cfuncsinstance)
                                                                                                                                     (G-merge-states->state function-state
                                                                                                                                                            (G-pop-to-stack-divider->state s))
                                                                                                                                     e)))))])
      (list (get-value-from-pair evaluate-function-call)
            (G-merge-states->state function-state
                                   (G-pop-to-stack-divider->state (get-state-from-pair evaluate-function-call)))))))

;(trace eval-function-post-name-eval)
(define evaluate-actual-args-for-state
  (lambda (actual state cfuncsinstance)
    (cond
      ((null? actual) state)
      (else (evaluate-actual-args-for-state (rest-of-args actual)
                                            (get-state-from-pair (G-eval-atomic-statement->value_state (head-of-args actual)
                                                                                                       state
                                                                                                       cfuncsinstance))
                                            cfuncsinstance)))))

; Evaluates arguments
(define evaluate-actual-args
  (lambda (actual state cfuncsinstance)
    (cond
      ((null? actual) actual)
      (else
       (let* ([value-lookup (G-value-lookup->value_state (head-of-args actual) state cfuncsinstance)])
         (cons
          (get-value-from-pair value-lookup)
          (evaluate-actual-args (rest-of-args actual)
                                (get-state-from-pair value-lookup)
                                cfuncsinstance)))))))

(define head-of-args car)
(define rest-of-args cdr)

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

; Dot functions section

; Evaluates a dotted function
(define evaluate-dotted-function-name
  (lambda (arglist state)
    (dotted-class-call arglist)))

; constructs a dotted state
(define construct-dotted-state
  (lambda (dotexpr state)
    (add-class-instance-to-state (dotted-class-instance dotexpr) state)))

; Adds a class instance to a state
(define add-class-instance-to-state
 (lambda (instancename state)
   (append (G-get-instance-state instancename state)
           (G-pop-to-class-level->state state))))

; extract a new class instance
(define extract-new-class-instance-state
  (lambda (state)
    (reverse (extract-new-class-instance-state-sub (get-tail-scope (reverse state))))))

; extrats a new class instance from a state
(define extract-new-class-instance-state-sub
 (lambda (state)
   (cond
     ((null? state) empty-extraction-state)
     ((is-top-scope-class-divider? state) empty-extraction-state)
     (else (cons (get-top-scope state)
                 (extract-new-class-instance-state-sub (get-tail-scope state)))))))

(define empty-extraction-state '())

; Updates a class instance with a new instance state
(define update-class-instance
  (lambda (instancename new-instance-state state)
    (G-push-state->state instancename
                         (list (get-instance-value-front (get-value-from-pair (G-value-lookup->value_state instancename state empty-cfuncs)))
                               new-instance-state)
                         state)))

(define get-instance-value-front car)

; Pops a state to the class level
(define G-pop-to-class-level->state
  (lambda (state)
    (list (get-reversedstate-head (reverse state)))))

(define get-reversedstate-head car)


; Pushes a stack divider to a state
(define G-push-class-divider-to-state->state
  (lambda (classname state)
    (cons `((.cf) (,classname)) state)))


; Gets a valid state for a 'this' keyword call
(define get-valid-this-call-state
  (lambda (state)
    (cond
      ((G-state-has-stack-divider? state) (G-pop-to-stack-divider->state state))
      (else state))))
                                  
; Determines if the top scope in a state is the stack divider
(define is-top-scope-class-divider?
  (lambda (state)
    (cond
      ((null? (get-variable-section-head (get-top-scope state))) #f)
      (else (eq? (get-scope-variable-head (get-top-scope state)) '.cf)))))



; TODO add side effects


;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

; if statement section

; Returns the value yielded from an if statement and the updated state
(define G-evaluate-if-statement->state
  (lambda (arglist state cfuncsinstance)
    (let* ([eval-ifcond (G-eval-atomic-statement->value_state (get-if-cond arglist) state cfuncsinstance)])
      (cond
        ; If the if condition is true, evaluate the statements inside of it.
        ((get-value-from-pair eval-ifcond)
         (evaluate-statement-list->state (list (get-if-then arglist))
                                         (get-state-from-pair eval-ifcond)
                                         cfuncsinstance))
        ((has-else? arglist)
         (evaluate-statement-list->state (list (get-if-else arglist))
                                         (get-state-from-pair eval-ifcond)
                                         cfuncsinstance))
        (else (get-state-from-pair eval-ifcond))))))

; Checks if an arglist has an if-else branch
(define get-if-else
  (lambda (arglist)
    (cond
      ((not (has-else? arglist)) (error "no else statement"))
      (else (get-args-after-if-else arglist)))))

; Checks if an arglist has an else branch
(define has-else?
  (lambda (arglist)
    (pair? (get-else-from-if-else arglist))))

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

; try catch section
; Eval try statement
(define G-evaluate-try-statement->state
  (lambda (arglist state cfuncsinstance)
    (call/cc
     (lambda (throw)
       (evaluate-inner-try-statement arglist
                                     state
                                     (lambda (s)
                                       (throw (G-remove-scope-from-state->state (evaluate-statement-list->state (get-finally-from-try arglist)
                                                                                                                (G-add-empty-scope-to-state->state s)
                                                                                                                cfuncsinstance))))
                                     cfuncsinstance)))))

; Evaluate an inner try statement
(define evaluate-inner-try-statement
  (lambda (arglist state finally cfuncsinstance)
    (finally
     (G-remove-scope-from-state->state (evaluate-statement-list->state (get-statements-from-try arglist)
                                                                       (G-add-empty-scope-to-state->state state)
                                                                       (cfuncs-update-catch cfuncsinstance
                                                                                            (lambda (s e)
                                                                                              (finally
                                                                                               (G-remove-scope-from-state->state (evaluate-statement-list->state
                                                                                                                                  (get-statements-from-catch (get-catch-from-try arglist))
                                                                                                                                  (G-push-state->state (get-exception-from-catch (get-catch-from-try arglist))
                                                                                                                                                       e
                                                                                                                                                       (G-add-empty-scope-to-state->state (G-remove-scope-from-state->state s)))
                                                                                                                                  cfuncsinstance))))))))))

; Gets the catch statement from try
(define get-catch-from-try
  (lambda (arglist)
    (cond
      ((null? (get-catch-wrapper arglist)) '())
      (else (get-inner-catch-statement (get-contents-of-catch arglist))))))

; Gets the finally statement from try
(define get-finally-from-try
  (lambda (arglist)
    (cond
      ((null? (get-finally-wrapper arglist)) '())
      (else (get-inner-finally-statement (get-contents-of-finally arglist))))))



;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

; while loop section
; Returns the value yielded from a while statement and the updated state
(define G-evaluate-while-statement->state
  (lambda (arglist state cfuncsinstance)
    (call/cc
     (lambda (break)
       (evaluate-recursive-while arglist state (cfuncs-update-break cfuncsinstance break))))))


; Evaluate a while statement 
(define evaluate-recursive-while
  (lambda (arglist state cfuncsinstance)
    (call/cc
     (lambda (endcontinue)
       (let* ([eval-while-cond (G-eval-atomic-statement->value_state (get-while-cond arglist) state cfuncsinstance)])
         (cond
           ((get-value-from-pair eval-while-cond)
            (evaluate-recursive-while arglist
                                      (evaluate-statement-list->state (list (get-while-statement arglist))
                                                                      (get-state-from-pair eval-while-cond)
                                                                      (cfuncs-update-continue cfuncsinstance
                                                                                              (lambda (s)
                                                                                                (endcontinue (evaluate-recursive-while arglist
                                                                                                                                       s
                                                                                                                                       cfuncsinstance)))))

                                      cfuncsinstance))
           ; If the while condition is false, return '() for the return value, and also return the updated state after evaluating the condition (side effects challenge)
           (else (get-state-from-pair eval-while-cond))))))))





;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------



; Variable declaration section
; Returns updated state after a declaration or initialization
(define G-evaluate-var-declare-statement->state
  (lambda (arglist state cfuncsinstance)
    (cond
      ((null? (arglist-tail arglist)) (error "Nothing after the var"))
      ((G-declared-in-stack-frame? (get-var-name-from-declare-args arglist) state)
       (error "variable already declared" (get-var-name-from-declare-args arglist) state))
      ((only-declare? arglist) (declare-var->state (get-var-name-from-declare-args arglist) state))
      (else (let* ([evaluate-assign (G-eval-atomic-statement->value_state (truncate-var-name-from-declare arglist) state cfuncsinstance)])
              (initialize-var->state (get-var-name-from-declare-args arglist)
                                     (get-value-from-pair evaluate-assign)
                                     (get-state-from-pair evaluate-assign)))))))

; Adds an instance to the state. The value of the instance is '(type (instancestate))
; value is a list: e.g. (new A). Prereq: the name of the instance has not been declared in the current stack frame
(define get-instance-initialization-value
  (lambda (value state)
    (let* ([cn (get-instance-init-value-head value)])
      (cond
        (else (list (list 'classname cn)
                    (G-eval-class-closure->state cn state)))))))

(define get-instance-init-value-head cadr)


; Pushes the declaration statement to the state
(define declare-var->state
  (lambda (name state)
    (initialize-var->state name empty-value-for-declare state)))

(define empty-value-for-declare '())

; Pushes and initializes the variable to the state
(define initialize-var->state
  (lambda (name value state)
    (cons (append-head-scope-to-scope (list (list name)
                                            (list value))
                                      (get-top-scope state))
          (get-tail-scope state))))

; Determines if an argument is a declare (and not an assign).
(define only-declare?
  (lambda (arglist)
    (null? (get-declare-from-assign arglist))))



;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------



; atomic statement evaluator
; atomic statements are statements that are valid inside of a conditional statement/assignment statement or on their own
; at the moment, this is just assign statements and expressions
; returns a pair of (value, state)
; (e.g. (> x (+ y 1)) is an atomic statement)
; (e.g. (== 3 (= x (+ x 1)) is an atomic statement)
; Atomic statements can be put
(define G-eval-atomic-statement->value_state
  (lambda (arglist state cfuncsinstance)
    (cond
      ((single-atom? arglist) (G-value-lookup->value_state arglist state cfuncsinstance))
      ((single-value-list? arglist) (G-value-lookup->value_state (arglist-head arglist) state cfuncsinstance))
      ((dot-expr? arglist) (evaluate-dotted-expr->value_state (arglist-dot arglist) state cfuncsinstance))
      ((G-expr? arglist) (G-eval-expr->value_state arglist state cfuncsinstance))
      ((G-assign? arglist) (G-eval-assign->value_state arglist state cfuncsinstance))
      ((is-funcall? arglist) (eval-funcall->value_state (arglist-tail arglist) state cfuncsinstance))
      ((is-initialization? arglist) ; arglist = (new classname). Value should be '((classname name) state), state should be original state
       (list (get-instance-initialization-value arglist state) state))
      ((raw-class-instance arglist) (list arglist state))
      (else (error "not a valid atomic statement" arglist state)))))

(define raw-class-instance
  (lambda (arglist)
    (eq? (get-value-from-pair (get-value-from-pair arglist)) 'classname)))

; checks if an arglist is a dot-expression
(define dot-expr?
  (lambda (arglist)
    (cond
      ((not (list? arglist)) #f)
      ((not (eq? (length arglist) 3)) #f)
      ((eq? (arglist-head arglist) 'dot) #t)
      (else #f))))

(define arglist-dot
  (lambda (arglist)
    (arglist-tail arglist)))


; evaluates a dotted expression
(define evaluate-dotted-expr->value_state
  (lambda (arglist state cfuncsinstance)
    (cond
      ((eq? (dotted-class-instance arglist) 'this)
       (list (get-value-from-pair (G-value-lookup->value_state (dotted-class-call arglist)
                                                               (get-valid-this-call-state state)
                                                               cfuncsinstance))
             state))
      ((eq? (dotted-class-instance arglist) 'super)
       (list (get-value-from-pair (G-value-lookup->value_state (dotted-class-call arglist)
                                                               (get-tail-scope (get-valid-this-call-state state))
                                                               cfuncsinstance))
             state))
      (else
       (list (get-value-from-pair (G-value-lookup->value_state (dotted-class-call arglist)
                                                               (G-get-instance-state (dotted-class-instance arglist) state)
                                                               cfuncsinstance))
             state)))))

; FUNCALL Section

; eval function atomic statement section
(define is-funcall?
  (lambda (arglist)
    (eq? (arglist-head arglist) 'funcall)))

(define is-initialization?
  (lambda (arglist)
    (eq? (arglist-head arglist) 'new)))

; Evaluates the function call
(define eval-funcall->value_state
  (lambda (arglist state cfuncsinstance)
    (G-eval-function->value_state (get-function-name arglist) (get-function-actual-args arglist) state cfuncsinstance)))


; Determines if an argument is a single atom
(define single-atom?
  (lambda (arglist)
    (not (list? arglist))))

; Determines if an argument is of length 1
(define single-value-list?
  (lambda (arglist)
    (eq? (length arglist) 1)))

; Determines if an argument is an atomic statement
(define G-atomic-statement?
  (lambda (arglist state)
    (cond
      ((single-atom? arglist) #t)
      ((single-value-list? arglist) #t)
      ((G-expr? arglist) #t)
      ((G-assign? arglist) #t)
      (else #f))))





;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------



; eval-assign section
; this function evaluates assignment statements
; will returns value state pair
; (e.g. (= x 1) will return (1 (updated-state)))
(define G-eval-assign->value_state
  (lambda (arglist state cfuncsinstance)
    (let* ([evaluate-assign (G-eval-atomic-statement->value_state (get-arg2-from-expr arglist) state cfuncsinstance)])
      (cond
        ((not (G-assign? arglist)) (error "not an assignment"))
        ((dot-expr? (get-arg1-from-expr arglist))
         (evaluate-dotted-assign->value_state (arglist-dot (get-arg1-from-expr arglist))
                                              (get-value-from-pair evaluate-assign)
                                              (get-state-from-pair evaluate-assign)
                                              cfuncsinstance))
        ((G-declared? (get-arg1-from-expr arglist) state)
         (G-value-lookup->value_state (get-arg1-from-expr arglist)
                                      (G-push-state->state (get-arg1-from-expr arglist)
                                                           (get-value-from-pair evaluate-assign)
                                                           (get-state-from-pair evaluate-assign)) cfuncsinstance))
        (else (error "variable undeclared args:" arglist "state" state))))))

; Evaluates a dotted assignment expression
(define evaluate-dotted-assign->value_state
  (lambda (dot-expression assign-value state cfuncsinstance)
    (cond
      ((eq? (dotted-class-instance dot-expression) 'super)
       (let* ([evaled-assign (G-eval-assign->value_state `(= ,(dotted-class-call dot-expression) ,assign-value)
                                                         (get-tail-scope (get-valid-this-call-state state))
                                                         cfuncsinstance)]
              ; tail-scope is called to remove .cf pointer
              [evaled-state (get-state-from-pair evaled-assign)]
              [evaled-value (get-value-from-pair evaled-assign)])
         (list evaled-value (G-merge-states->state state evaled-state))))
      
      ((eq? (dotted-class-instance dot-expression) 'this)
       (let* ([evaled-assign (G-eval-assign->value_state `(= ,(dotted-class-call dot-expression) ,assign-value)
                                                         (get-valid-this-call-state state)
                                                         cfuncsinstance)]
              ; tail-scope is called to remove .cf pointer
              [evaled-state (get-state-from-pair evaled-assign)]
              [evaled-value (get-value-from-pair evaled-assign)])
         (list evaled-value (G-merge-states->state state evaled-state))))
      
      (else
       (update-class-instance (dotted-class-instance dot-expression)
                              (extract-new-class-instance-state (get-state-from-pair G-eval-assign->value_state
                                                                                     `(= ,(dotted-class-call dot-expression) ,assign-value)
                                                                                     (construct-dotted-state dot-expression state)
                                                                                     cfuncsinstance)))
       state))))

; Determines whether or not an assignment argument is reached
(define G-assign?
  (lambda (arglist)
    (cond
      ((eq? (get-op-from-expr arglist) '=) #t)
      (else #f))))





;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------



; eval-expression section
; this function evaluates all expressions
; expressions are defined as built in math, boolean, or comparison operators
; expressions cover the scope of math expressions and boolean expressions (or conditional statements)
; Returns (value, updated-state)
(define G-eval-expr->value_state
  (lambda (arglist state cfuncsinstance)
    (cond
      ((not (G-expr? arglist)) (error "given invalid expression operation"))
      ((eq? (length arglist) 2)
       (eval-expr-uni->value_state (get-op-from-expr arglist)
                                   (get-arg1-from-expr arglist)
                                   state
                                   cfuncsinstance))
      ((eq? (length arglist) 3)
       (eval-expr-multi->value_state (get-op-from-expr arglist)
                                     (get-arg1-from-expr arglist)
                                     (get-arg2-from-expr arglist)
                                     state
                                     cfuncsinstance))
      (else (error "invalid number of arguments")))))


; this function evaluates all 1 argument expressions
; it currently only evaluates ints and booleans
(define eval-expr-uni->value_state
  (lambda (op arg1 state cfuncsinstance)
    (cond
      ((math-expr? op) (eval-math-expr-uni->value_state op arg1 state cfuncsinstance))
      ((boolean-expr? op) (eval-boolean-expr-uni->value_state op arg1 state cfuncsinstance))
      (else (error "unsupported expression")))))

; this function evaluates booleans
; this function is for 1 argument boolean expressions
(define eval-boolean-expr-uni->value_state
  (lambda (op arg1 state cfuncsinstance)
    (cond
      ((eq? (G-type-lookup arg1 state cfuncsinstance) 'boolean)
       (cons ((boolean-operator-to-function-uni op)
              (get-value-from-pair (G-value-lookup->value_state arg1 state cfuncsinstance)))
             (list (get-state-from-pair (G-value-lookup->value_state arg1 state cfuncsinstance)))))
      (else (error "boolean operator not valid for non boolean types")))))

; this function evaluates math expressions
; it currently only supports integers
; this function is for 1 argument math expressions
(define eval-math-expr-uni->value_state
  (lambda (op arg1 state cfuncsinstance)
    (cond
      ((eq? (G-type-lookup arg1 state cfuncsinstance) 'integer)
       (eval-math-expr-int-uni->value_state op arg1 state cfuncsinstance))
      (else (error "invalid type for math expression")))))

; this function evaluates math expressions of integers
; this function is for 1 argument math expressions
(define eval-math-expr-int-uni->value_state
  (lambda (op arg1 state cfuncsinstance)
    (let* ([lookup-arg1 (G-value-lookup->value_state arg1 state cfuncsinstance)])
      (cons ((math-operator-to-function-uni op #t)
             (get-value-from-pair lookup-arg1))
            (list (get-state-from-pair lookup-arg1))))))

; this function evaluates all 2 argument expressions
; it currently only evaluates ints and booleans
(define eval-expr-multi->value_state
  (lambda (op arg1 arg2 state cfuncsinstance)
    (cond
      ((compare-expr? op) (eval-compare-expr-multi->value_state op arg1 arg2 state cfuncsinstance))
      ((math-expr? op) (eval-math-expr-multi->value_state op arg1 arg2 state cfuncsinstance))
      ((boolean-expr? op) (eval-boolean-expr-multi->value_state op arg1 arg2 state cfuncsinstance))
      (else (error "unsupported expression")))))

; this function evaluates comparisons
; this function is for 2 argument comparison expressions
(define eval-compare-expr-multi->value_state
  (lambda (op arg1 arg2 state cfuncsinstance)
    ; We return a (value, state), hence the cons for the value and the state
    ; The value is derived from applying the operator on arg1 and arg2
    ; To handle side effects, the state passed into arg2 is the state after evaluating arg1
    (let* ([lookup-arg1 (G-value-lookup->value_state arg1 state cfuncsinstance)]
           [lookup-arg2 (G-value-lookup->value_state arg2 (get-state-from-pair lookup-arg1) cfuncsinstance)])
      (cons ((compare-operator-to-function-multi op) (get-value-from-pair lookup-arg1)
                                                     (get-value-from-pair lookup-arg2))
            (list (get-state-from-pair lookup-arg2))))))

; this function evaluates booleans
; this function is for 2 argument boolean expressions
; Returns (value, updated->state)
(define eval-boolean-expr-multi->value_state
  (lambda (op arg1 arg2 state cfuncsinstance)
    (let* ([lookup-arg1 (G-value-lookup->value_state arg1 state cfuncsinstance)]
           [lookup-arg2 (G-value-lookup->value_state arg2 (get-state-from-pair lookup-arg1) cfuncsinstance)])
      (cond
        ; We return a (value, state), hence the cons for the value and the state
        ; The value is derived from applying the operator on arg1 and arg2
        ; To handle side effects, the state passed into arg2 is the state after evaluating arg1
        ((and (eq? (G-type-lookup arg1 state cfuncsinstance) 'boolean)
              (eq? (G-type-lookup arg2 state cfuncsinstance) 'boolean))
         (cons ((boolean-operator-to-function-multi op)
                (get-value-from-pair lookup-arg1)
                (get-value-from-pair lookup-arg2))
               (list (get-state-from-pair lookup-arg2))))
        (else (error "not valid types for boolean expression"))))))


; this function evaluates math expressions
; it currently only supports integers
; this function is for 2 argument math expressions
(define eval-math-expr-multi->value_state
  (lambda (op arg1 arg2 state cfuncsinstance)
    (cond
      ((and (eq? (G-type-lookup arg1 state cfuncsinstance) 'integer)
            (eq? (G-type-lookup arg2 state cfuncsinstance) 'integer))
       (eval-math-expr-int-multi->value_state op arg1 arg2 state cfuncsinstance))
      (else (error "invalid types for math expression")))))

; this function evaluates math expressions of integers
; this function is for 2 argument math expressions
; returns updated state
(define eval-math-expr-int-multi->value_state
  (lambda (op arg1 arg2 state cfuncsinstance)
    ; We return a (value, state), hence the cons for the value and the state
    ; The value is derived from applying the operator on arg1 and arg2
    ; To handle side effects, the state passed into arg2 is the state after evaluating arg1
    (let* ([lookup-arg1 (G-value-lookup->value_state arg1 state cfuncsinstance)]
           [lookup-arg2 (G-value-lookup->value_state arg2 (get-state-from-pair lookup-arg1) cfuncsinstance)])
      (cons ((math-operator-to-function-multi op #t)
             (get-value-from-pair lookup-arg1)
             (get-value-from-pair lookup-arg2))
            (list (get-state-from-pair lookup-arg2))))))





;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------



; state-interface section
; this function takes values (integers, strings, variables, expressions, ...) and returns their actual value
; for now it only handles int and bolean literals and expressions of the two
(define G-value-lookup->value_state
  (lambda (value state cfuncsinstance)
    (cond
      ; if its an expression, evaluate to get value
      ((list? value) (G-eval-atomic-statement->value_state value state cfuncsinstance))
      ((this-expr? value) (handle-this-expr state))
      ((super-expr? value) (error "not implemented"))
      ((integer? value) (cons value (list state)))
      ((boolean? value) (cons value (list state)))
      ((java-boolean? value) (cons (lookup-java-boolean value) (list state)))
      ((G-initialized? value state) (cons (variable-value-lookup value state) (list state)))
      (else (error "unsupported value lookup" value "state" state)))))

; check if an arglist is a super-expression
(define super-expr?
  (lambda (arglist)
    (equal? arglist 'super)))

; check if an arglist is a this-expression
(define this-expr?
  (lambda (arglist)
    (equal? arglist 'this)))

; evaluate a this-expression
(define handle-this-expr
  (lambda (state)
    (list (list `(classname ,(get-base-class state empty-cfuncs))
                (G-pop-to-stack-divider->state (extract-new-class-instance-state state)))
          state)))

; Determines whether a boolean in java boolean notation was encountered
(define java-boolean?
  (lambda (value)
    (cond
      ((eq? value 'true) #t)
      ((eq? value 'false) #t)
      (else #f))))

; Converts java to scheme boolean
(define lookup-java-boolean
  (lambda (value)
    (cond
      ((eq? value 'true) #t)
      ((eq? value 'false) #f)
      (else (error "not a java boolean")))))

; tests whether variable is declared
; this function will need to check inputs and error
; as it will recieve bogus inputs
(define G-declared?
  (lambda (variable-name state)
    (cond
      ((state-empty? state) #f)
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) variable-name) #t)
      (else (G-declared? variable-name (get-tail-scope state))))))

; Determines if a variable was declared in a scope
(define G-declared-in-stack-frame?
  (lambda (variable-name state)
    (cond
      ((state-empty? state) #f)
      ((is-top-scope-stack-divider? state) #f)
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) variable-name) #t)
      (else (G-declared-in-stack-frame? variable-name (get-tail-scope state))))))

; tests whether a variable is declared in a given scope, which is a state in the list of states we have.
(define declared-in-scope?
  (lambda (lis variable)
    (cond
      ((null? lis) #f)
      ((eq? (get-variable-section-head lis) variable) #t)
      (else (declared-in-scope? (get-variable-section-tail lis) variable)))))

; tests whether variable is declared and initialized
(define G-initialized?
  (lambda (variable-name state)
    (cond
      ((not (G-declared? variable-name state)) #f)
      ((null? (variable-value-lookup variable-name state)) #f)
      (else #t))))





;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------




; Adding/removing state section and scope access functions
; This section helps other sections affect the state with scoping rules
(define G-add-empty-scope-to-state->state
  (lambda (state)
    (cons (get-top-scope initstate) state)))

; removes the top scope from a state
(define G-remove-scope-from-state->state
  (lambda (state)
    (cond
      ((state-empty? state) (error "The main scope can't be removed!"))
      ((null? (get-tail-scope state)) initstate)
      (else (get-tail-scope state)))))

; Gets the head of a scope (e.g. ((a) (b)) is the head of ((a b c) (b d e)))
(define get-head-scope
  (lambda (state)
    (list (list (get-scope-variable-head state))
          (list (get-scope-value-head state)))))

; Gets the tail of a state (e.g. state is ((a b c) (1 2 3)) tail is ((b c) (2 3)))
(define get-tail-state
  (lambda (state)
    (list (get-scope-variable-tail state)
          (get-scope-value-tail state))))






;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------


; Pushing state section
; This section will handle the logic of pushing to a state, looking up, and updating values from variables
; adds variable and its value to state or over->writes it
; If the value is a variable, the value of this variable is found and pushed to the state
; (e.g. if we are pushing (x y) and y = 3, we push (x 3) to the state
; returns the new state
; precondition: value not a variable
(define G-push-state->state
  (lambda (variable value state)
    (cond
      ; If the state is empty, push it to it
      ((state-empty? state) (list (list (list variable) (list value))))
      ; If the value is a number, null, or boolean, push to the state
      ((or (number? value) (null? value) (boolean? value) (list? value))
       (push-variable-as-literal->state variable value state))
      ; If the value is not a number, push the value of this variable to the state
      (else (error "Value is a variable, expected to be value" variable value state)))))

; Pushes a variable and a number (not a variable as the value) to the state, or updates the state if the variable is there
; Returns the updated state
(define push-variable-as-literal->state
  (lambda (variable number state)
    (cond
      ; If the state is empty, push to the state
      ((state-empty? state)
       (list (list (list variable)
                   (list number))))
      ; If it's been declared before, update the variable, if not, add it to the state
      ((G-declared? variable state) (update-variable variable number state))
      (else (cons (list (cons variable (get-variable-section-state (get-top-scope state)))
                        (cons number (get-value-section-state (get-top-scope state))))
                  (G-remove-scope-from-state->state state))))))

; Updatea a variable in a state, assumes variable is in the state
(define update-variable
  (lambda (variable number state)
    (cond
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) variable)
       (cons (update-variable-in-scope variable number (get-top-scope state))
             (get-tail-scope state)))
      (else (cons (get-top-scope state) (update-variable variable number (get-tail-scope state)))))))

; Updates a variable in a scope, assumes variable is in the scope
(define update-variable-in-scope
  (lambda (variable number state)
    (cond
      ((eq? (get-scope-variable-head state) variable)
       (list (cons variable (get-scope-variable-tail state))
             (cons number (get-scope-value-tail state))))
      (else (append-head-scope-to-scope (get-head-scope state)
                                        (update-variable-in-scope variable number (get-tail-state state)))))))

; appends a head scope to scope
; (e.g. ((a) (1)) appended to ((b c d) (2 3 4))
; yields ((a b c d) (1 2 3 4)))
(define append-head-scope-to-scope
  (lambda (head-state tail-state)
    (list (append (list (get-scope-variable-head head-state))
                  (get-variable-section-state tail-state))
          (append (list (get-scope-value-head head-state))
                  (get-value-section-state tail-state)))))

; looks up the value of a variable in the state
; returns the value of the variable or an error if the variable was not found
(define variable-value-lookup
  (lambda (variable state)
    (cond
      ((state-empty? state) (error "State is empty"))
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) variable)
       (lookup-variable-value-in-scope variable (get-top-scope state)))
      (else (variable-value-lookup variable (get-tail-scope state))))))

; Looks up the value of a variable in a scope
(define lookup-variable-value-in-scope
  (lambda (variable state)
    (cond
      ((eq? (get-scope-variable-head state) variable) (get-scope-value-head state))
      (else (lookup-variable-value-in-scope variable (get-tail-state state))))))

; this function takes values (integers, strings, variables, ...) and returns their type
; for now it only handles any atomic statement
(define G-type-lookup
  (lambda (value state cfuncsinstance)
    (cond
      ((list? value)
       (G-type-lookup (get-value-from-pair (G-value-lookup->value_state value state cfuncsinstance))
                      state cfuncsinstance))
      ((integer? value) 'integer)
      ((boolean? value) 'boolean)
      ((java-boolean? value) 'boolean)
      ((G-initialized? value state) (variable-type-lookup value state cfuncsinstance))
      (else (error "uninitialized variable or unsupported type" value state)))))

; looks up the type of a variable in the state
(define variable-type-lookup
  (lambda (variable state cfuncsinstance)
    (G-type-lookup (variable-value-lookup variable state) state cfuncsinstance)))

; Pops scopes off of the state until the head scope contains a function
;pops to the scope after the currentclass
(define G-pop-scope-to-function-or-class->state
  (lambda (fn desiredclass state)
    (cond
      ((null? state) (error "function was not found in state"))
      ((null? desiredclass) (pop-scope-to-function-default fn state))
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) '.class)
       (cond
         ((eq? (get-valuesection-value (get-value-section-state (get-top-scope state))) desiredclass)
          state)
         (else (G-pop-scope-to-function-or-class->state fn desiredclass (get-tail-scope state)))))
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) fn) state)
      (else (G-pop-scope-to-function-or-class->state fn desiredclass (get-tail-scope state))))))

(define get-valuesection-value car)

(define G-pop-scope-to-function->state
  (lambda (fn desiredclass state)
    (cond
      ((null? state) (error "function was not found in state"))
      ((null? desiredclass) (pop-scope-to-function-default fn state))
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) '.class)
       (cond
         ((eq? (get-value-popped-head (get-value-section-state (get-top-scope state))) desiredclass)
          (pop-scope-to-function-default fn state))
         (else (G-pop-scope-to-function->state fn desiredclass (get-tail-scope state)))))
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) fn) state)
      (else (G-pop-scope-to-function->state fn desiredclass (get-tail-scope state))))))

(define get-value-popped-head car)

; Pops a scope to a givenfunction
(define pop-scope-to-function-default
  (lambda (fn state)
    (cond
      ((null? state) (error "function was not found in state"))
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) fn) state)
      (else (pop-scope-to-function-default fn (get-tail-scope state))))))


; shortmerges an origin state to a modified state
(define G-shortmerge-states->state
  (lambda (origin-state mod-state)
    (reverse (shortmerge (reverse origin-state)
                         (reverse mod-state)))))

(define shortmerge
  (lambda (orig-state mod-state)
    (cond
      ((null? orig-state) empty-orig-state)
      (else (cons (get-top-scope mod-state)
                  (shortmerge (get-tail-scope orig-state)
                              (get-tail-scope mod-state)))))))

; Merges an original state with the state after a funciton call,
; assumes that the function call state has smaller airty than the original state
(define G-merge-states->state
  (lambda (origin-state mod-state)
    (reverse (merge (reverse origin-state)
                    (reverse mod-state)))))

; merges two reversed states
(define merge
  (lambda (orig-state mod-state)
    (cond
      ((null? mod-state) orig-state)
      ((null? orig-state) empty-orig-state)
      (else (cons (get-top-scope mod-state)
                  (merge (get-tail-scope orig-state)
                         (get-tail-scope mod-state)))))))

(define empty-orig-state '())

; Add arguments to state
(define G-add-arguments-to-state->state
  (lambda (arg-namelist value-list state)
    (cond
      ((null? arg-namelist) state)
      ((not (eq? (length arg-namelist) (length value-list))) (error "Arity mis-match between values and argument names"))
      (else (cons (concatenate-scopes (list arg-namelist value-list) (get-initstate-value initstate))
                  state)))))

(define get-initstate-value car)

; Merges two scopes into a single scope (e.g. ((a b) (1 2)) and ((c d) (3 4)) yeilds ((a b c d) (1 2 3 4)))
(define concatenate-scopes
  (lambda (head-state tail-state)
    (let* ([varsec-head (get-variable-section-state head-state)]
           [varsec-tail (get-variable-section-state tail-state)]
           [valsec-head (get-value-section-state head-state)]
           [valsec-tail (get-value-section-state tail-state)])
      (list(append varsec-head varsec-tail)
           (append valsec-head valsec-tail)))))

; Pushes a stack divider to a state
(define G-push-stack-divider-to-state->state
  (lambda (name state)
    (cond
      ((not (list? state)) (error "state is void"))
      (else (cons `((.sf) (,name)) state)))))

; Determines if a state has a stack divider in it
(define G-state-has-stack-divider?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((is-top-scope-stack-divider? state) #t)
      (else (G-state-has-stack-divider? (get-tail-scope state))))))

; Pops scopes off the stack until a stack dividor scope is found
(define G-pop-to-stack-divider->state
  (lambda (state)
    (cond
      ((null? state) (error "No stack divider found"))
      ((is-top-scope-stack-divider? state) (get-tail-scope state))
      (else (G-pop-to-stack-divider->state (get-tail-scope state))))))

; Determines if the top scope in a state is the stack divider
(define is-top-scope-stack-divider?
  (lambda (state)
    (cond
      ((null? (get-variable-section-head (get-top-scope state))) #f)
      (else (eq? (get-scope-variable-head (get-top-scope state)) '.sf)))))

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

; Evaluates a class closure 
(define G-eval-class-closure->state
  (lambda (classname state)
    (cond
      ;push-variable as literal code is wrong, it produces an extra scope
      ;looks wrong, don't touch
      ((null? (G-get-class-superclass classname state)) (list (get-top-scope (evaluate-closure->state classname state))))
      (else (cons (get-top-scope
                   (evaluate-closure->state classname state)) (G-eval-class-closure->state (G-get-class-superclass classname state) state))))))

; Evaluates a closure of a class (given a classname) and returns the state after evaluation
(define evaluate-closure->state
  (lambda (classname state)
    (push-variable-as-literal->state '.class
                                     classname
                                     (evaluate-closure-statement-list->state (G-get-class-closure classname state)
                                                                             initstate
                                                                             empty-cfuncs))))

; Evaluates a list of statements in a closure
(define evaluate-closure-statement-list->state
  (lambda (program state cfuncsinstance)
    (cond
      ((null? program) state)
      ((not (list? program)) (error "Invalid program syntax"))
      ((pair? (program-head program))
       (evaluate-closure-statement-list->state (program-tail program)
                                               (evaluate-closure-statement->state (program-head program) state cfuncsinstance)
                                               cfuncsinstance))
      (else (error "Invalid statement list syntax")))))

; Delegates the appropriate evaluation function for a closure statement
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

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

; Converts a parsed file into our state, and adds a new scope to the front once conversion is complete
(define G-parsed-file-to-state->state
  (lambda (parsedFile state)
    (G-add-empty-scope-to-state->state (parse-file-to-state parsedFile state))))

; Converts a parsed file into a state
(define parse-file-to-state
  (lambda (parsedFile state)
    (cond
      ((null? parsedFile) state)
      (else (parse-file-to-state (next-parsedfile parsedFile)
                                 (G-add-class-to-state->state (get-top-parsedfile parsedFile) state))))))

; Adds a (class, closure) to the state, as well as its contents
; The contents are: (classname, name), (super, classname), (staticField, value), (staticFunction, value)
(define G-add-class-to-state->state
  (lambda (class state)
    (cond
      ((null? class) (error "Class is empty"))
      ((eq? (get-top-class class) classname-parse) (G-add-class-contents-to-state->state (next-class class) state))
      (else state))))

; Adds a class's contents to the state. This first adds the classname, any exending classes, then calls a helper to add contetns
; contents = '(A (extends B) ((var x 5)))
(define G-add-class-contents-to-state->state
  (lambda (contents state)
    (cond
      ((null? contents) state)
      (else (add-statics-to-state->state (get-class-closure-value contents)
                                         (push-superclass-to-state->state (get-superclasscontents-from-contents contents)
                                                                          (push-classname-to-state->state (get-classname-from-contents contents)
                                                                                                          (get-class-closure-value contents)
                                                                                                          state)))))))

; Pushes a (classname, name) to the value section of the most recent class in the top scope of the state
; closure = '((var x 5) (var b 3))
(define push-classname-to-state->state
  (lambda (classname closure state)
    (cond
      ((null? classname) (error "No classname"))
      (else (list (push-classname-to-scope->scope classname closure (get-top-scope state)))))))

; Helper for push-classname-to-state-state, returns a classname pushed to the first scope's class
(define push-classname-to-scope->scope
  (lambda (name closure scope)
    (merge-scope-sections (add-name-to-scope name scope)
                          (add-closure-to-scope closure scope))))

; Merges a variable and value section of a scope together into a scope
(define merge-scope-sections
  (lambda (variables values)
    (list variables values)))

; Adds a name (classname) to a scope
(define add-name-to-scope
  (lambda (name scope)
    (cons name (get-variable-section-state scope))))

; Adds a closure to a scope
(define add-closure-to-scope
  (lambda (closure scope)
    (cons (list closure) (get-value-section-state scope))))

; Pushes a superclass to a state
(define push-superclass-to-state->state
  (lambda (supercontents state)
    (list (push-superclass-to-scope->scope supercontents (get-top-scope state)))))

; Adds a superclass to a scope when given supercontents, which contain a classname or null for no superclass
(define push-superclass-to-scope->scope
  (lambda (supercontents scope)
    (cond
      ((null? supercontents) (add-superclass-to-scope empty-supercontents-name scope))
      (else (add-superclass-to-scope (get-supercontents-name supercontents) scope)))))

; Adds and creates a superclass list: '(superclass classname) to a staticscope
(define add-superclass-to-scope
  (lambda (superclassname scope)
    (merge-scope-sections (get-variable-section-state scope)
                          (append (list (reverse (cons (list 'superclass superclassname)
                                                       (reverse (get-top-valuesection (get-value-section-state scope))))))
                                  (get-rest-value-section (get-value-section-state scope))))))

; Adds static fields and methods to our state
; e.g. '((class A (extends B) ((static-var x 5))))
; closure = ((static-var x 5))
; For each element in the closure, push it to our state, then pop the top scope to get a scope
(define add-statics-to-state->state
  (lambda (closure state)
    (list (add-statics-to-scope->scope closure (get-top-nestedstate-scope state) initstate))))

; For each element in the closure, push it to a state, then take the top scope, and append it to the classcope
; classcope is '((B A) ((contents1) (contents2)))
(define add-statics-to-scope->scope
  (lambda (closure classcope nestedstate)
    (cond
      ((null? closure); merge nestedstate as an element to our class contents
       (merge-scope-sections (get-variable-section-state classcope)
                             (append (list (reverse (cons (get-top-nestedstate-scope nestedstate)
                                                          (reverse (get-top-valuesection (get-value-section-state classcope))))))
                                     (get-rest-value-section (get-value-section-state classcope)))))
      ((eq? (get-closure-name closure) 'static-var)
       (add-statics-to-scope->scope (next-closure closure) classcope (G-push-state->state (get-closure-variable-contents closure)
                                                                                          (get-closure-var-contents closure)
                                                                                          nestedstate)))
      ((eq? (get-closure-name closure) 'static-function)
       (add-statics-to-scope->scope (next-closure closure) classcope (G-push-state->state (get-closure-variable-contents closure)
                                                                                          (get-closure-function-contents closure)
                                                                                          nestedstate)))
      (else (add-statics-to-scope->scope (next-closure closure) classcope nestedstate)))))

; Helper functions for easy access/lookup to our state for class operations
; LOOKUP SECTION ----------------------------------------------------------
; Gets a class's staticscope (the scope with static fields and functions)
(define G-get-class-closure
  (lambda (classname state)
    (get-closure-section (get-value-from-pair (G-value-lookup->value_state classname state empty-cfuncs)))))

; Get the superclass classname of a class
(define G-get-class-superclass
  (lambda (classname state)
    (get-superclass-classname-section (get-value-from-pair (G-value-lookup->value_state classname state empty-cfuncs)))))

; Get the classname of an instance
(define G-get-instance-classname
  (lambda (instancename state)
    (get-classname-section get-classname-section (get-value-from-pair (G-value-lookup->value_state instancename state empty-cfuncs)))))

; Get an instancestate of an instance
(define G-get-instance-state
  (lambda (instancename state)
    (get-instance-section (get-value-from-pair (G-value-lookup->value_state instancename state empty-cfuncs)))))

; Get a class's staticscope given a classname
(define G-get-class-staticscope->staticscope
  (lambda (classname state)
    (get-staticscope-section (get-value-from-pair (G-value-lookup->value_state classname state empty-cfuncs)))))