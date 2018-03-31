; Brett Johnson
; Adam Beck
; Daniel Grigsby
#lang racket
(provide (all-defined-out))

(require "expression-ops.scm")
(require "state-structs.scm")
(require "helpers.scm")



(define state-empty?
  (lambda (state)
    (cond
      ((or (null? state) (equal? state initstate)) #t)
      (else #f))))

; Interpretation loop section

; Main evaluation of parse tree function
(define evaluate-parse-tree->retval
  (lambda (program state)
    (call/cc
     (lambda (return)
       (cond
         ; not all programs/ segments must end in return
         ; empty list should return the state (ie: at the end of an if statement's statements)
         ((null? program) '())
         ((not (list? program)) (error "Invalid program syntax"))

         (else (evaluate-statement-list->state program state
                                               (cfuncs return identity identity identity))))))))

(define evaluate-statement-list->state
  (lambda (program state cfuncsinstance)
    (cond
      ((null? program) state)
      ((not (list? program)) (error "Invalid program syntax"))
      ((pair? (program-head program))
       (evaluate-statement-list->state
        (program-tail program)
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
        (get-value-from-pair (G-eval-atomic-statement->value_state (rest-of-return-statement arglist) state))))

      ((eq? 'continue (get-upcoming-statement-name arglist))
       ((cfuncs-continue cfuncsinstance) state))

      ((eq? 'var (get-upcoming-statement-name arglist))
       (G-evaluate-var-declare-statement->state arglist state))

      ((eq? 'try (get-upcoming-statement-name arglist))
       (G-evaluate-try-statement->state arglist state cfuncsinstance))

      ((eq? 'throw (get-upcoming-statement-name arglist))
       ((cfuncs-catch cfuncsinstance) (G-remove-scope-from-state->state state) (get-contents-of-throw arglist)))

      ((eq? 'while (get-upcoming-statement-name arglist))
       (G-evaluate-while-statement->state arglist state cfuncsinstance))

      ((eq? 'if (get-upcoming-statement-name arglist))
       (G-evaluate-if-statement->state arglist state cfuncsinstance))

      ((eq? 'begin (get-upcoming-statement-name arglist))
       (G-remove-scope-from-state->state
        (evaluate-statement-list->state
         (rest-of-begin-statement arglist)
         (G-add-scope-to-state->state state)
         cfuncsinstance)))

      ((eq? 'break (get-upcoming-statement-name arglist))
       ((cfuncs-break cfuncsinstance) (G-remove-scope-from-state->state state)))

      ((eq? 'function (get-upcoming-statement-name arglist))
        (G-define-function->state arglist state cfuncsinstance))

      (else (get-state-from-pair (G-eval-atomic-statement->value_state arglist state))))))



; Function definition section

(define G-define-function->state 
  (lambda (arglist state cfuncsinstance)
    (G-push-state->state (get-function-name arglist) (list (get-function-formal-args arglist) (get-function-body arglist)))))

(define function-descriptor caar)
(define function-name cadar)
(define function-params caddar)
(define function-body (lambda (arglist) (car (cdddar arglist))))

(define extract-main-method->method-body
  (lambda (arglist)
    (cond 
      ((null? arglist) (error "no main method found"))
      ((is-main-method? (function-descriptor arglist) (function-name arglist) (function-params arglist))
        (function-body arglist))
      (else (error "no main method found")))))

(define is-main-method?
  (lambda (stmt1 stmt2 stmt3)
    ((and (eq? stmt1 'function) (eq? stmt2 'main) (eq? stmt3 `())))))


; if statement section

; Returns the value yielded from an if statement and the updated state
(define G-evaluate-if-statement->state
  (lambda (arglist state cfuncsinstance)
    (cond
      ; If the if condition is true, evaluate the statements inside of it.
      ((get-value-from-pair (G-eval-atomic-statement->value_state (get-if-cond arglist) state))

       ; The state for evaluating the if statement's statements is the state after evaluating the if statement's condition (side effects challenge)
       (evaluate-statement-list->state
        (list (get-if-then arglist))
        (get-state-from-pair (G-eval-atomic-statement->value_state (get-if-cond arglist) state))
        cfuncsinstance))

      ((has-else? arglist)
       (evaluate-statement-list->state
        (list (get-if-else arglist))
        (get-state-from-pair (G-eval-atomic-statement->value_state (get-if-cond arglist) state))
        cfuncsinstance))

      ; If the if condition is false, return '() for the return value, and also return the updated state after evaluating the condition (side effects challenge)
      (else (get-state-from-pair (G-eval-atomic-statement->value_state (get-if-cond arglist) state))))))

(define get-if-else
  (lambda (arglist)
    (cond
      ((not (has-else? arglist)) (error "no else statement"))
      (else (get-args-after-if-else arglist)))))

(define has-else?
  (lambda (arglist)
    (pair? (get-else-from-if-else arglist))))


; try catch section
(define G-evaluate-try-statement->state
  (lambda (arglist state cfuncsinstance)
    (call/cc
     (lambda (throw)
       (evaluate-inner-try-statement
        arglist
        state
        (lambda (s) (throw
                     (G-remove-scope-from-state->state
                      (evaluate-statement-list->state
                       (get-finally-from-try arglist)
                       (G-add-scope-to-state->state s)
                       cfuncsinstance))))
        cfuncsinstance)))))

(define evaluate-inner-try-statement
  (lambda (arglist state finally cfuncsinstance)
    (finally
     (G-remove-scope-from-state->state
      (evaluate-statement-list->state
       (get-statements-from-try arglist)
       (G-add-scope-to-state->state state)
       (cfuncs-update-catch
        cfuncsinstance
        (lambda (s e)
          (finally
           (G-remove-scope-from-state->state
            (evaluate-statement-list->state
             (get-statements-from-catch (get-catch-from-try arglist))
             (G-push-state->state
              (get-exception-from-catch (get-catch-from-try arglist))
              e
              (G-add-scope-to-state->state s))
             cfuncsinstance))))))))))







(define get-catch-from-try
  (lambda (arglist)
    (cond
      ((null? (get-catch-wrapper arglist)) '())
      (else (get-inner-catch-statement (get-contents-of-catch arglist))))))

(define get-finally-from-try
  (lambda (arglist)
    (cond
      ((null? (get-finally-wrapper arglist)) '())
      (else (get-inner-finally-statement (get-contents-of-finally arglist))))))




; while loop section

; Returns the value yielded from a while statement and the updated state
(define G-evaluate-while-statement->state
  (lambda (arglist state cfuncsinstance)
    (call/cc
     (lambda (break)
       (evaluate-recursive-while arglist state (cfuncs-update-break cfuncsinstance break))))))

(define evaluate-recursive-while
  (lambda (arglist state cfuncsinstance)
    (call/cc
     (lambda (endcontinue)
       (cond
         ; If the while condition is true, evaluate the statements inside of it.
         ((get-value-from-pair (G-eval-atomic-statement->value_state (get-while-cond arglist) state))
          (evaluate-recursive-while
           arglist
           ; The state for evaluating the while statement's statements is the state after evaluating the while statement's condition (side effects challenge)
           (evaluate-statement-list->state
            (list (get-while-statement arglist))
            (get-state-from-pair (G-eval-atomic-statement->value_state (get-while-cond arglist) state))

            ; s is a passed in state
            (cfuncs-update-continue
             cfuncsinstance
             (lambda (s) (endcontinue (evaluate-recursive-while arglist s cfuncsinstance)))))

           cfuncsinstance))

         ; If the while condition is false, return '() for the return value, and also return the updated state after evaluating the condition (side effects challenge)
         (else (get-state-from-pair (G-eval-atomic-statement->value_state (get-while-cond arglist) state))))))))









; Variable declaration section
; Returns updated state after a declaration or initialization
(define G-evaluate-var-declare-statement->state
  (lambda (arglist state)
    (cond
      ((null? (arglist-tail arglist)) (error "Nothing after the var"))
      ((G-declared? (get-var-name-from-declare-args arglist) state)
       (error "variable already declared"))
      ((only-declare? arglist)
       (declare-var->state (get-var-name-from-declare-args arglist)
                          state))
      (else (initialize-var->state (get-var-name-from-declare-args arglist)
                                  (get-value-from-pair (G-eval-atomic-statement->value_state (truncate-var-name-from-declare arglist)
                                                                                            state))
                                  (get-state-from-pair (G-eval-atomic-statement->value_state (truncate-var-name-from-declare arglist)
                                                                                            state)))))))

(define declare-var->state
  (lambda (name state)
    (G-push-state->state name `() state)))

; Pushes the initializes the variable to the state
(define initialize-var->state
  (lambda (name value state)
    (G-push-state->state name value state)))

(define only-declare?
  (lambda (arglist)
    (null? (get-declare-from-assign arglist))))







; atomic statement evaluator
; atomic statements are statements that are valid inside of a conditional statement/assignment statement or on their own
; at the moment, this is just assign statements and expressions
; returns a pair of (value, state)
; (e.g. (> x (+ y 1)) is an atomic statement)
; (e.g. (== 3 (= x (+ x 1)) is an atomic statement)
; Atomic statements can be put
(define G-eval-atomic-statement->value_state
  (lambda (arglist state)
    (cond
      ((single-atom? arglist) (G-value-lookup->value_state arglist state))
      ((single-value-list? arglist) (G-value-lookup->value_state (arglist-head arglist) state))
      ((G-expr? arglist) (G-eval-expr->value_state arglist state))
      ((G-assign? arglist) (G-eval-assign->value_state arglist state))
      ((is-funcall? arglist) (eval-funcall->value_state (arglist-tail arglist) state))
      (else (error "not a valid atomic statement")))))


(define single-atom?
  (lambda (arglist)
    (not (list? arglist))))

(define single-value-list?
  (lambda (arglist)
    (eq? (length arglist) 1)))

(define G-atomic-statement?
  (lambda (arglist state)
    (cond
      ((single-atom? arglist) #t)
      ((single-value-list? arglist) #t)
      ((G-expr? arglist) #t)
      ((G-assign? arglist) #t)
      (else #f))))


; eval function atomic statement section
(define is-funcall?
  (lambda (arglist)
    (eq? (arglist-head arglist) 'funcall)))

(define eval-funcall->value_state
  (lambda (arglist state)
    (G-eval-function->value_state (get-function-name arglist) (get-function-actual-args arglist))))








; eval-assign section
; this function evaluates assignment statements
; will returns value state pair
; (e.g. (= x 1) will return (1 (updated-state)))
(define G-eval-assign->value_state
  (lambda (arglist state)
    (cond
      ((not (G-assign? arglist)) (error "not an assignment"))
      ((G-declared? (get-arg1-from-expr arglist) state)
       (G-value-lookup->value_state (get-arg1-from-expr arglist)
                                   (G-push-state->state (get-arg1-from-expr arglist)
                                                       (get-value-from-pair (G-eval-atomic-statement->value_state (get-arg2-from-expr arglist)
                                                                                                                 state))
                                                       (get-state-from-pair (G-eval-atomic-statement->value_state(get-arg2-from-expr arglist)
                                                                                                                state)))))
      (else (error "variable undeclared")))))

(define G-assign?
  (lambda (arglist)
    (cond
      ((eq? (get-op-from-expr arglist) '=) #t)
      (else #f))))









; eval-expression section
; this function evaluates all expressions
; expressions are defined as built in math, boolean, or comparison operators
; expressions cover the scope of math expressions and boolean expressions (or conditional statements)
; Returns (value, updated-state)
(define G-eval-expr->value_state
  (lambda (arglist state)
    (cond
      ((not (G-expr? arglist)) (error "given invalid expression operation"))
      ((eq? (length arglist) 2)
       (eval-expr-uni->value_state (get-op-from-expr arglist)
                                  (get-arg1-from-expr arglist)
                                  state))
      ((eq? (length arglist) 3)
       (eval-expr-multi->value_state (get-op-from-expr arglist)
                                    (get-arg1-from-expr arglist)
                                    (get-arg2-from-expr arglist)
                                    state))
      (else (error "invalid number of arguments")))))


; this function evaluates all 1 argument expressions
; it currently only evaluates ints and booleans
(define eval-expr-uni->value_state
  (lambda (op arg1 state)
    (cond
      ((math-expr? op) (eval-math-expr-uni->value_state op arg1 state))
      ((boolean-expr? op) (eval-boolean-expr-uni->value_state op arg1 state))
      (else (error "unsupported expression")))))

; this function evaluates booleans
; this function is for 1 argument boolean expressions
(define eval-boolean-expr-uni->value_state
  (lambda (op arg1 state)
    (cond
      ((eq? (G-type-lookup arg1 state) 'boolean)
       (cons ((boolean-operator-to-function-uni op)
              (get-value-from-pair (G-value-lookup->value_state arg1 state)))
             (list (get-state-from-pair (G-value-lookup->value_state arg1 state)))))
      (else (error "boolean operator not valid for non boolean types")))))

; this function evaluates math expressions
; it currently only supports integers
; this function is for 1 argument math expressions
(define eval-math-expr-uni->value_state
  (lambda (op arg1 state)
    (cond
      ((eq? (G-type-lookup arg1 state) 'integer)
       (eval-math-expr-int-uni->value_state op arg1 state))
      (else (error "invalid type for math expression")))))

; this function evaluates math expressions of integers
; this function is for 1 argument math expressions
(define eval-math-expr-int-uni->value_state
  (lambda (op arg1 state)
    (cons ((math-operator-to-function-uni op #t)
           (get-value-from-pair (G-value-lookup->value_state arg1 state)))
          (list (get-state-from-pair (G-value-lookup->value_state arg1 state))))))

; this function evaluates all 2 argument expressions
; it currently only evaluates ints and booleans
(define eval-expr-multi->value_state
  (lambda (op arg1 arg2 state)
    (cond
      ((compare-expr? op) (eval-compare-expr-multi->value_state op arg1 arg2 state))
      ((math-expr? op) (eval-math-expr-multi->value_state op arg1 arg2 state))
      ((boolean-expr? op) (eval-boolean-expr-multi->value_state op arg1 arg2 state))
      (else (error "unsupported expression")))))

; this function evaluates comparisons
; this function is for 2 argument comparison expressions
(define eval-compare-expr-multi->value_state
  (lambda (op arg1 arg2 state)
    ; We return a (value, state), hence the cons for the value and the state
    ; The value is derived from applying the operator on arg1 and arg2
    ; To handle side effects, the state passed into arg2 is the state after evaluating arg1
    (cons ((compare-operator-to-function-multi op)
           (get-value-from-pair (G-value-lookup->value_state arg1
                                                            state))
           (get-value-from-pair (G-value-lookup->value_state arg2
                                                            (get-state-from-pair (G-value-lookup->value_state arg1 state)))))
          (list (get-state-from-pair (G-value-lookup->value_state arg2
                                                            (get-state-from-pair (G-value-lookup->value_state arg1
                                                                                                             state))))))))

; this function evaluates booleans
; this function is for 2 argument boolean expressions
; Returns (value, updated->state)
(define eval-boolean-expr-multi->value_state
  (lambda (op arg1 arg2 state)
    (cond
      ; We return a (value, state), hence the cons for the value and the state
      ; The value is derived from applying the operator on arg1 and arg2
      ; To handle side effects, the state passed into arg2 is the state after evaluating arg1
      ((and (eq? (G-type-lookup arg1 state) 'boolean) (eq? (G-type-lookup arg2 state) 'boolean))
       (cons ((boolean-operator-to-function-multi op)
              (get-value-from-pair (G-value-lookup->value_state arg1
                                                               state))
              (get-value-from-pair (G-value-lookup->value_state arg2
                                                               (get-state-from-pair (G-value-lookup->value_state arg1 state)))))
             (list (get-state-from-pair (G-value-lookup->value_state arg2 (get-state-from-pair (G-value-lookup->value_state arg1 state)))))))
      (else (error "not valid types for boolean expression")))))


; this function evaluates math expressions
; it currently only supports integers
; this function is for 2 argument math expressions
(define eval-math-expr-multi->value_state
  (lambda (op arg1 arg2 state)
    (cond
      ((and (eq? (G-type-lookup arg1 state) 'integer) (eq? (G-type-lookup arg2 state) 'integer))
       (eval-math-expr-int-multi->value_state op arg1 arg2 state))
      (else (error "invalid types for math expression")))))

; this function evaluates math expressions of integers
; this function is for 2 argument math expressions
; returns updated state
(define eval-math-expr-int-multi->value_state
  (lambda (op arg1 arg2 state)
    ; We return a (value, state), hence the cons for the value and the state
    ; The value is derived from applying the operator on arg1 and arg2
    ; To handle side effects, the state passed into arg2 is the state after evaluating arg1
    (cons ((math-operator-to-function-multi op #t)
           (get-value-from-pair (G-value-lookup->value_state arg1
                                                            state))
           (get-value-from-pair (G-value-lookup->value_state arg2
                                                            (get-state-from-pair (G-value-lookup->value_state arg1 state)))))
          (list (get-state-from-pair (G-value-lookup->value_state arg2 (get-state-from-pair (G-value-lookup->value_state arg1 state))))))))









; state-interface section
; this function takes values (integers, strings, variables, expressions, ...) and returns their actual value
; for now it only handles int and bolean literals and expressions of the two
(define G-value-lookup->value_state
  (lambda (value state)
    (cond
      ; if its an expression, evaluate to get value
      ((list? value) (G-eval-atomic-statement->value_state value state))
      ((integer? value) (cons value (list state)))
      ((boolean? value) (cons value (list state)))
      ((java-boolean? value) (cons (lookup-java-boolean value) (list state)))
      ((G-initialized? value state) (cons (variable-value-lookup value state) (list state)))
      (else (error "unsupported value lookup")))))

; Determines whether a boolean in java boolean notation was encountered
(define java-boolean?
  (lambda (value)
    (cond
      ((eq? value 'true) #t)
      ((eq? value 'false) #t)
      (else #f))))


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


; tests whether a variable is declared in a given scope, which is a state in the list of states we have.
(define declared-in-scope?
  (lambda (lis variable)
    (cond
      ((null? lis) #f)
      ((eq? (get-variable-section-head lis) variable) #t)
      (else (declared-in-scope? (get-variable-section-tail lis) variable)))))

;tests whether variable is declared and initialized
(define G-initialized?
  (lambda (variable-name state)
    (cond
      ((not (G-declared? variable-name state)) #f)
      ((null? (variable-value-lookup variable-name state)) #f)
      (else #t))))









; Adding/removing state section
; This section helps other sections affect the state with scoping rules
(define G-add-scope-to-state->state
  (lambda (state)
    (cons (get-top-scope initstate) state)))

(define G-remove-scope-from-state->state
  (lambda (state)
    (cond
      ((state-empty? state) (error "The main scope can't be removed!"))
      ((null? (get-tail-scope state)) initstate)
      (else (get-tail-scope state)))))









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
      ((or (number? value) (null? value) (boolean? value))
       (push-variable-as-literal->state variable value state))
      ; If the value is not a number, push the value of this variable to the state
      (else (error "Value is a variable, expected to be value")))))

; Pushes a variable and a number to the state, or updates the state if the variable is there
; Returns the updated state
(define push-variable-as-literal->state
  (lambda (variable number state)
    (cond
      ; If the state is empty, push to the state
      ((state-empty? state)
       (list (list (list variable)
             (list number))))

      ((G-declared? variable state) (update-variable variable number state))
      (else
       (cons
        (list
         (cons variable (get-variable-section-state (get-top-scope state)))
         (cons number (get-value-section-state (get-top-scope state))))
        (G-remove-scope-from-state->state state))))))

; precondition: the variable is somehwere in the state
(define update-variable
  (lambda (variable number state)
    (cond
      ((declared-in-scope? (get-variable-section-state (get-top-scope state)) variable)
       (cons (update-variable-in-scope variable number (get-top-scope state))
             (get-tail-scope state)))
      (else (cons (get-top-scope state) (update-variable variable number (get-tail-scope state)))))))

(define update-variable-in-scope
  (lambda (variable number state)
    (cond
      ((eq? (get-state-variable-head state) variable)
       (list (cons variable
                   (get-state-variable-tail state))
             (cons number
                   (get-state-value-tail state))))
      (else (append-state
             (get-head-state state)
             (update-variable-in-scope variable number (get-tail-state state)))))))


; appends a head state to a tail state
; (e.g. ((a) (1)) appended to ((b c d) (2 3 4))
; yields ((a b c d) (1 2 3 4)))
(define append-state
  (lambda (head-state tail-state)
    (list
     (append (list (get-state-variable-head head-state))
             (get-variable-section-state tail-state))
     (append (list (get-state-value-head head-state))
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

(define lookup-variable-value-in-scope
  (lambda (variable state)
    (cond
      ((eq? (get-state-variable-head state) variable)
       (get-state-value-head state))
      (else (lookup-variable-value-in-scope variable (get-tail-state state))))))

; this function takes values (integers, strings, variables, ...) and returns their type
; for now it only handles any atomic statement
(define G-type-lookup
  (lambda (value state)
    (cond
      ((list? value)
       (G-type-lookup (get-value-from-pair (G-value-lookup->value_state value state))
                      state))
      ((integer? value) 'integer)
      ((boolean? value) 'boolean)
      ((java-boolean? value) 'boolean)
      ((G-initialized? value state) (variable-type-lookup value state))
      (else (error "uninitialized variable or unsupported type")))))

; looks up the type of a variable in the state
(define variable-type-lookup
  (lambda (variable state)
    (G-type-lookup (variable-value-lookup variable state) state)))

; Important section helper functions for abstraction are defined below

(define get-head-state
  (lambda (state)
    (list
     (list (get-state-variable-head state))
     (list (get-state-value-head state)))))

(define get-tail-state
  (lambda (state)
    (list (get-state-variable-tail state)
          (get-state-value-tail state))))
