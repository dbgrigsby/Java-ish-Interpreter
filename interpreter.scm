;


















; atomic statement evaluator
; atomic statements are statements that are valid inside of a conditional statement/assignment statement or on their own
; at the moment, this is just assign statements and expressions
; returns a pair of (value, state)
(define G_eval_atomic_statement
  (lambda (arglist state)
    (cond
      ((G_expr? arglist) (G_eval_expr arglist state))
      ((G_assign? arglist) (G_eval_assign arglist state))
      (else (error "not a valid atomic statement")))))

(define G_atomic_statement?
  (lambda (arglist state)
    (cond
      ((G_expr? arglist) #t)
      ((G_assign? arglist) #t)
      (else #f))))

(define get_op
  (lambda (arglist)
    (car arglist)))

(define get_arg1
  (lambda (arglist)
    (cadr arglist)))

(define get_arg2
  (lambda (arglist)
    (caddr arglist)))

(define get_state_from_pair
  (lambda (args)
    (cadr args)))

(define get_value_from_pair
  (lambda (args)
    (car args)))


;





















; eval_assign section
; this function evaluates assignment statements
; currently does nothing
; will returns new state and value pair
(define G_eval_assign
  (lambda (arglist state)
    (cond
      ((not (G_assign? (get_op arglist))) (error "not an assignment"))
      ((G_declared? (get_arg1 arglist))
       (cons
        (G_value_lookup (get_arg1 arglist)
                              (G_push_state (get_arg1 arglist)
                                            (G_eval_atomic_statement (get_arg2 arglist) state)
                                            (G_type_lookup (get_arg2 arglist) state)))
        (list (G_push_state (get_arg1 arglist)
                     (G_eval_atomic_statement (get_arg2 arglist) state)
                     (G_type_lookup (get_arg2 arglist) state)))))
      (else (error "variable undeclared")))))

(define G_assign?
  (lambda (arglist)
    (cond
      ((eq? (get_op arglist) '=) #t)
      (else #f))))




; 



















; eval_expression section
; this function evaluates all expressions
; expressions are defined as built in math, boolean, or comparison operators
; expressions cover the scope of math expressions and boolean expressions (or conditional statements)
    
(define G_eval_expr
  (lambda (arglist state)
    (cond
      ((not (G_expr? arglist)) (error "given invalid expression operation"))
      ((eq? (length arglist) 2) (eval_expr_uni (get_op arglist) (get_arg1 arglist) state))
      ((eq? (length arglist) 3) (eval_expr_multi (get_op arglist) (get_arg1 arglist) (get_arg2 arglist) state))
      (else (error "invalid number of arguments")))))

(define G_expr?
  (lambda (arglist)
    (cond
      ((math_expr? (get_op arglist)) #t)
      ((boolean_expr? (get_op arglist)) #t)
      ((compare_expr? (get_op arglist)) #t)
      (else #f))))

(define math_expr?
  (lambda (op)
    (cond
      ((eq? op '+) #t)
      ((eq? op '-) #t)
      ((eq? op '*) #t)
      ((eq? op '/) #t)
      ((eq? op '%) #t)
      (else #f))))

(define boolean_expr?
  (lambda (op)
    (cond
      ((eq? op '&&) #t)
      ((eq? op '||) #t)
      ((eq? op '!) #t)
      (else #f))))

(define compare_expr?
  (lambda (op)
    (cond
      ((eq? op '==) #t)
      ((eq? op '!=) #t)
      ((eq? op '<) #t)
      ((eq? op '>) #t)
      ((eq? op '<=) #t)
      ((eq? op '>=) #t)
      (else #f))))
  
; this function evaluates all 1 argument expressions
; it currently only evaluates ints and booleans
(define eval_expr_uni
  (lambda (op arg1 state)
    (cond
      ((math_expr? op) (eval_math_expr_uni op arg1 state))
      ((boolean_expr? op) (eval_boolean_expr_uni op arg1 state))
      (else (error "unsupported expression")))))

; this function evaluates booleans
; this function is for 1 argument boolean expressions
(define eval_boolean_expr_uni
  (lambda (op arg1 state)
    (cond
      ((eq? (G_type_lookup arg1 state) 'boolean)
       (cons ((boolean_operator_to_function_uni op) (get_value_from_pair(G_value_lookup arg1 state)))
             (list (get_state_from_pair (G_value_lookup arg1 state)))))
      (else (error "boolean operator not valid for non boolean types")))))

; this function evaluates math expressions
; it currently only supports integers
; this function is for 1 argument math expressions
(define eval_math_expr_uni
  (lambda (op arg1 state)
    (cond
      ((eq? (G_type_lookup arg1 state) 'integer)
       (eval_math_expr_int_uni op arg1 state))
      (else (error "invalid type for math expression")))))

; this function evaluates math expressions of integers
; this function is for 1 argument math expressions
(define eval_math_expr_int_uni
  (lambda (op arg1 state)
    (cons ((math_operator_to_function_uni op #t) (get_value_from_pair (G_value_lookup arg1 state)))
          (list (get_state_from_pair (G_value_lookup arg1 state))))))

; this function evaluates all 2 argument expressions
; it currently only evaluates ints and booleans
(define eval_expr_multi
  (lambda (op arg1 arg2 state)
    (cond
      ((compare_expr? op) (eval_compare_expr_multi op arg1 arg2 state))
      ((math_expr? op) (eval_math_expr_multi op arg1 arg2 state))
      ((boolean_expr? op) (eval_boolean_expr_multi op arg1 arg2 state))
      (else (error "unsupported expression")))))

; this function evaluates comparisons
; this function is for 2 argument comparison expressions
(define eval_compare_expr_multi
  (lambda (op arg1 arg2 state)
    (cons
     ((compare_operator_to_function_multi op) (G_value_lookup arg1 state) (G_value_lookup arg2 state))
     (list (get_state_from_pair (G_value_lookup arg1 (get_state_from_pair (G_value_lookup arg2 state))))))))

; this function evaluates booleans
; this function is for 2 argument boolean expressions
(define eval_boolean_expr_multi
  (lambda (op arg1 arg2 state)
    (cond
      ((and (eq? (G_type_lookup arg1 state) 'boolean) (eq? (G_type_lookup arg2 state) 'boolean))
       (cons ((boolean_operator_to_function_multi op)
              (get_value_from_pair (G_value_lookup arg1 state))
              (get_value_from_pair (G_value_lookup arg2 state)))
             (list (get_state_from_pair (G_value_lookup arg1 (get_state_from_pair (G_value_lookup arg2 state)))))))
      (else (error "not valid types for boolean expression")))))
      
      
; this function evaluates math expressions
; it currently only supports integers
; this function is for 2 argument math expressions
(define eval_math_expr_multi
  (lambda (op arg1 arg2 state)
    (cond
      ((and (eq? (G_type_lookup arg1 state) 'integer) (eq? (G_type_lookup arg2 state) 'integer))
       (eval_math_expr_int_multi op arg1 arg2 state))
      (else (error "invalid types for math expression")))))
    
; this function evaluates math expressions of integers
; this function is for 2 argument math expressions
(define eval_math_expr_int_multi
  (lambda (op arg1 arg2 state)
    (cons ((math_operator_to_function_multi op #t)
           (get_value_from_pair (G_value_lookup arg1 state))
           (get_value_from_pair (G_value_lookup arg2 state)))
          (list (get_state_from_pair (G_value_lookup arg1 (get_state_from_pair (G_value_lookup arg2 state))))))))

; this function takes a boolean operator (ie: !) and returns the actual function
; this translation is for 1 argument boolean expressions
(define boolean_operator_to_function_uni
  (lambda (op)
    (cond
      ((eq? op '!) (lambda (arg1) (not arg1)))
      (else (error "unsupported unary boolean expression")))))

; this function takes a math operator ie: - along with a boolean is_int that specifies whether the values are integers
; this translation is for 1 argument math expressions
(define math_operator_to_function_uni
  (lambda (op is_int)
    (cond
      ((eq? op '-) -)
      (else (error "invalid unary math operator")))))

; this function takes a math operator ie: +, -, * ... along with a boolean is_int that specifies whether the values are integers
; this translation is for 2 argument math expressions
(define math_operator_to_function_multi
  (lambda (op is_int)
    (cond
      ((eq? op '+) +)
      ((eq? op '*) *)
      ((eq? op '-) -)
      ((and (eq? op '/) is_int) quotient)
      ((eq? op '/) /)
      ((and (eq? op '%) is_int) %)
      ((eq? op '%) (error "modulo % only works on integers"))
      (else (error "invalid math operator")))))

; this function takes a boolean operator (ie: &&, ||, ...) and returns the actual function
; this translation is for 2 argument boolean expressions
(define boolean_operator_to_function_multi
  (lambda (op)
    (cond
      ((eq? op '&&) (lambda (arg1 arg2) (and arg1 arg2)))
      ((eq? op '||) (lambda (arg1 arg2) (or arg1 arg2)))
      (else (error "unsupported boolean expression")))))
      
(define compare_operator_to_function_multi
  (lambda (op)
    (cond
      ((eq? op '==) equal?)
      ((eq? op '!=) (lambda (arg1 arg2) (not (equal? arg1 arg2))))
      ((eq? op '<) <)
      ((eq? op '>) >)
      ((eq? op '<=) <=)
      ((eq? op '>=) >=))))




;




















;
; state_interface section
; this function takes values (integers, strings, variables, expressions, ...) and returns their actual value
; for now it only handles int and bolean literals and expressions of the two
(define G_value_lookup
  (lambda (value state)
    (cond
      ;if its an expression, evaluate to get value
      ((list? value) (G_eval_atomic_statement value state))
      ((integer? value) (cons value (list state)))
      ((boolean? value) (cons value (list state)))
      ((G_declared? value) (cons (variable_value_lookup value state) (list state)))
      (else (error "unsupported value lookup")))))


; tests whether variable is declared
; currently does nothing as a placeholder

; this function will need to check inputs and error
; as it will recieve bogus inputs
(define G_declared?
  (lambda (variable_name state)
    (#f)))

; adds variable to state or over_writes it
; currently left un-defined
; should return the new state
(define G_push_state
  (lambda (variable value type)
    (#f)))

; looks up the value of a variable in the state
; currently does nothing as a placeholder
(define variable_value_lookup
  (lambda (variable state)
    (#f)))

; this function takes values (integers, strings, variables, ...) and returns their type
; for now it only handles int and bolean literals
(define G_type_lookup
  (lambda (value state)
    (cond
      ((list? value) (G_type_lookup (get_value_from_pair (G_value_lookup value state)) state))
      ((integer? value) 'integer)
      ((boolean? value) 'boolean)
      (else (error "unsupported type lookup")))))

; looks up the type of a variable in the state
; currently does nothing as a placeholder
(define variable_type_lookup
  (lambda (variable state)
    (#f)))