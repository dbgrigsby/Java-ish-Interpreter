(load "simpleParser.scm")
; Interpretation loop section


(define interpret
  (lambda (filename)
    (evaluate_parse_tree (parser filename) `(()()) )))

(define evaluate_parse_tree
  (lambda (program state)
    (cond
      ((null? program) (error "No program contents"))
      ((not (list? program)) (error "Invalid program syntax"))
      ;temporary work in
      ((eq? 'return (car (car program))) (G_evaluate_return_statement (car program) state))
      ((pair? (car program))  (evaluate_parse_tree (cdr program) (evaluate_statement (car program) state)))
      (else (error "Invalid program syntax")))))

; Returns state updated after evaluating pair
(define evaluate_statement
  (lambda (arglist state)
    (cond
      ((null? arglist) (error "Not a statement"))
      ((eq? 'var (car arglist)) (G_evaluate_var_declare_statement arglist state))
      ((eq? 'while (car arglist)) (G_evaluate_while_statement arglist state))
      ((eq? 'if (car arglist)) (G_evaluate_if_statement arglist state))
      (else (G_eval_atomic_statement arglist state)))))

















; return statement section
; currently returns both state and value, should just return value
(define G_evaluate_return_statement
  (lambda (arglist state)
    (G_eval_atomic_statement (rest_of_return_statement arglist) state)))

(define rest_of_return_statement
  (lambda (arglist)
    (cdr arglist)))















; if statement section
; currently does nothing as placeholder
(define G_evaluate_if_statement
  (lambda (arglist state)
    (state)))




















; while loop section
; currently does nothing as placeholder
(define G_evaluate_while_statement
  (lambda (arglist state)
    (state)))


























; Variable declaration section
; Returns updated state after a declaration or initialization
(define G_evaluate_var_declare_statement
  (lambda (arglist state)
    (cond
      ((null? (cdr arglist)) (error "Nothing after the var"))
      ((G_declared? (get_var_name_from_declare_args arglist) state)
       (error "variable already declared"))
      ((only_declare? arglist) (declare_var (get_var_name_from_declare_args arglist) state))
      (else (initialize_var (get_var_name_from_declare_args arglist)
                            (get_value_from_pair (G_eval_atomic_statement (truncate_var_name_from_declare arglist) state)) state)))))

(define only_declare?
  (lambda (arglist)
    (null? (cddr arglist))))

(define get_var_name_from_declare_args
  (lambda (arglist)
    (cadr arglist)))

(define truncate_var_name_from_declare
  (lambda (arglist)
    (cddr arglist)))

(define declare_var
  (lambda (name state)
    (G_push_state name `() state)))

; Must check if value is variable or not
(define initialize_var
  (lambda (name value state)
    (G_push_state name value state)))
; End of section 


















; atomic statement evaluator
; atomic statements are statements that are valid inside of a conditional statement/assignment statement or on their own
; at the moment, this is just assign statements and expressions
; returns a pair of (value, state)
(define G_eval_atomic_statement
  (lambda (arglist state)
    (cond
      ((G_single_value? arglist) (G_value_lookup (car arglist) state))
      ((G_expr? arglist) (G_eval_expr arglist state))
      ((G_assign? arglist) (G_eval_assign arglist state))
      (else (error "not a valid atomic statement")))))

(define G_single_value?
  (lambda (arglist)
    (eq? (length arglist) 1)))

(define G_atomic_statement?
  (lambda (arglist state)
    (cond
      ((G_expr? arglist) #t)
      ((G_assign? arglist) #t)
      (else #f))))

(define get_op_from_expr
  (lambda (arglist)
    (car arglist)))

(define get_arg1_from_expr
  (lambda (arglist)
    (cadr arglist)))

(define get_arg2_from_expr
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
; will returns value state pair
(define G_eval_assign
  (lambda (arglist state)
    (cond
      ((not (G_assign? arglist)) (error "not an assignment"))
      ((G_declared? (get_arg1_from_expr arglist) state)
       (cons
        (get_value_from_pair (G_value_lookup (get_arg1_from_expr arglist)
                              (G_push_state (get_arg1_from_expr arglist)
                                            (get_value_from_pair (G_eval_atomic_statement (get_arg2_from_expr arglist) state))
                                            (get_state_from_pair (G_eval_atomic_statement (get_arg2_from_expr arglist) state)))))
        (list (G_push_state (get_arg1_from_expr arglist)
                            (get_value_from_pair (G_eval_atomic_statement (get_arg2_from_expr arglist) state))
                            (get_state_from_pair (G_eval_atomic_statement (get_arg2_from_expr arglist) state))))))
      (else (error "variable undeclared")))))

(define G_assign?
  (lambda (arglist)
    (cond
      ((eq? (get_op_from_expr arglist) '=) #t)
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
      ((eq? (length arglist) 2) (eval_expr_uni (get_op_from_expr arglist) (get_arg1_from_expr arglist) state))
      ((eq? (length arglist) 3) (eval_expr_multi (get_op_from_expr arglist) (get_arg1_from_expr arglist) (get_arg2_from_expr arglist) state))
      (else (error "invalid number of arguments")))))

(define G_expr?
  (lambda (arglist)
    (cond
      ((math_expr? (get_op_from_expr arglist)) #t)
      ((boolean_expr? (get_op_from_expr arglist)) #t)
      ((compare_expr? (get_op_from_expr arglist)) #t)
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
      ((and (eq? op '%) is_int) modulo)
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
      ((G_initialized? value state) (cons (variable_value_lookup value state) (list state)))
      (else (error "unsupported value lookup")))))


; tests whether variable is declared
; this function will need to check inputs and error
; as it will recieve bogus inputs
(define G_declared?
  (lambda (variable_name state)
    (cond
      ((null? state) (error "State is empty"))
      ((null? (car state)) #f)
      ((eq? (get_state_variable_head state) variable_name) #t)
      (else (G_declared? variable_name
                                   (list (get_state_variable_tail state)
                                         (get_state_value_tail state)))))))

;tests whether variable is declared and initialized
(define G_initialized?
  (lambda (variable_name state)
    (cond
      ((not (G_declared? variable_name state)) #f)
      ((null? (variable_value_lookup variable_name state)) #f)
      (else #t))))

; adds variable to state or over-writes it
; returns the new state
(define G_push_state
  (lambda (variable value state)
    (cond
      ((null? state) (list (list variable) (list value)))
      ((null? (car state)) (list (list variable) (list value)))
      ((eq? (get_state_variable_head state) variable)
       (list (cons variable (get_state_variable_tail state))
             (cons value (get_state_value_tail state))))
      (else (append_state
             (get_head_state state)
             (G_push_state variable value (get_tail_state state)))))))

; appends a head state to a tail state
; (e.g. ((a) (1)) appended to ((b c d) (2 3 4))
; yields ((a b c d) (1 2 3 4)))
(define append_state
  (lambda (head_state tail_state)
    (list
     (append (list (get_state_variable_head head_state)) (car tail_state))
     (append (list (get_state_value_head head_state)) (cadr tail_state)))))

; looks up the value of a variable in the state
; returns the value of the variable or an error if the variable was not found
(define variable_value_lookup
  (lambda (variable state)
    (cond
      ((null? state) (error "State is empty"))
      ((null? (car state)) (error "Variable not found in state"))
      ((eq? (get_state_variable_head state) variable) (get_state_value_head state))
      (else (variable_value_lookup variable
                                   (list (get_state_variable_tail state)
                                         (get_state_value_tail state)))))))

(define get_state_variable_head
  (lambda (state)
    (caar state)))

(define get_state_variable_tail
  (lambda (state)
    (cdar state)))

(define get_state_value_head
  (lambda (state)
    (caadr state)))

(define get_state_value_tail
  (lambda (state)
    (cdadr state)))

(define get_tail_state
  (lambda (state)
    (list (get_state_variable_tail state) (get_state_value_tail state))))

(define get_head_state
  (lambda (state)
    (list
     (list (get_state_variable_head state))
     (list (get_state_value_head state)))))

; this function takes values (integers, strings, variables, ...) and returns their type
; for now it only handles int and boolean literals
(define G_type_lookup
  (lambda (value state)
    (cond
      ((list? value) (G_type_lookup (get_value_from_pair (G_value_lookup value state)) state))
      ((integer? value) 'integer)
      ((boolean? value) 'boolean)
      ((G_initialized? value state) (variable_type_lookup value state))
      (else (error "uninitialized variable or unsupported type")))))

; looks up the type of a variable in the state
(define variable_type_lookup
  (lambda (variable state)
    (G_type_lookup (variable_value_lookup variable state) state)))