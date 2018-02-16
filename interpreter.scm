(load "simpleParser.scm")
; Interpretation loop section


(define interpret
  (lambda (filename)
    (evaluate_parse_tree (parser filename) `(()()) )))

(define evaluate_parse_tree
  (lambda (program state)
    (cond
      ; not all programs/ segments must end in return
      ; empty list should return the state (ie: at the end of an if statement's statements)
      ((null? program) state)
      ((not (list? program)) (error "Invalid program syntax"))
      ; temporary work in
      ((eq? 'return (car (car program))) (G_evaluate_return_statement-value_state (car program) state))
      ((pair? (car program))  (evaluate_parse_tree (cdr program) (evaluate_statement-state (car program) state)))
      (else (error "Invalid program syntax")))))


; Returns state updated after evaluating pair
(define evaluate_statement-state
  (lambda (arglist state)
    (cond
      ((null? arglist) (error "Not a statement"))
      ((eq? 'var (get_upcoming_statement_name arglist)) (G_evaluate_var_declare_statement-state arglist state))
      ((eq? 'while (get_upcoming_statement_name arglist)) (G_evaluate_while_statement-state arglist state))
      ((eq? 'if (get_upcoming_statement_name arglist)) (G_evaluate_if_statement-state arglist state))
      (else (get_state_from_pair (G_eval_atomic_statement-value_state arglist state))))))

; Returns the type of the upcoming statement in an arglist
; (e.g. (var x (+ 1 2)) yields 'var)
(define get_upcoming_statement_name
  (lambda (arglist)
    (car arglist)))

















; return statement section
; currently returns both state and value, should just return value
; returns state and value for debug purposes
(define G_evaluate_return_statement-value_state
  (lambda (arglist state)
    (G_eval_atomic_statement-value_state (rest_of_return_statement arglist) state)))

(define rest_of_return_statement
  (lambda (arglist)
    (cdr arglist)))











; if statement section
; currently does nothing as placeholder
(define G_evaluate_if_statement-state
  (lambda (arglist state)
    (cond
      ((get_value_from_pair (G_eval_atomic_statement-value_state (get_if_cond arglist) state))
       (get_state_from_pair (G_eval_atomic_statement-value_state (get_if_then arglist) (get_state_from_pair (G_eval_atomic_statement-value_state (get_if_cond arglist) state)))))
      ((has_else? arglist) (get_state_from_pair (G_eval_atomic_statement-value_state (get_if_else arglist) (get_state_from_pair (G_eval_atomic_statement-value_state (get_if_cond arglist) state)))))
      (else (get_state_from_pair (G_eval_atomic_statement-value_state (get_if_cond arglist) state))))))

(define get_if_cond
  (lambda (arglist)
    (cadr arglist)))

(define get_if_then
  (lambda (arglist)
    (caddr arglist)))

(define get_if_else
  (lambda (arglist)
    (cond
      ((not (has_else? arglist)) (error "no else statement"))
      (else (cadddr arglist)))))

(define has_else?
  (lambda (arglist)
    (pair? (cdddr arglist))))

















; while loop section
; currently does nothing as placeholder
(define G_evaluate_while_statement-state
  (lambda (arglist state)
    (state)))


























; Variable declaration section
; Returns updated state after a declaration or initialization
(define G_evaluate_var_declare_statement-state
  (lambda (arglist state)
    (cond
      ((null? (cdr arglist)) (error "Nothing after the var"))
      ((G_declared? (get_var_name_from_declare_args arglist) state)
       (error "variable already declared"))
      ((only_declare? arglist) (declare_var-state (get_var_name_from_declare_args arglist) state))
      (else (initialize_var-state (get_var_name_from_declare_args arglist)
                            (get_value_from_pair (G_eval_atomic_statement-value_state (truncate_var_name_from_declare arglist) state)) state)))))

(define only_declare?
  (lambda (arglist)
    (null? (cddr arglist))))

(define get_var_name_from_declare_args
  (lambda (arglist)
    (cadr arglist)))

(define truncate_var_name_from_declare
  (lambda (arglist)
    (cddr arglist)))

(define declare_var-state
  (lambda (name state)
    (G_push_state-state name `() state)))

; Pushes the initializes the variable to the state
(define initialize_var-state
  (lambda (name value state)
    (G_push_state-state name value state)))


















; atomic statement evaluator
; atomic statements are statements that are valid inside of a conditional statement/assignment statement or on their own
; at the moment, this is just assign statements and expressions
; returns a pair of (value, state)
; (e.g. (> x (+ y 1)) is an atomic statement)
; (e.g. (== 3 (= x (+ x 1)) is an atomic statement)
; Atomic statements can be put
(define G_eval_atomic_statement-value_state
  (lambda (arglist state)
    (cond
      ((single_atom? arglist) (G_value_lookup-value_state arglist state))
      ((single_value_list? arglist) (G_value_lookup-value_state (car arglist) state))
      ((G_expr? arglist) (G_eval_expr-value_state arglist state))
      ((G_assign? arglist) (G_eval_assign-value_state arglist state))
      (else (error "not a valid atomic statement")))))

(define single_atom?
  (lambda (arglist)
    (not (list? arglist))))

(define single_value_list?
  (lambda (arglist)
    (eq? (length arglist) 1)))

(define G_atomic_statement?
  (lambda (arglist state)
    (cond
      ((single_atom? arglist) #t)
      ((single_value_list? arglist) #t)
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























; eval_assign section
; this function evaluates assignment statements
; will returns value state pair
; (e.g. (= x 1) will return (1 (updated_state)))
(define G_eval_assign-value_state
  (lambda (arglist state)
    (cond
      ((not (G_assign? arglist)) (error "not an assignment"))
      ((G_declared? (get_arg1_from_expr arglist) state)
       (G_value_lookup-value_state (get_arg1_from_expr arglist)
                              (G_push_state-state (get_arg1_from_expr arglist)
                                            (get_value_from_pair (G_eval_atomic_statement-value_state (get_arg2_from_expr arglist) state))
                                            (get_state_from_pair (G_eval_atomic_statement-value_state (get_arg2_from_expr arglist) state)))))
      (else (error "variable undeclared")))))

(define G_assign?
  (lambda (arglist)
    (cond
      ((eq? (get_op_from_expr arglist) '=) #t)
      (else #f))))























; eval_expression section
; this function evaluates all expressions
; expressions are defined as built in math, boolean, or comparison operators
; expressions cover the scope of math expressions and boolean expressions (or conditional statements)
; Returns (value, updated_state)
(define G_eval_expr-value_state
  (lambda (arglist state)
    (cond
      ((not (G_expr? arglist)) (error "given invalid expression operation"))
      ((eq? (length arglist) 2) (eval_expr_uni-value_state (get_op_from_expr arglist) (get_arg1_from_expr arglist) state))
      ((eq? (length arglist) 3) (eval_expr_multi-value_state (get_op_from_expr arglist) (get_arg1_from_expr arglist) (get_arg2_from_expr arglist) state))
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
(define eval_expr_uni-value_state
  (lambda (op arg1 state)
    (cond
      ((math_expr? op) (eval_math_expr_uni-value_state op arg1 state))
      ((boolean_expr? op) (eval_boolean_expr_uni-value_state op arg1 state))
      (else (error "unsupported expression")))))

; this function evaluates booleans
; this function is for 1 argument boolean expressions
(define eval_boolean_expr_uni-value_state
  (lambda (op arg1 state)
    (cond
      ((eq? (G_type_lookup arg1 state) 'boolean)
       (cons ((boolean_operator_to_function_uni op) (get_value_from_pair (G_value_lookup-value_state arg1 state)))
             (list (get_state_from_pair (G_value_lookup-value_state arg1 state)))))
      (else (error "boolean operator not valid for non boolean types")))))

; this function evaluates math expressions
; it currently only supports integers
; this function is for 1 argument math expressions
(define eval_math_expr_uni-value_state
  (lambda (op arg1 state)
    (cond
      ((eq? (G_type_lookup arg1 state) 'integer)
       (eval_math_expr_int_uni-value_state op arg1 state))
      (else (error "invalid type for math expression")))))

; this function evaluates math expressions of integers
; this function is for 1 argument math expressions
(define eval_math_expr_int_uni-value_state
  (lambda (op arg1 state)
    (cons ((math_operator_to_function_uni op #t) (get_value_from_pair (G_value_lookup-value_state arg1 state)))
          (list (get_state_from_pair (G_value_lookup-value_state arg1 state))))))

; this function evaluates all 2 argument expressions
; it currently only evaluates ints and booleans
(define eval_expr_multi-value_state
  (lambda (op arg1 arg2 state)
    (cond
      ((compare_expr? op) (eval_compare_expr_multi-value_state op arg1 arg2 state))
      ((math_expr? op) (eval_math_expr_multi-value_state op arg1 arg2 state))
      ((boolean_expr? op) (eval_boolean_expr_multi-value_state op arg1 arg2 state))
      (else (error "unsupported expression")))))

; this function evaluates comparisons
; this function is for 2 argument comparison expressions
(define eval_compare_expr_multi-value_state
  (lambda (op arg1 arg2 state)
    (cons
     ((compare_operator_to_function_multi op)
      (get_value_from_pair (G_value_lookup-value_state arg1 state))
      (get_value_from_pair (G_value_lookup-value_state arg2 state)))
     (list (get_state_from_pair (G_value_lookup-value_state arg1 (get_state_from_pair (G_value_lookup-value_state arg2 state))))))))

; this function evaluates booleans
; this function is for 2 argument boolean expressions
; Returns (value, updated-state)
(define eval_boolean_expr_multi-value_state
  (lambda (op arg1 arg2 state)
    (cond
      ((and (eq? (G_type_lookup arg1 state) 'boolean) (eq? (G_type_lookup arg2 state) 'boolean))
       (cons ((boolean_operator_to_function_multi op)
              (get_value_from_pair (G_value_lookup-value_state arg1 state))
              (get_value_from_pair (G_value_lookup-value_state arg2 state)))
             (list (get_state_from_pair (G_value_lookup-value_state arg1 (get_state_from_pair (G_value_lookup-value_state arg2 state)))))))
      (else (error "not valid types for boolean expression")))))


; this function evaluates math expressions
; it currently only supports integers
; this function is for 2 argument math expressions
(define eval_math_expr_multi-value_state
  (lambda (op arg1 arg2 state)
    (cond
      ((and (eq? (G_type_lookup arg1 state) 'integer) (eq? (G_type_lookup arg2 state) 'integer))
       (eval_math_expr_int_multi-value_state op arg1 arg2 state))
      (else (error "invalid types for math expression")))))

; this function evaluates math expressions of integers
; this function is for 2 argument math expressions
; returns updated state
(define eval_math_expr_int_multi-value_state
  (lambda (op arg1 arg2 state)
    (cons ((math_operator_to_function_multi op #t)
           (get_value_from_pair (G_value_lookup-value_state arg1 state))
           (get_value_from_pair (G_value_lookup-value_state arg2 state)))
          (list (get_state_from_pair (G_value_lookup-value_state arg1 (get_state_from_pair (G_value_lookup-value_state arg2 state))))))))

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
























; state_interface section
; this function takes values (integers, strings, variables, expressions, ...) and returns their actual value
; for now it only handles int and bolean literals and expressions of the two
(define G_value_lookup-value_state
  (lambda (value state)
    (cond
      ; if its an expression, evaluate to get value
      ((list? value) (G_eval_atomic_statement-value_state value state))
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
      ((null? state) #f)
      ((null? (car state)) #f)
      ((eq? (get_state_variable_head state) variable_name) #t)
      (else (G_declared? variable_name (get_tail_state state))))))

;tests whether variable is declared and initialized
(define G_initialized?
  (lambda (variable_name state)
    (cond
      ((not (G_declared? variable_name state)) #f)
      ((null? (variable_value_lookup variable_name state)) #f)
      (else #t))))

; adds variable and its value to state or over-writes it
; If the value is a variable, the value of this variable is found and pushed to the state
; (e.g. if we are pushing (x y) and y = 3, we push (x 3) to the state
; returns the new state
(define G_push_state-state
  (lambda (variable value state)
    (cond
      ; If the value is a number, null, or boolean, push to the state
      ((or (number? value) (null? value) (boolean? value)) (push_variable_as_literal-state variable value state))
      ; If the value is not a number, push the value of this variable to the state
      (else (push_variable_as_variable-state variable value state)))))

; Pushes a variable and a number to the state, or updates the state if the variable is there
; Returns the updated state
(define push_variable_as_literal-state
  (lambda (variable number state)
    (cond
      ; If the state is empty, push to the state
      ((null? state) (list (list variable) (list number)))
      ((null? (car state)) (list (list variable) (list number)))

      ; If the variable head of the state equals the variable we are tryinig to push,
      ; Update the variable's value
      ((eq? (get_state_variable_head state) variable)
       (list (cons variable (get_state_variable_tail state))
             (cons number (get_state_value_tail state))))

      ; If the variable head doesn't equal the variable we are trying to push, keep searching for it
      (else (append_state
             (get_head_state state)
             (push_variable_as_literal-state variable number (get_tail_state state)))))))


; Pushes a variable and the value of the variable's value to the state, or updates the state if the variable is there
; Precondition: Value is not null
; Returns the updated state
(define push_variable_as_variable-state
  (lambda (variable value state)
    (cond
      ((not (G_declared? value state)) (error "Initialized variable value is an undeclared variable"))
      (else
       (push_variable_as_variable-state variable (get_value_from_pair (G_value_lookup-value_state value state)) state)))))


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
; for now it only handles any atomic statement
(define G_type_lookup
  (lambda (value state)
    (cond
      ((list? value) (G_type_lookup (get_value_from_pair (G_value_lookup-value_state value state)) state))
      ((integer? value) 'integer)
      ((boolean? value) 'boolean)
      ((G_initialized? value state) (variable_type_lookup value state))
      (else (error "uninitialized variable or unsupported type")))))

; looks up the type of a variable in the state
(define variable_type_lookup
  (lambda (variable state)
    (G_type_lookup (variable_value_lookup variable state) state)))
