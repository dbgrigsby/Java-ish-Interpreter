; Helper for index-of
(define index-of-helper
  (lambda (var list index)
    (cond
      ((null? list) -1)
      ((eq? var (car list)) index)
      (else (index-of-helper var (cdr list) (+ 1 index))))))

; Returns index of item in list
(define index-of
  (lambda (var list)
    (index-of-helper var list 0)))

; Removes item at index in list
(define delete-at-index
  (lambda (index list)
    (cond
      ((= index 0) (cdr list))
      ((< index 0) list)
      (else (append (cons (car list) `())  (delete-at-index (- index 1) (cdr list) ))))))

; Removes an item from the state, both variable and value list
(define remove-from-state
  (lambda (var state)
    (cond
     ((null? state) `())
     (else (list (remove var (s-variables state)) (delete-at-index (index-of var (s-variables state)) (s-values state)))))))

; Gets variables from state (car)
(define s-variables
  (lambda (state)
    (car state)))

; Get values from state (cadr)
(define s-values
  (lambda (state)
    (cadr state)))

; Adds/replaces value of var in state, returns new state
(define update-state
  (lambda (var val state)
    (list (cons var (s-variables (remove-from-state var state))) (cons val (s-values (remove-from-state var state))))))



