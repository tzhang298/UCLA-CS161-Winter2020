;Q1. PAD takes in an input N
; Returns Nth Padovan number
(defun PAD(N)
       (cond
           ((= n 0) 1) ;base cases
           ((= n 1) 1)
           ((= n 2) 1)
           (T (+ (PAD(- n 2)) (PAD(- n 3))) ;recursively call the function until it reaches base case
           )
       )
 )
;Q2 SUMS takes in an input N
; Returns number of + operations required to compute (PAD N)
(defun SUMS(N)
       (cond
           ((= n 0) 0) ;base cases
           ((= n 1) 0)
           ((= n 2) 0)
           (T (+ (SUMS(- n 2)) (SUMS(- n 3)) 1) ;recursively call the function until it reaches the base case
           )
       )
 )

 ;Q3 replace all leaves with ?
 (defun ANON(tree)
    (cond ((null tree) '()) ; base case
          ((atom tree) '?)
          (T (cons (ANON (car tree)) (ANON (cdr tree))) ; call the function for its children and combine the result
          )
    )
 )
