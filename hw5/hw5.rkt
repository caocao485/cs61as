#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.


; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-structure branch)
  (cadr branch))
(define (branch-length branch)
  (car branch))

; b. Define total-weight.
(define (simple-weight?  structure)
  (number? structure))
(define (str-weight structure)
  (if(not(pair? structure))
     structure
     (total-weight structure)))
(define (branch-weight branch)
  (str-weight (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; c. Define balanced?

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))
(define (torque-and-balance-branch? branch)
  (let ((branchlength (branch-length branch))
        (branchstructure (branch-structure branch))
        (branch-torque (torque branch)))
    (cond((or(simple-weight? branchstructure)
             (balanced? branchstructure))
            (cons branch-torque #t))
         (else
          (cons branch-torque #f)))))
       
(define (balanced? mobile)
  (let ((left-branch-balance-torque (torque-and-balance-branch? (left-branch mobile)))
        (right-branch-balance-torque (torque-and-balance-branch? (right-branch mobile))))
    (and (cdr left-branch-balance-torque)
         (cdr right-branch-balance-torque)
         (= (car left-branch-balance-torque) (car right-branch-balance-torque)))))


; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))
(define (make-mobile-cons left right)
  (cons left right))
(define (make-branch-cons length structure)
  (cons length structure))
;;选择函数部分的right-branch和branch-structure函数修改即可
(define (left-branch-cons mobile)
  (car mobile))
(define (right-branch-cons mobile)
  (cdr mobile))

(define (branch-length-cons branch)
  (car branch))
(define (branch-structure-cons branch)
  (cdr branch))


;Exercise 3a - Define square-tree

(define (square-tree d-l)
  (cond((null? d-l) '())
       ((not (pair? d-l))(square d-l))
       (else (cons (square-tree (car d-l))
                   (square-tree (cdr d-l))))))

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
  (map (lambda(sub-tree)
         (if(pair? sub-tree)
            (tree-map fn sub-tree)
            (fn sub-tree)))
       tree))

;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda(v0)
         (accumulate + 0 
          (map * v0 v)))  m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(v0)
           (matrix-*-vector cols v0))
         m)))


;Exercise 6 - Give the property that op should satisfy:

#|

Your property here
* + 这样左结合、右结合的结果都一样的操作符 ，也即是传入的操作函数必须符合结合律(monoid)
|#

;Exercise 7 - Define equal?

(define (my-equal? list1 list2)
  (cond((and(not(pair? list1))(not(pair? list2)))
        (eq? list1 list2)) ;;包括'l()
       ((and(pair? list1)(pair? list2))
        (and(equal? (car list1)(car list2))(equal? (cdr list1)(cdr list2))))
       (else #f)))

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda(v)
                            (cons (car s)
                                  v))
                          rest)))))
;;和换硬币一样，包含(car s)的情况，和不包含(car s)值的情况，
;;而包含(car s)的各项又可以由不包含(car s)的序列各项与(car s)值组合起来形成。 


;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((symbol? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define word-operators '(first  butfirst bf last butlast bl word))

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (foldr + 0 (cdr args))))))
	((eq? fn '*) (foldr * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (foldr * 1 (cdr args))))))
        ((member? fn word-operators)
         (cond ((null? args) (error "Calc: no args to /"))
               (else (apply (eval fn) (map  calc-eval  args)))))
	(else (error "Calc: bad operator:" fn))))


