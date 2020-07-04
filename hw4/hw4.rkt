#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
   (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; SICP 2.10 - Modify div-interval

(define (div-interval x y)
  (if(and (< (lower-bound y) 0)(> (upper-bound y)0))
     (error "横跨0了!")
     (mul-interval x
                (make-interval(/ 1.0 (upper-bound y))
                              (/ 1.0 (lower-bound y))))))


;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  (let ((p1 (/ tol 100)))
    (make-interval (* (- 1 p1) c) (*(+ 1 p1)c))))
(define (percent cp)
  (*(/(/ (- (upper-bound cp) (lower-bound cp))2)(center cp)) 100))

; SICP 2.17 - Define last-pair

(define (last-pair lst)
  (if(= (length lst) 1)
     lst
     (last-pair (cdr lst))))

; SICP 2.20 - Define same-parity

(define (same-parity sample . others)
  (filter (if(even? sample)
             even?
             odd?)
          (cons sample others))
  )

; SICP 2.22 - Write your explanation in the comment block:

#|
Your explanation here
;;第一个是反向的,第二个程序是序对而非列表。
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  (cond((null? lst)'())
       ((equal? (car lst)
                old)
        (cons new
              (substitute (cdr lst)
                          old new)))
       ((list?(car lst))
        (cons (substitute (car lst)
                         old new)
              (substitute (cdr lst)
                         old new)))
       (else
        (cons (car lst)
              (substitute (cdr lst)
                          old new)))))

; Exercise 3 - Define my-substitute2
(define (nth lst item)
  (if(equal? (car lst)item)
     1
     (+ 1 (nth (cdr lst) item))))


(define (substitute2 lst old new)
  (cond((null? lst)'())
       ((and(not(list? (car lst)))
            (member? (car lst) old))
        (cons (item (nth old (car lst))
                    new)
              (substitute2 (cdr lst)
                          old new)))
       ((list?(car lst))
        (cons (substitute2 (car lst)
                         old new)
              (substitute2 (cdr lst)
                         old new)))
       (else
        (cons (car lst)
              (substitute2 (cdr lst)
                          old new)))))

(define (cxr-function name)
  (lambda(lst)
    (define (helper rst)
      (cond ((equal? rst 'ar)(car lst))
            ((equal? rst 'dr)(cdr lst))
            ((equal? (first rst) 'a)
             (car(helper (bf rst))))
            (else
             (cdr(helper (bf rst))))))
    (helper (bf name))
))

(define (cxr-functionn name)
  (lambda(lst)
    (apply (eval name) (list lst))))


(lambda (add n m)
  (lambda(f)(lambda(x)((n f)((m f)x)))))
(define zero (lambda(f)(lambda(x)x)))
(define (add-1 n)
  (lambda(f)(lambda(x)(f ((n f)x)))))
(define one
  (lambda(f)(lambda(x)(f x))))
(define two
  (lambda(f)(lambda(x)(f (f x)))))
(define three
  (lambda(f)(lambda(x)(f(f(f x))))))


(trace zero)
(trace add-1)

(define (reverse lst)
  (define (iter acclist rlst)
    (if(null? rlst)
       acclist
       (iter (cons (car rlst)
                   acclist)
             (cdr rlst))))
  (iter '() lst))


