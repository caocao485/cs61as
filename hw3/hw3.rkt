#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter b n)
  ; Your code here
  (define (iter x y count)
    (cond((= count 0)x)
          ((even? count)(iter x (square y)(/ count 2)))
          (else(iter (* x y) y (- count 1)))))
  (iter 1 b n)
)

; Exericse 2 - Define phi

(define (phi)
  ; Your code here
  (fixed-point (lambda(x)(+ 1 (/ 1 x))) 1.0)
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
  ; Your code here
  (define (iter count)
    (if(= count k)
    (/ (n k) (d k))
    (/ (n count) (+ (d count)
                    (iter (+ count 1))))))
  (iter 1)
)

;; Iterative version
(define (cont-frac-iter n d k)
  ; Your code here
  (define (iter count acc)
    (if(= count 0)
       acc
       (iter (- count 1) (/ (n count) (+(d count) acc)))))
  (iter k 0)
)

(define (e k)
  ; Your code here to estimate e using cont-frac with k terms.
  (+
   (cont-frac-iter
    (lambda(x)1)
   (lambda(count)
     (if(= (remainder (+ count 1) 3) 0)
        (* 2 (/ (+ count 1)3))
        1))
   k)
   2)
)

; Exercise 4 - Define next-perf
(define (allFactors n)
  (define (iter i sent)
    (cond ((= i (sqrt n))(se i sent))
          ((> i (sqrt n))sent)
          ((= (remainder n i)0)
           (iter (+ i 1) (se i (/ n i) sent)))
          (else
           (iter (+ i 1) sent))))
  (iter 2 '()))

(define (next-perf n)
  ;  Your code here
  (let*
      ((allfactor (allFactors n))
         (acc (accumulate + 1  allfactor)))
    (cond((= n 1)
        (next-perf( + n 1)))
       ((= acc n)
        n)
       (else (next-perf (+ n 1)))))
)

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here
如果调换位置，会忽略1多余一次的情况。
|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))

(define (evaluate-counter b n)
  (define (iter counter)
    (if(> counter n)
       #f
       (if(= 37 (+ (* (expt b (- n counter)) counter) b n))
          counter
          (iter (+ counter 1)))))
  (iter 1))

(define (make-sen b n)
  (define (make-sen-one c z sen)
    (if(> c b)
       sen
       (if(not(boolean? (evaluate-counter c z)))
          (make-sen-one (+ c 1) z
                        (se (list c  z(evaluate-counter c z)) sen))
          (make-sen-one (+ c 1) z sen))))
  (if(= n 0)
     '()
     (se (make-sen-one 1 n '())
      (make-sen b (- n 1)))))

#|

Formula for expt:

Formula for expt-iter:
b^(n - counter) = product
product * counter + b + n = 37
(b^(n - counter))*counter + b + n = 37


要么b = 1 ，即  1*counter + 1 + n =  counter + n + 1 = 37， counter + n =36 
要么 n = counter， 即 counter + b + n = 2n + b = 37 
|#
