;;Lesson 10

;;Exercise 1
;;SICP 3.5.1
#| Explain
;;promise

;;number

|#

;;Exercise 2
;;SICP 3.5.1
#| Explain
(force (cdr '(2 3))报错

|#

;;Exercise 3
#| Explain
区别在于 delay只是延迟enumerate-interval的计算
而stream-enumerate-interval是一个延迟流。

|#

;;Exercise 4
;a.
(define (num-seq n)
  (define streams
    (cons-stream n
                 (stream-map (lambda (x)
                               (if(odd? x)
                                  (+ (* x 3) 1)
                                  (/ x 2)))
                             streams)))
  streams)
(display-stream (num-seq 7))


;b.
(define (seq-length stream)
  (define (count n current-stream)
    (if(= (stream-car current-stream)1)
       (+ n 1)
       (begin
         (count (+ n 1) (stream-cdr current-stream)))))
  (count 0 stream))

(seq-length (num-seq 7)) ;;17

;;Exercise 5
;;3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;3.51
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
#| Returns:
1
2
3
4
5

|#
(stream-ref x 7)
#| Returns:
6
7

|#


;;3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

#| What is the value for 'sum'?
136

|#

#| What is the printed response to evaluating the stream-ref and display-stream?

10
15
45
55
105
120
190
210done
|#

#| Will it be diffferent if we implemented (delay <exp>) as (lambda () <exp>)
没有缓存的话，就会每次对sum进行赋值。

|#
;;3.53
#|Describe the elements of the stream
元素是(2^n)次方的元素,n为元素的序号,以0开始。

|#

;;3.54

(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1
                                (mul-streams
                                   factorials
                                   (stream-cdr integers))))

;;3.55
(define (add-streams s1 s2)(stream-map + s1 s2))
(define (partiam-sums stream)
  (cons-stream (stream-car stream)
               (add-streams
                (partiam-sums stream)
                (stream-cdr stream))))
(display-stream (partiam-sums integers))

;;3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


(define (scale-stream strm factor)
  (stream-map (lambda (x) (* x factor)) strm))
(define S (cons-stream
           1
           (merge
            (scale-stream s1 2)
            (merge
             (scale-stream s1 3)
             (scale-stream s1 5)))))


;;3.64
(define (average x y)(/ (+ x y)2))
(define (sqrt-improve guess x)(average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map
      (lambda(guess)(sqrt-improve guess x))
      guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (if(< (abs (- (stream-car stream)
             (stream-car (stream-cdr stream))))
        tolerance)
     (stream-car (stream-cdr stream))
     (stream-limit (stream-cdr stream) tolerance)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;3.66
#| Explain
a). 基于交错组合原则,对于(1 , n)的情况(n > 1),前面序对数有2n - 3 的规律,所以100之前的序对数目为197。
b). 对于(n, n)(n > 1),其前面一个序对为(1, 2^(n - 1)),所以(n, n)之前的序对数为(2 * 2 ^(n - 1) - 3 + 1)= 2^n - 2 ,那么(100, 100)的序对前的数量为 2^100 - 2。1.2676506e+30 - 2。
整体上:f(i,j) = 2^i * (j-i) + 2^(i-1) - 2, i < j。

|#

;;3.68
#| Explain
interleave不是延时求值的，所以pair会被循环调用，进入死循环。


|#


;;Exercise 6
(define (fract-stream lst)
  (define (f-stream nume deno)
    (let((quot(quotient (* nume 10) deno))
         (remain (remainder (* nume 10) deno)))
      (cons-stream
       quot
       (f-stream remain deno))))
  (f-stream (car lst) (cadr lst)))

(define (approximation f-stream n)
  (if(= n 0)
     '()
     (cons (stream-car f-stream)
           (approximation
            (stream-cdr f-stream)
            (- n 1)))))

(stream-car (stream-cdr (stream-cdr (fract-stream '(1 7)))))
(approximation (fract-stream '(1 7)) 4)
(approximation (fract-stream '(1 2)) 4)         


;;Optional Challenge Problem.
;;Note: this is NOT extra credit. Only do it if you want more practice.
#|
  Your explanation Here
|#
