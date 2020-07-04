#lang sicp

#|
Exercise 1. Why did the student get an error?
因为(cdr x)是引用类型，并不会改变x的cdr部分。
如果想要改变，必须使用(set! x (cons (car x) y))的方式

|#

; Exercise 2
; Exercise 2a. Fill in the ?? so that the calls produce the desired effect.

(define list1 (list (list 'a) 'b))
(define list2 (list (list 'x) 'y))
(define (answer3)
  (set-cdr! (car list2) (cdr list1))
  (set-cdr! (car list1) (car list2)))
(answer3)
list1 ; Should output ((a x b) b)
list2 ; Should output ((x b) y)

;Exercise 2b.  Draw a box-and-pointer diagram that explains the effect 
;              of evaluating
;(set-car! (cdr list1) (cadr list2)).
;(Reminder:  You can use ASCII art or submit a jpg or pdf file.)


;Exercise 3. 
;SICP 3.13
;Draw the box-pointer diagram of z
(define (last-pair x)(car x))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;What happens if we try to compute (last-pair z)?
;;会出现死循环，因为其不停在循环引用自己的结果。

;SICP 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
  
;What does mystery do in general?


(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;Draw the box-pointer diagram of v before the call to mystery, 
;v after the call to mystery, and w
;;将列表反转

;What would be printed as the values of v and w?
;; v (list a b c d)
;; w (list d c b a)


;Exercise 4.
;SICP 3.16 Draw the 4 box-and-pointer diagrams.
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define count-3 (list 'a 'b 'c))
(count-pairs count-3);;3

(define last-pair (list 'c))
(define count-4 (cons last-pair (cons 'b last-pair)))
(count-pairs count-4);;4

(define two-pairs (cons last-pair last-pair))
(define count-7 (cons two-pairs two-pairs))
(count-pairs count-7);;7

(define count-helper (list 'a 'b 'c))
(define count-no-return (append! count-helper count-helper))
(count-pairs count-no-return);;no-return
		 
 #|
a. Returns 3:

b. Returns 4:

c. Returns 7:

d. Never returns:

|#

;SICP 3.17 Write a correct version of count-pairs.
(define (count-pairs-n x)
  (let ((counted '()))
    (define (count-helper x)
      (if(or(not(pair? x))(memq x counted))
         0
         (begin
           (set! counted (cons x counted))
           (+(count-helper (car x))
             (count-helper (cdr x))
             1))))
    (count-helper x)))

(count-pairs-n count-4) ;;3
(count-pairs-n count-7) ;;3
(count-pairs-n count-no-return) ;;3


;SICP 3.21 Explain what Eva Lu Ator is talking about, and what happened with
;Ben's examples.  Then define print-queue.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
	  
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 
#| What happened with Ben's examples?




|#
; Implement the definition of print-queue
;Make sure you use display to print the queue.
(define (print-queue queue)
	(print (front-ptr queue)))


;SICP 3.25 Write lookup and insert!

(define (lookup-interval key-list curr-table)
      (if (and (not(null? key-list)) (not(null? curr-table))(pair? (cdr curr-table)))
          (let ((record-or-table (assoc (car key-list) (cdr curr-table))))
            (if record-or-table
                (if(null?(cdr key-list))
                   (cons record-or-table true)
                   (lookup-interval (cdr key-list)  record-or-table)) ;;如果key在table(即record)中才会递归
                (cons key-list curr-table)))
          (cons key-list curr-table)))

(define (lookup keys table)
	(let ((record (lookup-interval keys local-table)))
        (if (eq? (cdr record) true)
            (cdr(car record))
            false)))

(define (insert! keys value table)
	 (let ((records (lookup-interval key-list  table)))
       (let ((true-or-tables (cdr records))
             (record-or-keylist (car records)))
       (if (eq? true-or-tables true)
           (set-cdr! record-or-keylist value)
           (set-cdr! true-or-tables (cons (build-table record-or-keylist value) (cdr true-or-tables))))))
     'ok)

#|
SICP 3.27

Explain why the number of steps is proportional to n (you may want to
include a trace to explain).

Would it still work (efficiently) if we define memo-fib as (memoize
fib)?  Why or why not?
(memo-fib n)与(memo-fib (- n 1)相差一次查表的过程,故其时间复杂度为线性的,即为O(n)。

把memo-fib换成(memoize fib),并不能直接工作,因为fib中的递归过程并未记忆化,fib过程体需要转换(内部调用memoize过程返回新过程)

|#

;Exercise 5. Write vector-append.
(define (vector-set-loop new-v start end old-v fstart fend)
  (if (= fstart fend)
      (vector-set! new-v end (vector-ref old-v fend))
      (begin
        (vector-set! new-v start (vector-ref old-v fstart))
        (vector-set-loop new-v (+ start 1) end old-v (+ fstart 1) fend))))
(define (vector-append v1 v2)
     (cond ((= (vector-length v1)0)v2)
           ((= (vector-length v2)0)v1)
           (else
            (let ((new-vector (make-vector (+(vector-length v1)
                                          (vector-length v2)))))
              (vector-set-loop new-vector 0 (- (vector-length v1)
                                          1)
                               v1 0  (- (vector-length v1)
                                          1))
              (vector-set-loop new-vector (vector-length v1)
                               (- (vector-length new-vector)
                                          1)
                               v2 0  (- (vector-length v2)
                                          1))
              new-vector))))
              


;Exercise 6. Write vector-filter.
(define (vector-filter pred vec)
  (let ((v-length (vector-length vec)))
	(define (loop n new-v)
          (cond((= n v-length) new-v)
               ((pred (vector-ref vec n))
                (loop (+ n 1)
                 (vector-append new-v (vector
                                       (vector-ref vec n)))))
               (else
                (loop (+ n 1)
                               new-v))))
    (loop 0 (vector))))

(define x2 (vector-filter (lambda(x)
                            (> x 1))
                          v1))
x2
                               

;Exercise 7. Write bubble-sort!
(define (bubble-sort! vec)
  (define (interval-loop cur-p n)
    (if(= cur-p n)
       vec
       (let((cur-v (vector-ref vec cur-p))
            (next-v (vector-ref vec (+ cur-p 1))))
         (if (> cur-v next-v)
             (begin
               (vector-set! vec cur-p next-v)
               (vector-set! vec (+ cur-p 1) cur-v)))
         (interval-loop (+ cur-p 1) n))))
  (define (loop n)
          (cond((= n 1)vec)
               (else
                (interval-loop 0 n)
                (loop (- n 1)))))
  (loop (- (vector-length vec) 1))
)

(define v3 (vector 2 3 5 4 1 8 7))
(bubble-sort! v3)
v3

; The order of growth of the running time of bubble sort is Theta(??)
;;若初始文件是反序的,需要进行 n - 1 趟排序。
;;每趟排序要进行 n - i 次关键字的比较(1≤i≤n-1),
;;且每次比较都必须移动记录三次来达到交换记录位置。
;;在这种情况下,比较和移动次数均达到最大值:
;;即3n(n-1)/2。
;;如果完全是正序的，则需要比较n(n-1)/2次。
;;所以平均时间复杂度为O(n^2)

