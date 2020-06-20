#lang racket

(require berkeley)
(provide (all-defined-out))


(define (second wd)
  (first (bf wd)))
; Exercise 1 - Define describe-time
(define (describe-time secs) ;;every pattern
  ; your code here
 (cond((> secs 86400)(sentence (quotient secs 86400) 'days
                               (describe-time (remainder secs 86400))))
    ((> secs 3600)(sentence (quotient secs 3600) 'hours
                            (describe-time (remainder secs 3600))))
       ((> secs 60)(sentence (quotient secs 60) 'minutes
                             (describe-time (remainder secs 60))))
       (else (sentence secs 'seconds)))
)

; Exercise 2 - Define remove-once
(define (remove-once wd sent) ;;every patern two base case
  ; your code here
 (if(empty? sent)
    '()
    (if(equal? (first sent) wd)
       (bf sent)
       (sentence (first sent)
                 (remove-once  wd(bf sent)))))
)

; Exercise 3 - Define differences
(define (differences nums) ;;every pattern
  ;your code here
 (if(empty?(bf nums))
    '()
    (sentence (- (second nums)
                 (first nums))
              (differences (bf nums))))
)

; Exercise 4 - Define location
(define (location small big) ;;keep pattern
  ; your code here
  (define (iter acc bg)
    (cond((empty? bg)#f)
         ((equal? (first bg) small) (+ acc 1))
         (else (iter (+ acc 1)(bf bg)))))
 (iter 0 big)
)

; Exercise 5 - Define initials
(define (initials sent) ;;every pattern
  ; your code here
 (if (empty? sent)
     '()
     (sentence (first(first sent))
               (initials (bf sent))))
)

; Exercise 6 - Define copies
(define (copies num wd) ;;every pattern
  ; your code here
 (if(= num 0)
    '()
    (sentence wd (copies (- num 1) wd)))
)

; Exercise 7 - Define gpa
(define (gpa grades) ;;every
  ; your code here
  (define (iter scores rest-grades)
    (if(empty? rest-grades)
       scores
       (iter (+ scores
                (+ (base-grade (first rest-grades))
                   (grade-modifier (first rest-grades))))
             (bf rest-grades))))
 (/ (iter 0 grades) (length grades))
)

(define (base-grade grade)
  (cond((equal? (first grade) 'A)4)
       ((equal? (first grade) 'B)3)
       ((equal? (first grade) 'C)2)
       ((equal? (first grade) 'D)1)
       (else 0)))
(define (grade-modifier grade)
  (cond((equal? (bf grade) '+) .33)
       ((equal? (bf grade) '-) -.33)
       (else 0)))

; Exercise 8 - Define repeat-words
(define (repeat-words sent) ;;相互递归
  ; your code here
  (define (repeats number word rsent)
    (if(= number 0)
       rsent
       (sentence word
                 (repeats (- number 1)
                          word
                          rsent))))
  (if(empty? sent)
     '()
     (if(number? (first sent))
        (repeats (first sent) (second sent) (repeat-words
                                             (bf(bf sent))))
        (sentence (first sent) (repeat-words (bf sent)))))
)

; Exercise 9 - Define same-shape?
 (define (snp s0 s1)
    (if(empty? (bf s0))
       (and (= (count (first s0))
           (count (first s1))))
    (and(= (count (first s0))
           (count (first s1)))
        (snp (bf s0)(bf s1)))))
(define (same-shape? sent1 sent2)
  ; your code here

 (and(= (count sent1)
        (count sent2))
     (snp sent1 sent2))
)

(define (vowel? letter) (member? letter '(a e i o u)))
(define (no-vowel? wd)
  (if(and(empty?(bf wd))
         (not(vowel? (first wd))))
     #t
     (and(not(vowel? (first wd)))
         (no-vowel? (bf wd)))))


(define (pigl wd)
  (cond((no-vowel? wd)wd)
       ((vowel? (first wd))(word wd 'ay))
       (else(pigl (word (bf wd)(first wd))))))

(pigl 'hello) ; ellohay 
(pigl 'open) ; openay 
(pigl 'scheme) ; emeschay
(pigl 'my) ; my