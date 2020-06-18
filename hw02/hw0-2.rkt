#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10
;2. Compound Expression (3 Atoms)
(+ 5 5)
;3. Compound Expression (4 Atoms)
(+ 8 1 1)
;4. Compound Expression (1 Atom and 2 subexpressions)
(+ (+ 2 2)(+ 3 3))
;5. Any Other Kind Expression
(+ 4 (+ 3 3))

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  ; your code here
  (word (first wd)(second wd))
)

;;2. Define two-first
(define (two-first x y)
  (word (first x)(first y))
  ; your code here
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word (first(first sent))(first(second sent)))
  ; your code here
  
)

;Exercise 2 - Define teen?
(define (teen? num)
  ; your code here
  (and(<= num 19)(>= num 13))
)

;Exercise 3 - Define indef-article
(define (indef-article wd)
  ; your code here
  (if(member? (first wd) '(a e i o u))
     (sentence 'an wd)
     (sentence 'a wd))
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  ; your code here
  (sentence (bl sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  ; your code here
  (sentence (second sent)
            (first sent)
            (butfirst (butfirst(butlast sent)))
            (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  ; your code here
  (if(equal? (second time) 'pm)
         (if(= (first time)12)
            12
            (+ (first time) 12))
         (if(= (first time)12)
            0
            (first time)))
     
)

(define (american-time time)
  ; your code here
  (if(> time 12)
         (sentence (- time 12) 'pm)
         (if(= time 12)
            (sentence 12 'pm)
            (if(= time 0)
               (sentence 12 'am)
               (sentence time 'am))))
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  ; your code here
  (cond((> secs 86400)(sentence (/ secs 86400) 'days))
    ((> secs 3600)(sentence (/ secs 3600) 'hours))
       ((> secs 60)(sentence (/ secs 60) 'minutes))
       (else (sentence secs 'seconds)))
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

Explanation here.
word是关键字，不能被当成参数名
|#