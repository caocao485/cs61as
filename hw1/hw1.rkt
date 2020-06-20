#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  ; Your code here
  (if(empty? sent)
     '()
     (if(member? (first sent) (bf sent))
        (dupls-removed (bf sent))
        (se (first sent)(dupls-removed (bf sent)))))
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
  ; Your code here
  (if(empty? sent)
     0
     (if(equal? (first sent) wd)
        (+ 1 (count-word (bf sent) wd))
        (count-word (bf sent) wd)))
)

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here
inifinite loop with invoking pigl
|#

; Exercise 4 - Define squares

(define (squares sent)
  ; Your code here
  (if (empty? sent)
      '()
      (se (square (first sent))(squares (bf sent))))
)

; Exercise 5 - Define switch

(define (switch sent)
  ; Your code here
  (define (switch-helper sen)
    (cond((empty? sen)'())
         ((equal? (first sen) 'you)
          (se 'me (switch-helper (bf sen))))
         ((or (equal? (first sen) 'me)
              (equal? (first sen) 'I))
          (se 'you (switch-helper (bf sen))))
         (else (se (first sen)
                   (switch-helper (bf sen))))))
  (cond((equal? (first sent)'you)(se 'I (switch-helper (bf sent))))
       ((equal? (first sent) 'I)(se 'you (switch-helper (bf sent))))
       (else (se (first sent) (switch-helper (bf sent)))))
)

; Exercise 6 - Define ordered?

(define (ordered? sent)
  ; Your code here
  (if(empty? (bf sent))
     #t
     (and(< (first sent)(first (bf sent)))
         (ordered? (bf sent))))
)

; Exercise 7 - Define ends-e

(define (ends-e sent)
  ; Your code here
  (if (empty? sent)
      '()
      (if(equal? (last (first sent)) 'e)
         (se (first sent)
             (ends-e (bf sent)))
         (ends-e (bf sent))))
  
)

; Exercise 8

(or (> 3 2) (/ 3 0)) ;;#t
(or  (/ 3 0) (> 3 2)) ;;; /: division by zero [,bt for context]
;;special form
#|

Your explanation here

if or is ordinary function ,it can be passed into a higher order function,
so it can be treated as a first class function

|#
