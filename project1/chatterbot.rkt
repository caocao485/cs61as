#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt") 

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent)
    ;;insert your answer here
    sent
  )

;;Q2 - stupidbot-creator
  (define (stupidbot-creator motto)
    ;;insert your answer here
    (lambda(sent)
      motto)
  )


;;Q3 - matcherbot-creator
(define (match-every pattern sent)
  (cond((empty? pattern)sent)
       ((> (count pattern)(count sent)) false)
       ((not(equal? (first pattern)(first sent)))
        false)
       (else(match-every (bf pattern) (bf sent)))))


  (define (matcherbot-creator pattern)
    ;;insert your answer here
    (define (helper sent)
      (cond ((empty? pattern) sent)
            ((> (count pattern)(count sent)) false)
            ((equal? (first pattern)
                     (first sent))
             (let((tryed (match-every pattern sent)))
               (if(not(boolean? tryed ))
                  tryed
                  (helper (bf sent)))))
            (else
             (helper(bf sent)))))
    helper
             
  )

(define (get-member-num member sent)
  (cond((equal? member (first sent))1)
       (else(+ 1 (get-member-num member (bf sent))))))

;;Q4 - substitutebot-creator
  (define (substitutebot-creator from to)
    ;;insert your answer here
    (define (helper sent)
      (cond((empty? from)sent)
           ((empty? sent)'())
           ((member? (first sent) from)
            (se (item (get-member-num (first sent) from)
                      to)
                (helper(bf sent))))
           (else
            (se (first sent)
                (helper (bf sent))))))
    helper
  )



;;Q5 - switcherbot
  (define (switcherbot sent)
    ;;insert your answer here
   (define (switch-helper sen)
    (cond((empty? sen)'())
         (else
          (let ((fwd (first sen))
                (bfsen (bf sen)))
         (cond((equal? fwd 'you)
          (se 'me (switch-helper bfsen)))
         ((or (equal? fwd 'me)
              (equal? fwd 'I))
          (se 'you (switch-helper bfsen)))
         ((equal? fwd 'am)
          (se 'are (switch-helper bfsen)))
         ((equal? fwd 'are)
          (se 'am (switch-helper bfsen)))
         ((equal? fwd 'was)
          (se 'were (switch-helper bfsen)))
         ((equal? fwd 'were)
          (se 'was (switch-helper bfsen)))
         ((equal? fwd 'my)
          (se 'your (switch-helper bfsen)))
         ((equal? fwd 'your)
          (se 'my (switch-helper bfsen)))
         ((equal? fwd 'yours)
          (se 'mine (switch-helper bfsen)))
         ((equal? fwd 'mine)
          (se 'yours (switch-helper bfsen)))
         (else (se fwd
                   (switch-helper bfsen))))))))
  (cond((equal? (first sent)'you)(se 'I (switch-helper (bf sent))))
       ((equal? (first sent) 'I)(se 'you (switch-helper (bf sent))))
       (else (se (first sent) (switch-helper (bf sent)))))
  )


;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    ;;insert your answer here
    (define (switch-helper sen)
    (cond((empty? sen)'?)
         ((equal? (first sen) 'I)(se 'you (switch-helper (bf sen))))
         ((equal? (first sen) 'am)
          (se 'are (switch-helper (bf sen))))
         ((equal? (first sen) 'you)
          (se 'me (switch-helper (bf sen))))
         ((or (equal? (first sen) 'me)
              (equal? (first sen) 'I))
          (se 'you (switch-helper (bf sen))))
         (else (se (first sen)
                   (switch-helper (bf sen))))))
    (switch-helper sent)
  )
  
;;Q7 - eliza
  (define (eliza sent)
    ;;insert your answer here
   (define (switch-helper sen)
      (cond((empty? sen)'?)
           (else
          (let ((fwd (first sen))
                (bfsen (bf sen)))
         (cond((equal? fwd 'you)
          (se 'me (switch-helper bfsen)))
         ((or (equal? fwd 'me)
              (equal? fwd 'I))
          (se 'you (switch-helper bfsen)))
         ((equal? fwd 'am)
          (se 'are (switch-helper bfsen)))
         ((equal? fwd 'are)
          (se 'am (switch-helper bfsen)))
         ((equal? fwd 'was)
          (se 'were (switch-helper bfsen)))
         ((equal? fwd 'were)
          (se 'was (switch-helper bfsen)))
         ((equal? fwd 'my)
          (se 'your (switch-helper bfsen)))
         ((equal? fwd 'your)
          (se 'my (switch-helper bfsen)))
         ((equal? fwd 'yours)
          (se 'mine (switch-helper bfsen)))
         ((equal? fwd 'mine)
          (se 'yours (switch-helper bfsen)))
         (else (se fwd
                   (switch-helper bfsen))))))))
      
    (cond((empty? sent)
          '(how can I help you ?))
      ((equal?(first sent) 'hello) '(hello there!))
         ((and(equal? (first sent) 'I)
              (equal? (first (bf sent))'am))
          (se 'why 'are 'you (switch-helper (bf(bf sent)))))
         ((equal? (last sent) '?)
          '(I can not answer your question.))
         
         (else
          (switcherbot sent)))
  )

;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    ;;insert your answer here
    (lambda(sent)
      (cond((equal? pat sent)out)
           (else (bot sent))))
  )

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    ;;insert your answer here
    (lambda(sent)
      (let ((result ((matcherbot-creator pat)sent)))
        (if(not(boolean? result))
           (se before result after)
           (bot sent))))
  )

;;Q10 - exagerate
  (define (exaggerate bot n)
    ;;insert your answer here
    (define (filter-adj sent)
      (filter (lambda(wd)
                (adjective? wd))
              sent))
    (define (repeatvery num)
      (cond((= num 0) '())
           (else (se 'very (repeatvery (- num 1))))))
    
    (define (addvery sen)
      (cond((empty? sen)'())
           ((adjective? (first sen))
            (se (repeatvery n) (first sen) (addvery (bf sen))))
           (else (se (first sen) (addvery (bf sen))))))
    (lambda(sent)
      (if(empty? (filter-adj sent))
         (bot sent)
          (addvery sent)))
  )

;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
