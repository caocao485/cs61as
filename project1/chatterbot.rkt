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
    (error "not yet implemented")
  )


;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    ;;insert your answer here
    (error "not yet implemented")
  )
  
;;Q7 - eliza
  (define (eliza sent)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q10 - exagerate
  (define (exaggerate bot n)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
