(load "library/obj.scm")

; 1 - Modify the person class.

(define-class (person name)
  (instance-vars (current '()))
  (method (repeat)
          current)
  (method (say stuff)
          (set! current stuff)
          stuff)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name))))


; 2 - Determine which definition works as intended.
; In particular, make sure the repeat method works.
; the first one
(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat))) )



#|
Definition number ?? works as intended.
Your explanation here.
|#


; 3 - Write the random-generator class.
(define-class (random-generator init-value)
  (instance-vars (count 0))
  (method (number)
          (set! count (+ count 1))
          (random init-value)))
              

; 4 - Write the coke-machine class.
; For compatibility with the autograder, make sure that you display
; error messages.  That means you should say:
; (display "Not enough money") and
; (display "Machine empty") when appropriate.

(define-class (coke-machine coke-numbers price)
  (instance-vars (cents 0))
  (method (deposit cent)
          (set! cents (+ cents cent)))
  (method (coke)
          (cond((< cents price)
                (display "Not enough money"))
               ((= coke-numbers 0)
                (display "Machine empty"))
               (else
                (let((change (- cents price)))
                  (set! cents 0)
                  (set! coke-numbers (- coke-numbers 1))
                  change))))
  (method (fill cnums)
          (set! coke-numbers (+ cnums coke-numbers))))

; 5 - Write the deck class.

(define ordered-deck
  (accumulate append '()
	      (map (lambda (suit)
		     (map (lambda (value) (word value suit))
			  '(A 2 3 4 5 6 7 8 9 10 J Q K)))
		   '(s d c h))))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
	(cons card (shuffle (remove card deck))) )))

(define-class (deck)
  (instance-vars (remain-deck '()))
  (initialize (set! remain-deck (shuffle ordered-deck)))
  (method (deal)
          (if(ask self 'empty?)
             '()
             (let ((top (car remain-deck)))
               (set! remain-deck (cdr remain-deck))
               top)))
  (method (empty?)
          (null? remain-deck)))

; 6 - Write the miss-manners class.

(define-class (person name office)
  (instance-vars (current-repeat '())
                 (current-office 'down)
                 (remain-office '(a b c d)))
  (method (repeat)
          current-repeat)
  (method (say stuff)
          (set! current-repeat stuff)
          stuff)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name)))
  (method (go dir) 
          (display '(BRIAN MOVED FROM BH-OFFICE TO SODA))))
(define-class (miss-manners person)
  (default-method
    (if(not(equal? message 'please))
       (display '(ERROR: NO METHOD GO))
       (ask person (car args) (cadr args)))))