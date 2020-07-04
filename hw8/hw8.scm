; SICP 3.3, 3.4 - Modify make-password-account to make it generate
; password-protected accounts.
; Also, if an incorrect password is given 7 times consecutively, you
; should say (call-the-cops).
; Note: In the case of a wrong password, you should return the string
; "Incorrect password".  Do not use display or print or error.

(define (make-password-account   balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (call-the-cops)
    (error "call the cops"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define dispatch
    (let ((error-times 0))
    (lambda(p m)
      (if(equal? p password)
         (begin
           (set! error-times 0)
           (cond
             ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             ((eq? m 'correct-password)#t)
             (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
         (begin
           (if(> error-times 7)
              (call-the-cops)
              (begin (set! error-times (+ error-times 1))
                     (display error-times)
                     (error "Incorrect password"))))))))
  dispatch)

; SICP 3.7 - Define make-joint.
; You may want to modify make-password-account, but you shouldn't
; remove any of its previous functionality.
(define (make-joint prev-acc prev-password new-password)
  (if(prev-acc prev-password 'correct-password)
     (lambda(given-password mode)
       (if (eq? given-password new-password)
           (prev-acc prev-password mode)
           (error "Incorrect password")))))


; SICP 3.8 - Define reset-f!
; This is a function that defines the function f that the exercise
; asks for.

(define f #f)

(define (reset-f!)
  (set! f
        (let ((count 0))
          (lambda(value)
            (set! count (+ count 1))
            (if(and(= value 1)(odd? count))
               1
               0)))))

; For example, if you think that f should be the square function, you
; would say:
; (define (reset-f!)
;   (set! f (lambda (x) (* x x))))

; SICP 3.10 - Answer in the comment block.
(define (make-withdraw initial-amount)
  (let((balance initial-amount))
    (lambda(amount)
      (if(>= balance amount)
         (begin
           (set! balance (- balance amount))
           balance)
         "Insufficient funds"))))
(define (make-withdraw-b balance)
  (lambda(amount)
    (if(>= balance amount)
       (begin
         (set! balance (- balance amount))
         balance)
       "Insufficient funds")))
#|
You have two options:
1) Draw the environment diagram here using ASCII art
2) Submit a .jpg or .pdf file containing your environment diagram (for
example, you could scan a paper drawing), in which case you should
write the filename here.

Environment diagram here

Q. How do the environment structures differ for the two versions?
A. 
let版本多了一套环境

Q. Show that the two versions of make-withdraw create objects with the
same behavior.
A.
let内部所创建的环境和没有let版本是相似的。


|#

; SICP 3.11 - Answer in the comment block.
#|
Same options as in 3.10

Environment diagram here

Q. Where is the local state for acc kept?
A.

Q. How are the local states for the two accounts kept distinct?
A.

Q. Which parts of the environment structure are shared?
A. 

|#
