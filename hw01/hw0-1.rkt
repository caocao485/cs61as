#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
(require (planet dyoo/simply-scheme))

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

;; Exercise 0 - Introduce yourself

#|

This is a comment that spans multiple lines.

1) What is your name?

2) What is your major?

3) Are you a returning student? (i.e. Did you take 61AS last semester?)

4) What made you to take 61AS?

5) Tell us interesting things about yourself.

|#

;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))
(define (sum-of-squares x y)(+ (square x)(square y)))
(sum-of-squares 3 4)


;;interclude
;;first takes a word and returns the first letter of that word, or it takes
;;a sentence and returns the first word of that sentence.

;;butfirst (or bf) takes a word/sentence and returns everything but the first
;;letter/word.

;;if 特殊形式
;;(if(= 5 (+ 2 3)) 'yay! (/ 1 0)) ;;'yay!

;;cond
;; (cond ((= 3 1)'wrong!)
;;        ((= 3 2)'still-wrong!)
;;        (else 'yay))
;; 'yay

;;and
;;(and (> 5 3)(< 2 4)) ;;#t
;;(and (> 5 3)(< 2 1)) ;;#f

;;or

;;first
;;butfirst

;; Exercise 2a - Define can-drive
(define (can-drive age)
  (if(>= age 16)
     '(Good to go)
     '(Not yet)))


;; Exercise 2b - Define fizzbuzz
(define (fizzbuzz number)
  (cond ((and(= (remainder number 5)0)
            (= (remainder number 3)0))
        'fizzbuzz)
        ((= (remainder number 3)0)'fizz)
        ((= (remainder number 5)0)'buzz)
        (else number)))

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here

;;没有题目
|#

;; Exercise 4 - new-if vs if

#|
Your answer here

;;because new-if make the arguments invoked, so it will (infinite-loop) and cause
;;"dvided by zero" error

|#


;;note
#|
3         self-evaluating
(+ 2 3)   function notation
(sqrt 16) name don't have to be punctuation
(+ (* 3 4)5) composition of functions

+         functions are things in themselves
'+        quoting
'hello    can quote any word
'(+ 2 3)  can quote any expression
'(good morning)   even non-expression sentences

(first 274) functions don't have to be arithmetic
(butfirst 274) (abbreviation bf)
(first 'hello) works for non-numbers
(first hello) remainder about quoting
(first (bf 'hello)) composition of non-numeric functions
(+ (first 23)(last 45)) combining numeric and non-numeric

(define pi 3.1415926) speical form
pi value of a symbol
'pi constrast with quoted symbol
(+ pi 7) symbols work in larger expressions

(define (square x)
   (* x x)) defining a function
(square 5) invoking the function
(square (+ 2 3)) composition with defined functions

formal parameter is the name of the argument
actual argument  expression used in the invocation (+ 2 3)
the actual argument value is the value of the invocation 

|#

(bf '(computer science)) ;;'(science)
(count (bf '(computer science))) ;;1
(first (bf '(computer science))) ;;'science
(count (first (bf '(computer science))))  ;;7 字面值的长度为7
