;;;;METACIRCULAR EVALUATOR THAT SEPARATES ANALYSIS FROM EXECUTION
;;;; FROM SECTION 4.1.7 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator of
;;;;  sections 4.1.1-4.1.4, since it uses the expression representation,
;;;;  environment representation, etc.
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.

;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.1.7 definition
;; of eval overrides the definition from 4.1.1
;; modified 8/2/2000 by jeremy to add path 

(load "~cs61as/lib/mceval.scm")


;;; Exercise 4.22
;;; In the ANALYZE procedure below, extend the evaluator in this section to support the special form let

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp)(analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;; Exercise 4.23
#|
(define (analyze-sequence exps)
    (define (execute-sequence procs env)
          (cond ((null? (cdr procs)) ((car procs) env))
		          (else ((car procs) env)
				                (execute-sequence (cdr procs) env))))
      (let ((procs (map analyze exps)))
	    (if (null? procs)
		        (error "Empty sequence -- ANALYZE"))
	        (lambda (env) (execute-sequence procs env))))
|#


;;; Compare the two versions of analyze-sequence. For example, consider the common case (typical of procedure bodies) where the sequence has just one expression.
;;; What work will the execution procedure produced by Alyssa's program do?

#| Your answer Here:

Alysss的程序每次依旧会做null谓词检测,相当于分析阶段并没有做到优化和复用。因为主体被包裹在lambda中。
而原程序在loop计算时就会被展开，从而减少重复。

|#


;;; What about the execution procedure produced by the program in the text above? How do the two versions compare for a sequence with two expressions?
#| Your answer here:



|#


;;; Exercise 4.27
;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-27a 1)

;;; L-Eval input:
;;; w
;;; L-Eval value:
(define answer4-27b 10)

;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-27c 2)



;;; Exercise 4.29

;;; MEMOIZED VERSION:

;;; L-Eval input:
;;; (square (id 10))
;;; L-Eval value:
(define answer4-29a 100)

;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-29b 1)

;;; UNEMEMOIZED VERSION

;;; L-Eval input:
;;; (square (id 10))
;;; L-Eval value:
(define answer4-29c 100)

;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-29d 2)




;;; Exercise 4.25
#|
Suppose that (in ordinary applicative-order Scheme) we define unless as shown above and then define factorial in terms of unless as

(define (factorial n)
    (unless (= n 1)
	              (* n (factorial (- n 1)))
		                1))

What happens if we attempt to evaluate (factorial 5)? Will our definitions work in a normal-order language?

计算(factorial 5)时会出现死循环,因为是应用序,usual-value的实参值会被要求计算出来,这就会产生递归调用求出factorial(- n 1),然后继续调用,然后跳过n=1的情况,n递减的情况下一直递归调用factorial。

在正则序中,总是会先调用(= n 1)表达式,然后选择性地递归调用,就不会出现死循环。



|#



;;; Exercise 4.26

#| Your answer here:


将unless转换为if表达式即可,这是个特殊形式。

出错“Unbound variable unless”。如果是特殊形式,在求值环境中则获取不了与unless相关的定义,这就会报错。所以需要实现为一个过程才能被高阶过程所用。
if、cond等特殊形式都有这样的问题,即它们不是第一类元素。

|#





;;; Execise 4.28
;;; Eval uses actual-value rather than eval to evaluate the operator before passing it to apply, in order to force the value of the operator.
;;; Give an example that demonstrates the need for this forcing.


#| Your example here

因为apply需要被实际应用的那个过程,以便根据其类型去分派并应用它。 如果得到的是thunk,就无法判断其类型是(primitive。还是procedure),导致错误。


|#



;;; Exercise 4.30
;;; Part a
#| Your answer here
lambda作为过程应用的运算符,会被强制求值;car作为基本过程,其参数也会被强制求值。所以该求值器能正确处理表达式。


|#
;;; Part b

(define p1-original '(1 2))
(define p2-original 1)

(define p1-changed '(1 2))
(define p2-changed '(1 2))

;;; Part c
#| Your answer here

实际的效果和a一样,所以这个说法是正确的。

|#


;;; Part d
#| Your answer here

采用cy的方法,可预测性更强一些,

|#



;;; Exercise 4.32
#| Your answer here
在流模型中，car是没有惰性求值的，这里的car是可以的，这样就可以在构建诸如树形结构时，树的所有分支都是惰性求值的，提高效率。 


|#


;;; Exercise 4.33
;;; You can find the code for LAZY Evaluator below the code for analyzing evaluator
;;;
;;; To avoid overwriting analyzing's lazy-eval with lazy's, we have the procedures SETUP-ANALYZE and SETUP-LAZY that should be called before  calling (mce)
;;; If you want to make a change to Lazy's mc-eval, please change setup-lazy instead

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the ANALYZING evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;安装三个过程到primitive-procedures过程的列表中
(define (lazy-cons x y)(lambda(m)(m x y)))
(define (lazy-car z)(z (lambda(p q)p)))
(define (lazy-cdr z)(z (lambda(p q)q)))

(define (make-cons x y)
  (if(null? y)
     (lazy-cons x y)
     (lazy-cons x
               (make-cons (car y)
                          (cdr y)))))
(define (pair-or-text-quotation exp)
  (if(pair? (cadr exp))
     (make-cons (caadr exp)(cdadr exp))
     (cadr exp)))

(define (mc-eval exp env)
  ((analyze exp) env))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;; Added at Berkeley:

(define input-prompt ";;; A-Eval input:")
(define output-prompt ";;; A-Eval value:")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Below is the LAZY evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To avoid overwriting analyzing's lazy-eval with lazy's, we have SETUP-ANALYZE and SETUP-LAZY that should be called before  (mce)
(define (setup-analyze)
  (set! input-prompt ";;; A-Eval input:")
  (set! output-prompt ";;; A-Eval value:")  
  (set! mc-eval (lambda (exp env)
	((analyze exp) env))))

(define (setup-lazy)
  (set! input-prompt ";;; L-Eval input:")
  (set! output-prompt ";;; L-Eval value:")
  (set! mc-eval (lambda (exp env)
		  (cond ((self-evaluating? exp) exp)
			((variable? exp) (lookup-variable-value exp env))
			((quoted? exp) (pair-or-text-quotation exp) )
			((assignment? exp) (eval-assignment exp env))
			((definition? exp) (eval-definition exp env))
			((if? exp) (eval-if exp env))
			((lambda? exp)
			 (make-procedure (lambda-parameters exp)
					 (lambda-body exp)
					 env))
			((begin? exp) 
			 (eval-sequence (begin-actions exp) env))
			((cond? exp) (mc-eval (cond->if exp) env))
			((application? exp)             ; clause from book
			 (mc-apply (actual-value (operator exp) env)
				   (operands exp)
				   env))
			(else
			 (error "Unknown expression type -- EVAL" exp))))))

(define (actual-value exp env)
  (force-it (mc-eval exp env)))

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))



;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
  ;;; Used lazy list implementation (list is not defined this way yet)
  (list (list 'car (lambda (p) (p (lambda ( x y) x))))
        (list 'cdr (lambda (p) (p (lambda (x y) y))))
        (list 'cons (lambda (x y) (lambda (m) (m x y))))
	(list 'null? null?)
        (list 'list list)
	(list 'number? number?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

'LAZY-EVALUATOR-LOADED
