#lang sicp

;; Lab 11 Template

;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)
(define (get  op type)
  (let ((record (assoc (cons op type) (cdr maked-table))))
    (if (not record)
        #f
        (cdr record))))

(define (put  op type  value)
  (let ((record (assoc (cons op type) (cdr maked-table))))
    (if (not record)
        (set-cdr! maked-table
                  (cons (cons (cons op type) value) (cdr maked-table)))
        (set-cdr! record value)))
  'ok)

(define maked-table
  (list '*table*))

;;;SECTION 4.1.1

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
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
        ((let?  exp) (mc-eval (let->combination exp) env))
	((application? exp)
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
	((boolean? exp) true)
	(else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (last-element lst) 
     (if (null? (cdr lst)) 
         (car lst) 
         (last-element (cdr lst)))) 
  
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (check-types preds vals base-env)
  (if(= (length preds)0)
     #t
     (if(not(mc-apply (mc-eval (car preds) base-env) (list (car vals))))
        (error "wrong argument type -- " (car vals))
        (check-types (cdr preds) (cdr vals) base-env))))
  

(define (extend-environment vars vals base-env)
  (if(pair? (car vars))
     (let ((preds (map car vars))
           (paras (map cadr vars)))
       (if (and(= (length vars) (length vals))
               (check-types preds vals base-env))
          (cons (make-frame paras vals) base-env)
          (if (< (length vars) (length vals))
              (error "Too many arguments supplied" vars vals)
              (error "Too few arguments supplied" vars vals))))
     (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals)))))
     

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'import
                      (list 'primitive
			    (lambda (name)
			      (define-variable! name
				                (list 'primitive (eval name))
				                the-global-environment)))
                      initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list 'list list)
	(list 'append append)
	(list 'equal? equal?)
	(list 'integer? integer?)
	(list 'number? number?)
	(list 'list? list?)
	(list 'pair? pair?)
	(list 'not not)
	(list 'list-ref list-ref)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
;;(define the-global-environment (setup-environment))
;;(driver-loop)

;; Added at Berkeley:
(define the-global-environment '())

(define (mce)
  (set! the-global-environment (setup-environment))
  (driver-loop))

; SICP 4.3
; Rewrite eval in data-directed style.
(define (install-package)
  (put 'op 'quote
       (lambda(quoted env)
         (text-of-quotation quoted)))
  (put 'op 'set! eval-assignment)
  (put 'op 'define eval-definition)
  (put 'op 'if eval-if)
  (put 'op 'lambda (lambda (exp env)  
                    (make-procedure
                     (lambda-parameters exp)
                     (lambda-body exp)
                     env)))
  (put 'op 'begin (lambda (exp env)  
                   (eval-sequence (begin-actions exp) env))) 
 (put 'op 'cond (lambda (exp env)  
                  (dispatch-eval (cond->if exp) env)))
  (put 'op 'let (lambda(exp env)
                  (dispatch-eval (let->combination exp) env)))
  (put 'op 'let* (lambda(exp env)
                  (dispatch-eval (let*->nested-lets exp) env)))
     'ok)

; 基于数据导向的设计方式,主要就是对每种类型加一个 tag 标示,
; 然后将不同类型的同一操作放入一个 table 中,
; 最后根据输入数据的类型来动态获取实际的操作

(define (expression-type exp)
  (car exp))
(define (expression-body exp)
  (cdr exp))

(install-package)
(define (dispatch-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'op (expression-type exp))
         (apply (get 'op (expression-type exp)) exp env))
        ((application? exp)
         (mc-apply (dispatch-eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


; SICP 4.6
; Write let->combination and install let in the original evaluator.
; You can also implement it for dispatch-eval.
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-body exp)
  (caddr exp))
(define (let-bindings exp)
  (cadr exp))
(define (accumulate op initial sequence)
  (if(null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))
(define (make-application procedure arguments) 
  (cons procedure arguments))
(define (get-var-exp-list let-sequenece)
  (accumulate (lambda(cvalue acclist)
                (list (cons (car cvalue) (car acclist))
                      (cons (cadr cvalue)(cadr acclist))))
              '(()())
              let-sequenece))

(define (let->combination exp)
  (let ((velist (get-var-exp-list(let-bindings exp))))
    (define (make-combination varlist body explist)
      (make-application
       (make-lambda varlist body)
       explist))
    (make-combination (car velist) (let-body exp) (cadr velist))))


; SICP 4.7 (*)
; Write let*->nested-lets and install let* in the original evaluator.
; You can also implement it for dispatch-eval.
(define (make-let bindings body)
  (list 'let  bindings body))
(define(let*? exp)
  (tagged-list? exp 'let*))
(define(expand-let-clauses bindings body)
  (if(null? bindings)
     body
     (make-let (list (car bindings))
               (expand-let-clauses (cdr bindings) body))))
(define (let*->nested-lets exp)
  (expand-let-clauses (let-bindings exp) (let-body exp)))
;;let*可以作为扩展表达式，不必使用非派生方式处理，因为eval的处理方式是递归的


; SICP 4.10 (*)
; Design and implement a new syntax for Scheme.
; After you have tested your code, comment it out to prevent it from
; interfering with the other questions.

#|
Your answer here (after you have tested it)
把set!改成set~
|#

; SICP 4.11 (*)
; Represent a frame as a list of bindings.  It may help to remember
; assoc and related procedures.
; Change the existing procedures (don't write new ones, apart from
; helper functions, if you need them)


; SICP 4.13
; Complete the specification of make-unbound! and justify your choice
; (with examples of its use).  Implement make-unbound!.
(define (make-unbound! var env0)
  (define (env-loop env)
    (define (scan vars vals) ;;frame形如:((a b c) . (1 2 3))
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var(car vars))
             (begin
               (set! vals (cdr vals))
               (set! vars (cdr vars))))
            (else (scan(cdr vars)(cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- MAKE-UNBOUND!" var)
        (let ((frame(first-frame env)))
          (scan (frame-variables frame)(frame-values frame)))))
  (env-loop env0))


; SICP 4.14
; Explain why Louis's map fails even though Eva's works.
#|
Your explanation here
这里的根本问题在于系统的map的procedure参数的数据结构
与我们的解释器的procedure的数据结构是不兼容的。
我们的解释器的procedure是由list加上tag来组合的,
系统的map要求未知。如果要保证能运行,
需要解释器的apply来处理procedure的应用,而非系统的apply来处理。
|#

; SICP 4.15
; Explain why it is impossible for the halts? procedure to exist.
#|
Your explanation here
著名的停机问题,是清晰给出的第一个不可计算的问题,也就是说,
是一个良好刻画的工作,却不能由一个计算过程完成。
利用反证法:
假定存在halts?,能对任何过程p和对象a判定是否p对a终止成立。
分情况讨论:

现假定halt?判断(try try)不能终止,即(try try)没有返回值;然而将(try try)展开,
发现过程应用返回了‘halted,就说明(try try)能终止。这与假定相互矛盾。

现假定halt?判断(try try)能终止,即(try try)有返回值;然而将(try try)展开,
却调用了(run-forever),这就说明(try try)并不能终止。这与假定相互矛盾。

综上,不存在这样的halts?。
|#

; Exercise 2 (*)
; Add type-checking abilities to the evaluator.


; Extra for experts
; Exercises 4.16 - 4.21
