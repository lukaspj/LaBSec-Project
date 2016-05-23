;;; week-4_a-syntax-checker-for-Scheme.scm
;;; dProgSprog 2015-2016, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 03 May 2016

;;; The raw Scheme code from
;;;   http://users-cs.au.dk/danvy/dProgSprog16/Lecture-notes/week-4_a-syntax-checker-for-Scheme.html

(load "checker-aux.scm")
(load "label-model.scm")

;;;;;;;;;;
;;; Utilities
;;;;;;;;;;

;;;;;;;;;;

(define check-silently
  #f)

;;;;;;;;;;

(define check-program
  (lambda (v pc env)
    (cond
      [(null? v)
       env]
      [(pair? v)
       (check-program (cdr v)
		      pc
		      (check-toplevel-form (car v) pc env))]
      [else
       (begin
         (unless check-silently
           (printf "check-program -- unrecognized input: ~s~n" v))
         #f)])))

;;;;;;;;;;

(define check-toplevel-form
  (lambda (v pc env)
    (cond
      [(is-definition? v)
       (check-definition (define-1 v) (define-2 v) pc env)]
      [else
       (check-expression v pc env)
       env])))

;;;;;;;;;;


;;;;;;;;;;
;;; the syntax checker proper for definitions:
;;;;;;;;;;

(define check-definition
  (lambda (name definiens pc env)
    (let ([label (check-expression definiens pc env)])
      (alist-extend name label env))))

;;;;;;;;;;


;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;

(define check-expression
  (lambda (v pc env)
    (cond
      [(is-number? v)
       (check-number v pc env)]
      [(is-boolean? v)
       (check-boolean v pc env)]
      [(is-character? v)
       (check-character v pc env)]
      [(is-string? v)
       (check-string v pc env)]
      [(is-variable? v)
       (check-variable v pc env)]

      ;;; IFC
      [(is-label? v)
       (check-label-expression (label-1 v) (label-2 v) pc env)]
      ;;;
      
      [(is-time? v)
       (check-time-expression (time-1 v) pc env)]
      [(is-if? v)
       (check-if-expression (if-1 v) (if-2 v) (if-3 v) pc env)]
      [(is-and? v)
       (check-and-expression (cdr v) pc env)]
      [(is-or? v)
       (check-or-expression (cdr v) pc env)]
      [(is-cond? v)
       (check-cond-expression (cdr v) pc env)]
      [(is-case? v)
       (check-case-expression (cdr v) pc env)]
      [(is-let? v)
       (check-let-expression (let-1 v) (let-2 v) pc env)]
      [(is-letstar? v)
       (check-let-expression (letstar-1 v) (letstar-2 v) pc env)]
      [(is-letrec? v)
       (check-letrec-expression (letrec-1 v) (letrec-2 v) pc env)]
      [(is-begin? v)
       (check-expressions (cdr v) pc env)]
      [(is-unless? v)
       (check-unless-expression (unless-1 v) (unless-2 v) pc env)]
      [(is-quote? v)
       (check-quote-expression (quote-1 v) pc env)]
      [(is-lambda? v)
       (check-lambda (lambda-1 v) (lambda-2 v) pc env)]
      [(is-trace-lambda? v)
       (check-trace-lambda (trace-lambda-1 v) (trace-lambda-2 v) (trace-lambda-3 v) pc env)]
      [(is-application? v)
       (check-application (application-operator v) (application-operands v) pc env)]
      [else
       (begin
         (unless check-silently
           (printf "check-expression -- unrecognized input: ~s~n" v))
         #f)])))

(define check-expressions
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-expression (car v) pc env)
	   (check-expressions (cdr v) pc env))])))


(define check-variable
  (lambda (v pc env)
    (alist-lookup
     v
     env
     (lambda(label)
       label)
       ;; (if (label-flows-to pc label)
       ;; 	   pc
       ;; 	   (errorf 'check-variable "Mismatched pc ~s for ~s with label ~s~n" pc v label)))
     (lambda(v) pc))))

(define check-variable*
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v) ;;TODO should be union
      (check-variable (car v) pc env)
      (check-variable* (cdr v) pc env)]
     [else
      (check-variable v pc env)])))

;;; IFC

(define check-label-expression
  (lambda (label expression pc env)
    (let ([l (eval label)]
          [exp-label (check-expression expression pc env)])
      (if (label-flows-to exp-label l)
	  l
	  (errorf 'check-label-expression
		  "Mismatched labels ~s -> ~s~nFor expression: ~s~n"
		  exp-label
		  l
		  expression)))))

;;;

(define check-time-expression
  (lambda (v pc env)
    (check-expression v pc env)))

(define check-if-expression
  (lambda (test consequent alternative pc env)
    (and (check-expression test pc env)
         (check-expression consequent pc env)
         (check-expression alternative pc env))))

(define check-and-expression
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-expression (car v) pc env)
	   (check-and-expression (cdr v) pc env))])))  

(define check-or-expression
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-expression (car v) pc env)
	   (check-or-expression (cdr v) pc env))])))

(define check-cond-expression
  (lambda (v pc env)
    (check-cond-clauses v pc env)))

(define check-cond-clauses
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(is-else? (car v))
      (check-expression (cdar v) pc env)]
     [(pair? v)
      (and (check-cond-clause (car v) pc env)
	   (check-cond-clauses (cdr v) pc env))])))
  

(define check-cond-clause
  (lambda (v pc env)
    (if (proper-list-of-given-length? v 1)
	(check-expression v pc env)
	(if (equal? (cadr v) '=>)
	    (and (check-expression (cdr v) pc env)
		 (check-expression (caddr v) pc env))
	    (and (check-expression (car v) pc env)
		 (check-expression (cadr v) pc env))))))

(define check-case-expression
  (lambda (v pc env)
    (and (check-expression (car v) pc env)
	 (check-case-clauses (cdr v) pc env))))

(define check-case-clauses
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(is-else? (car v))
      (check-expression (cdar v) pc env)]
     [(pair? v)
      (and (check-case-clause (car v) pc env)
	   (check-case-clauses (cdr v) pc env))])))

(define check-case-clause
  (lambda (v pc env)
    (and (check-quotations (car v) pc env)
	 (check-expression (cdr v) pc env))))

(define check-let-expression
  (lambda (bindings body pc env)
    (and (check-let-bindings bindings pc env)
	 (check-expression body pc env))))

(define check-let-bindings
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (check-let-binding (car v) pc env)
      (check-let-bindings (cdr v) pc env)])))

(define check-let-binding
  (lambda (v pc env)
    (and (pair? v)
	 (check-variable (car v) pc env)
	 (check-expression (cdr v) pc env))))

(define check-letrec-expression
  (lambda (bindings body pc env)
    (and (check-letrec-bindings bindings pc env)
	 (check-expression body pc env))))

(define check-letrec-bindings
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (check-letrec-binding (car v) pc env)
      (check-letrec-bindings (cdr v) pc env)])))

(define check-letrec-binding
  (lambda (v pc env)
    (and (pair? v)
	 (check-variable (car v) pc env)
	 (cond
	  [(is-lambda? (cdr v))
	   (check-lambda (cdr v) pc env)]
	  [(is-trace-lambda? (cdr v))
	   (check-trace-lambda (cdr v) pc env)]))))

(define check-unless-expression
  (lambda (test consequent pc env)
    (and (check-expression test pc env)
         (check-expression consequent pc env))))

(define check-quote-expression
  (lambda (v pc env)
    (check-quotation v pc env)))

(define check-quotations
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-quotation (car v) pc env)
	   (check-quotations (cdr v) pc env))])))

(define check-quotation
  (lambda (v pc env)
    (cond
      [(pair? v)
       (and (check-quotation (car v) pc env)
	    (check-quotation (cdr v) pc env))]
      [(number? v)
       pc]
      [(boolean? v)
       pc]
      [(char? v)
       pc]
      [(string? v)
       pc]
      [(symbol? v)
       pc]
      [(null? v)
       pc]
      [else
       #f])))

(define check-lambda
  (lambda (formals expression pc env)
    (and (check-lambda-formals formals pc env)
	 (check-expression expression pc env))))

(define check-trace-lambda
  (lambda (name formals expression pc env)
    (and (symbol? name)
	 (check-lambda-formals formals pc env)
	 (check-expression expression pc env))))

(define check-lambda-formals
  (lambda (v pc env)
    (cond
     [(is-variable? v)
      (check-variable v pc env)]
     [(list? v)
      (check-variable* v pc env)])))

(define check-application
  (lambda (v vs pc env)
    (and (check-expression v pc env)
	 (check-expressions vs pc env))))

;;; end of week-4_a-syntax-checker-for-Scheme.scm

"week-4_a-syntax-checker-for-Scheme.scm"
