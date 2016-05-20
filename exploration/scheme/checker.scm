;;; week-4_a-syntax-checker-for-Scheme.scm
;;; dProgSprog 2015-2016, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 03 May 2016

;;; The raw Scheme code from
;;;   http://users-cs.au.dk/danvy/dProgSprog16/Lecture-notes/week-4_a-syntax-checker-for-Scheme.html

;;;;;;;;;;
;;; Utilities
;;;;;;;;;;

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

(define proper-list-longer-than-given-length?
  (lambda (v n)
    (or (and (null? v)
             (<= n 0))
        (and (<= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-longer-than-given-length? (cdr v)
                                           (- n 1))))))

(define alist-mt
  '())

(define alist-extend
  (lambda (name denotable environment)
    (cons (cons name denotable)
          environment)))

(define alist-lookup
  (lambda (name environment found not-found)
    (letrec ([visit (lambda (e)
                      (if (null? e)
                          (not-found name)
                          (let ([binding (car e)])
                            (if (equal? name (car binding))
                                (found (cdr binding))
                                (visit (cdr e))))))])
      (visit environment))))

(define compare-labels
  (lambda (l1 l2)
    #t))

;;;;;;;;;;

(define check-silently
  #f)

;;;;;;;;;;

(define check-program
  (trace-lambda check-program (v pc env)
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
       (check-expression v pc)])))

;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for definitions:
;;;;;;;;;;

;;; generic:
(define is-given-type?
  (lambda (v length type)
    (and (proper-list-of-given-length? v length)
         (equal? (car v) type))))

(define is-given-variadic-type?
  (lambda (v length type)
    (and (proper-list-longer-than-given-length? v length)
         (equal? (car v) type))))

;;; predicate:
(define is-definition?
  (lambda (v)
    (is-given-type? v 3 'define)))

;;; 1st accessor:
(define define-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define define-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;
;;; the syntax checker proper for definitions:
;;;;;;;;;;

(define check-definition
  (trace-lambda check-definition (name definiens pc env)
    (let ([label (check-expression definiens pc)])
      (alist-extend name (eval label) env))))

;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for expressions:
;;;;;;;;;;

;;;;;

;;; predicate:
(define is-number?
  (lambda (v)
    (number? v)))

;;;;;

;;; predicate:
(define is-boolean?
  (lambda (v)
    (boolean? v)))

;;;;;

;;; predicate:
(define is-character?
  (lambda (v)
    (char? v)))

;;;;;

;;; predicate:
(define is-string?
  (lambda (v)
    (string? v)))

;;;;;

;;; predicate:
(define is-variable?
  (lambda (v)
    (and (symbol? v)
	 (not (keyword? v)))))

;;;;;

;;;;; IFC

(define is-label?
  (lambda (v)
    (is-given-type? v 3 'label)))

(define label-1
  (lambda (v)
    (list-ref v 1)))

(define label-2
  (lambda (v)
    (list-ref v 2)))

;;;;;

;;; predicate:
(define is-time?
  (lambda (v)
    (is-given-type? v 2 'time)))

;;; 1st accessor:
(define time-1
  (lambda (v)
    (list-ref v 1)))

;;;;;

;;; predicate:
(define is-if?
  (lambda (v)
    (is-given-type? v 4 'if)))

;;; 1st accessor:
(define if-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define if-2
  (lambda (v)
    (list-ref v 2)))

;;; 3rd accessor:
(define if-3
  (lambda (v)
    (list-ref v 3)))

;;;;;

;;; predicate:
(define is-and?
  (lambda (v)
    (is-given-variadic-type? v 1 'and)))

;;; predicate:
(define is-or?
  (lambda (v)
    (is-given-variadic-type? v 1 'or)))

;;; predicate:
(define is-cond?
  (lambda (v)
    (is-given-variadic-type? v 2 'cond)))

;;; predicate:
(define is-else?
  (lambda (v)
    (is-given-variadic-type? v 2 'else)))

;;; predicate:
(define is-case?
  (lambda (v)
    (is-given-variadic-type? v 2 'case)))

;;; predicate:
(define is-let?
  (lambda (v)
    (is-given-type? v 3 'let)))

;;; 1st accessor:
(define let-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define let-2
  (lambda (v)
    (list-ref v 2)))

;;; predicate:
(define is-letstar?
  (lambda (v)
    (is-given-type? v 3 'let*)))

;;; 1st accessor:
(define letstar-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define letstar-2
  (lambda (v)
    (list-ref v 2)))

;;; predicate:
(define is-letrec?
  (lambda (v)
    (is-given-type? v 3 'letrec)))

;;; 1st accessor:
(define letrec-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define letrec-2
  (lambda (v)
    (list-ref v 2)))

;;; predicate:
(define is-begin?
  (lambda (v)
    (and (pair? v)
	 (pair? (cdr v))
	 (equal? (car v) 'begin))))

;;; predicate:
(define is-unless?
  (lambda (v)
    (is-given-type? v 3 'unless)))

;;; 1st accessor:
(define unless-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define unless-2
  (lambda (v)
    (list-ref v 2)))

;;;;;

;;; predicate:
(define is-quote?
  (lambda (v)
    (is-given-type? v 2 'quote)))

;;; 1st accessor:
(define quote-1
  (lambda (v)
    (list-ref v 1)))

;;;;;

;;; predicate:
(define is-lambda?
  (lambda (v)
    (is-given-variadic-type? v 3 'lambda)))

;;; 1st accessor:
(define lambda-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define lambda-2
  (lambda (v)
    (list-ref v 2)))

;;;;;

;;; predicate:
(define is-trace-lambda?
  (lambda (v)
    (is-given-variadic-type? v 4 'trace-lambda)))

;;; 1st accessor:
(define trace-lambda-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define trace-lambda-2
  (lambda (v)
    (list-ref v 2)))

;;; 2nd accessor:
(define trace-lambda-3
  (lambda (v)
    (list-ref v 3)))

;;;;;

;;; predicate:
(define is-application?
  (lambda (v)
    (and (pair? v)
         (let ([w (car v)])
           (if (symbol? w)
               (not (keyword? w))
               #t)))))

;;; 1st accessor:
(define application-operator
  car)

;;; 2nd accessor:
(define application-operands
  cdr)

;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;

(define check-expression
  (lambda (v pc)
    (cond
      [(is-number? v)
       (check-number v pc)]
      [(is-boolean? v)
       (check-boolean v pc)]
      [(is-character? v)
       (check-character v pc)]
      [(is-string? v)
       (check-string v pc)]
      [(is-variable? v)
       (check-variable v pc)]

      ;;; IFC
      [(is-label? v)
       (check-label-expression (label-1 v) (label-2 v) pc)]
      ;;;
      
      [(is-time? v)
       (check-time-expression (time-1 v) pc)]
      [(is-if? v)
       (check-if-expression (if-1 v) (if-2 v) (if-3 v) pc)]
      [(is-and? v)
       (check-and-expression (cdr v) pc)]
      [(is-or? v)
       (check-or-expression (cdr v) pc)]
      [(is-cond? v)
       (check-cond-expression (cdr v) pc)]
      [(is-case? v)
       (check-case-expression (cdr v) pc)]
      [(is-let? v)
       (check-let-expression (let-1 v) (let-2 v) pc)]
      [(is-letstar? v)
       (check-let-expression (letstar-1 v) (letstar-2 v) pc)]
      [(is-letrec? v)
       (check-letrec-expression (letrec-1 v) (letrec-2 v) pc)]
      [(is-begin? v)
       (check-expressions (cdr v) pc)]
      [(is-unless? v)
       (check-unless-expression (unless-1 v) (unless-2 v) pc)]
      [(is-quote? v)
       (check-quote-expression (quote-1 v) pc)]
      [(is-lambda? v)
       (check-lambda (lambda-1 v) (lambda-2 v) pc)]
      [(is-trace-lambda? v)
       (check-trace-lambda (trace-lambda-1 v) (trace-lambda-2 v) (trace-lambda-3 v) pc)]
      [(is-application? v)
       (check-application (application-operator v) (application-operands v) pc)]
      [else
       (begin
         (unless check-silently
           (printf "check-expression -- unrecognized input: ~s~n" v))
         #f)])))

(define check-expressions
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-expression (car v) pc)
	   (check-expressions (cdr v) pc))])))

(define check-number
  (lambda (n pc)
    pc))

(define check-boolean
  (lambda (b pc)
    pc))

(define check-character
  (lambda (c pc)
    pc))

(define check-string
  (lambda (s pc)
    pc))

(define check-variable
  (lambda (v pc)
    pc))

(define check-variable*
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (check-variable (car v) pc)
      (check-variable* (cdr v) pc)]
     [else
      (check-variable v pc)])))

;;; IFC

(define check-label-expression
  (trace-lambda check-label (label expression pc)
    (if (compare-labels label pc)
	(check-expression expression label)
	(errorf "Mismatched labels ~n -> ~n" label pc))))

;;;

(define check-time-expression
  (lambda (v pc)
    (check-expression v pc)))

(define check-if-expression
  (lambda (test consequent alternative pc)
    (and (check-expression test pc)
         (check-expression consequent pc)
         (check-expression alternative pc))))

(define check-and-expression
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-expression (car v) pc)
	   (check-and-expression (cdr v) pc))])))  

(define check-or-expression
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-expression (car v) pc)
	   (check-or-expression (cdr v) pc))])))

(define check-cond-expression
  (lambda (v pc)
    (check-cond-clauses v pc)))

(define check-cond-clauses
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(is-else? (car v))
      (check-expression (cdar v) pc)]
     [(pair? v)
      (and (check-cond-clause (car v) pc)
	   (check-cond-clauses (cdr v) pc))])))
  

(define check-cond-clause
  (lambda (v pc)
    (if (proper-list-of-given-length? v 1)
	(check-expression v pc)
	(if (equal? (cadr v) '=>)
	    (and (check-expression (cdr v) pc)
		 (check-expression (caddr v) pc))
	    (and (check-expression (car v) pc)
		 (check-expression (cadr v) pc))))))

(define check-case-expression
  (lambda (v pc)
    (and (check-expression (car v) pc)
	 (check-case-clauses (cdr v) pc))))

(define check-case-clauses
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(is-else? (car v))
      (check-expression (cdar v) pc)]
     [(pair? v)
      (and (check-case-clause (car v) pc)
	   (check-case-clauses (cdr v) pc))])))

(define check-case-clause
  (lambda (v pc)
    (and (check-quotations (car v) pc)
	 (check-expression (cdr v) pc))))

(define check-let-expression
  (lambda (bindings body pc)
    (and (check-let-bindings bindings pc)
	 (check-expression body pc))))

(define check-let-bindings
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (check-let-binding (car v) pc)
      (check-let-bindings (cdr v) pc)])))

(define check-let-binding
  (lambda (v pc)
    (and (pair? v)
	 (check-variable (car v) pc)
	 (check-expression (cdr v) pc))))

(define check-letrec-expression
  (lambda (bindings body pc)
    (and (check-letrec-bindings bindings pc)
	 (check-expression body pc))))

(define check-letrec-bindings
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (check-letrec-binding (car v) pc)
      (check-letrec-bindings (cdr v) pc)])))

(define check-letrec-binding
  (lambda (v pc)
    (and (pair? v)
	 (check-variable (car v) pc)
	 (cond
	  [(is-lambda? (cdr v))
	   (check-lambda (cdr v) pc)]
	  [(is-trace-lambda? (cdr v))
	   (check-trace-lambda (cdr v) pc)]))))

(define check-unless-expression
  (lambda (test consequent pc)
    (and (check-expression test pc)
         (check-expression consequent pc))))

(define check-quote-expression
  (lambda (v pc)
    (check-quotation v pc)))

(define check-quotations
  (lambda (v pc)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (and (check-quotation (car v) pc)
	   (check-quotations (cdr v) pc))])))

(define check-quotation
  (lambda (v pc)
    (cond
      [(pair? v)
       (and (check-quotation (car v) pc)
	    (check-quotation (cdr v) pc))]
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
  (lambda (formals expression pc)
    (and (check-lambda-formals formals pc)
	 (check-expression expression pc))))

(define check-trace-lambda
  (lambda (name formals expression pc)
    (and (symbol? name)
	 (check-lambda-formals formals pc)
	 (check-expression expression pc))))

(define check-lambda-formals
  (lambda (v pc)
    (cond
     [(is-variable? v)
      (check-variable v pc)]
     [(list? v)
      (check-variable* v pc)])))

(define check-application
  (lambda (v vs pc)
    (and (check-expression v pc)
	 (check-expressions vs pc))))

;;;;;;;;;;
;;; auxiliaries:
;;;;;;;;;;

(define keywords
  (list 'define 'time 'if 'cond 'else 'case 'and 'or 'let 'let* 'letrec
        'begin 'quote 'quasiquote 'unquote 'unquote-splicing 'lambda 'trace-lambda))

(define keyword?
  (lambda (w)
    (member w keywords)))

(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v)
                                      (- i 1)))))])
      (if (>= n 0)
          (visit v n)
          (errorf 'list-strictly-longer-than? "negative length: ~s" n)))))

;;; reads an entire file as a list of Scheme data
;;; use: (read-file "filename.scm")
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))

;;; interface: 
(define check-file
  (lambda (filename pc env)
    (if (string? filename)
        (check-program (read-file filename) pc env)
        (errorf 'check-file "not a string: ~s" filename))))

;;;;;;;;

;;; end of week-4_a-syntax-checker-for-Scheme.scm

"week-4_a-syntax-checker-for-Scheme.scm"
