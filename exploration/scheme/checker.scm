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

;;;;;;;;;;

(define check-silently
  #f)

;;;;;;;;;;

(define check-program
  (lambda (v)
    (cond
      [(null? v)
       #t]
      [(pair? v)
       (and (check-toplevel-form (car v))
            (check-program (cdr v)))]
      [else
       (begin
         (unless check-silently
           (printf "check-program -- unrecognized input: ~s~n" v))
         #f)])))

;;;;;;;;;;

(define check-toplevel-form
  (lambda (v)
    (cond
      [(is-definition? v)
       (check-definition (define-1 v) (define-2 v))]
      [else
       (check-expression v)])))

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
  (lambda (name definiens)
    (and (check-variable name)
         (check-expression definiens))))

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
  (lambda (v)
    (cond
      [(is-number? v)
       (check-number v)]
      [(is-boolean? v)
       (check-boolean v)]
      [(is-character? v)
       (check-character v)]
      [(is-string? v)
       (check-string v)]
      [(is-variable? v)
       (check-variable v)]
      [(is-time? v)
       (check-time-expression (time-1 v))]
      [(is-if? v)
       (check-if-expression (if-1 v) (if-2 v) (if-3 v))]
      [(is-and? v)
       (check-and-expression (cdr v))]
      [(is-or? v)
       (check-or-expression (cdr v))]
      [(is-cond? v)
       (check-cond-expression (cdr v))]
      [(is-case? v)
       (check-case-expression (cdr v))]
      [(is-let? v)
       (check-let-expression (let-1 v) (let-2 v))]
      [(is-letstar? v)
       (check-let-expression (letstar-1 v) (letstar-2 v))]
      [(is-letrec? v)
       (check-letrec-expression (letrec-1 v) (letrec-2 v))]
      [(is-begin? v)
       (check-expressions (cdr v))]
      [(is-unless? v)
       (check-unless-expression (unless-1 v) (unless-2 v))]
      [(is-quote? v)
       (check-quote-expression (quote-1 v))]
      [(is-lambda? v)
       (check-lambda (lambda-1 v) (lambda-2 v))]
      [(is-trace-lambda? v)
       (check-trace-lambda (trace-lambda-1 v) (trace-lambda-2 v) (trace-lambda-3 v))]
      [(is-application? v)
       (check-application (application-operator v) (application-operands v))]
      [else
       (begin
         (unless check-silently
           (printf "check-expression -- unrecognized input: ~s~n" v))
         #f)])))

(define check-expressions
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(pair? v)
      (and (check-expression (car v))
	   (check-expressions (cdr v)))])))

(define check-number
  (lambda (n)
    #t))

(define check-boolean
  (lambda (b)
    #t))

(define check-character
  (lambda (c)
    #t))

(define check-string
  (lambda (s)
    #t))

(define check-variable
  (lambda (v)
    #t))

(define check-variable*
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(pair? v)
      (check-variable (car v))
      (check-variable* (cdr v))]
     [else
      (check-variable v)])))

(define check-time-expression
  (lambda (v)
    (check-expression v)))

(define check-if-expression
  (lambda (test consequent alternative)
    (and (check-expression test)
         (check-expression consequent)
         (check-expression alternative))))

(define check-and-expression
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(pair? v)
      (and (check-expression (car v))
	   (check-and-expression (cdr v)))])))  

(define check-or-expression
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(pair? v)
      (and (check-expression (car v))
	   (check-or-expression (cdr v)))])))

(define check-cond-expression
  (lambda (v)
    (check-cond-clauses v)))

(define check-cond-clauses
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(is-else? (car v))
      (check-expression (cdar v))]
     [(pair? v)
      (and (check-cond-clause (car v))
	   (check-cond-clauses (cdr v)))])))
  

(define check-cond-clause
  (lambda (v)
    (if (proper-list-of-given-length? v 1)
	(check-expression v)
	(if (equal? (cadr v) '=>)
	    (and (check-expression (cdr v))
		 (check-expression (caddr v)))
	    (and (check-expression (car v))
		 (check-expression (cadr v)))))))

(define check-case-expression
  (lambda (v)
    (and (check-expression (car v))
	 (check-case-clauses (cdr v)))))

(define check-case-clauses
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(is-else? (car v))
      (check-expression (cdar v))]
     [(pair? v)
      (and (check-case-clause (car v))
	   (check-case-clauses (cdr v)))])))

(define check-case-clause
  (lambda (v)
    (and (check-quotations (car v))
	 (check-expression (cdr v)))))

(define check-let-expression
  (lambda (bindings body)
    (and (check-let-bindings bindings)
	 (check-expression body))))

(define check-let-bindings
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(pair? v)
      (check-let-binding (car v))
      (check-let-bindings (cdr v))])))

(define check-let-binding
  (lambda (v)
    (and (pair? v)
	 (check-variable (car v))
	 (check-expression (cdr v)))))

(define check-letrec-expression
  (lambda (bindings body)
    (and (check-letrec-bindings bindings)
	 (check-expression body))))

(define check-letrec-bindings
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(pair? v)
      (check-letrec-binding (car v))
      (check-letrec-bindings (cdr v))])))

(define check-letrec-binding
  (lambda (v)
    (and (pair? v)
	 (check-variable (car v))
	 (cond
	  [(is-lambda? (cdr v))
	   (check-lambda (cdr v))]
	  [(is-trace-lambda? (cdr v))
	   (check-trace-lambda (cdr v))]))))

(define check-unless-expression
  (lambda (test consequent)
    (and (check-expression test)
         (check-expression consequent))))

(define check-quote-expression
  (lambda (v)
    (check-quotation v)))

(define check-quotations
  (lambda (v)
    (cond
     [(null? v)
      #t]
     [(pair? v)
      (and (check-quotation (car v))
	   (check-quotations (cdr v)))])))

(define check-quotation
  (lambda (v)
    (cond
      [(pair? v)
       (and (check-quotation (car v))
	    (check-quotation (cdr v)))]
      [(number? v)
       #t]
      [(boolean? v)
       #t]
      [(char? v)
       #t]
      [(string? v)
       #t]
      [(symbol? v)
       #t]
      [(null? v)
       #t]
      [else
       #f])))

(define check-lambda
  (lambda (formals expression)
    (and (check-lambda-formals formals)
	 (check-expression expression))))

(define check-trace-lambda
  (lambda (name formals expression)
    (and (symbol? name)
	 (check-lambda-formals formals)
	 (check-expression expression))))

(define check-lambda-formals
  (lambda (v)
    (cond
     [(is-variable? v)
      (check-variable v)]
     [(list? v)
      (check-variable* v)])))

(define check-application
  (lambda (v vs)
    (and (check-expression v)
	 (check-expressions vs))))

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
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (errorf 'check-file "not a string: ~s" filename))))

;;;;;;;;

;;; end of week-4_a-syntax-checker-for-Scheme.scm

"week-4_a-syntax-checker-for-Scheme.scm"
