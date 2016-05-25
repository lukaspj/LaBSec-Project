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

;;;;;;;;;;

(define check-file
  (lambda (file pc env)
    (check-program (read-file file) pc env)))

;;;;;;;;;;

(define check-program
  (lambda (v pc env)
    (cond
      [(null? v)
       env]
      [(pair? v)
       (if (equal? (car v) ''Skip__IFC__Check)
	   env
	   (check-program (cdr v)
			  pc
			  (check-toplevel-form (car v) pc env)))]
      [else
       (begin
         (unless check-silently
           (printf "check-program -- unrecognized input: ~s~n" v))
         #f)])))

;;;;;;;;;;

(define check-toplevel-form
  (lambda (v pc env)
    (cond
     [(is-load? v)
      (check-file (load-1 v) pc env)]
     [(is-definition? v)
      (check-definition (define-1 v) (define-2 v) pc env)]
     [else
      (begin (check-expression v pc env)
	     env)])))

;;;;;;;;;;


;;;;;;;;;;
;;; the syntax checker proper for definitions:
;;;;;;;;;;

(define check-definition
  (lambda (name definiens pc env)
    (let ([label (check-expression definiens pc (alist-extend name 'predefined env))])
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
      [(is-label-lambda? v)
       (check-label-lambda-expression (label-lambda-1 v)
                                      (label-lambda-2 v)
                                      (label-lambda-3 v)
                                      (label-lambda-4 v)
                                      pc env)]
      [(is-set? v)
       (check-set-expression (set-1 v) (set-2 v) pc env)]
      [(is-declassify? v)
       (check-declassify-expression (declassify-1 v) (declassify-2 v) pc env)]
      ;;;
      
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
      (begin (check-expression (car v) pc env)
             (check-expressions (cdr v) pc env))])))


(define check-variable
  (lambda (v pc env)
    (alist-lookup
     v
     env
     (lambda(label)
       (if (equal? label 'predefined)
           'predefined
           (label-join label pc)))
     (lambda(v)
       pc))))

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

(define check-label-lambda-expression
  (lambda (begin-label params end-label body pc env)
    (let ([b (eval begin-label)]
          [e (eval end-label)])
      (if (label-flows-to pc b)
          (let ([ret-label (check-expression body
                                             b
                                             (check-label-lambda-formals params
                                                                         pc
                                                                         env))])
            (if (label-flows-to ret-label e)
                `(lambda-label ,b ,(params-to-label-list params) ,e)
                (errorf 'check-label-lambda-expression
                        "Mismatched labels for ret-label and end-label~n~s -> ~s For expression: ~s~n"
                        ret-label
                        e
                        body)))
          (errorf 'check-label-lambda-expression
                  "Mismatched labels for pc and begin-label with ~s -> ~s~n For expression: ~s~n"
                  pc
                  b
                  body)))))

(define params-to-label-list
  (lambda (params)
    (cond
     [(null? params)
      '()]
     [(pair? params)
      (cons (eval (cadar params))
            (params-to-label-list (cdr params)))])))

(define check-label-lambda-formals
  (lambda (params pc env)
    (cond
     [(null? params)
      env]
     [(pair? params)
      (let ([varname (caar params)]
            [label (cadar params)])
        (alist-extend varname
                      (eval label)
                      (check-label-lambda-formals (cdr params)
                                                  pc
                                                  env)))])))

(define check-set-expression
  (lambda (name val-expr pc env)
    (alist-lookup
     name
     env
     (lambda(label)
       (if (label-flows-to label pc)
           (label-join pc (if (is-lambda-label? label) (lambda-label-begin label) label))
           (errorf 'check-set-expression
                   "Mismatched label for set! ~s -> ~s~nFor expression: (set! ~s ~s)~n"
                   pc
                   label
                   name
                   val-expr)))
     (lambda(v)
       (begin
         (warnf 'check-set-expression
                "set! not implemented for undefined variables: (set! ~s ~s)~n"
                name
                val-expr)
         pc)))))

(define check-declassify-expression
  (lambda (label expr pc env)
    (eval label)))

;;;

(define check-if-expression
  (lambda (test consequent alternative pc env)
    (let ([cond-label (check-expression test pc env)])
      (label-join 
       (check-expression consequent
                         (label-join cond-label
                                     pc)
                         env)
       (check-expression alternative
                         (label-join cond-label
                                     pc)
                         env)))))

(define check-and-expression
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (check-and-expression (cdr v)
			    (check-expression (car v) pc env)
			    env)])))  

(define check-or-expression
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(pair? v)
      (check-or-expression (cdr v)
			   (check-expression (car v) pc env)
			   env)])))

(define check-cond-expression
  (lambda (v pc env)
    (check-cond-clauses v pc env)))

(define check-cond-clauses
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(is-else? (car v))
      (check-expression (cadar v) pc env)]
     [(pair? v)
      (check-cond-clauses
       (cdr v)
       (check-cond-clause (car v) pc env)
       env)])))
  

(define check-cond-clause
  (lambda (v pc env)
    (if (proper-list-of-given-length? v 1)
	(check-expression v pc env)
        (check-expression (cadr v)
                          (check-expression (cadr v) pc env)
                          env))))

(define check-case-expression
  (lambda (v pc env)
    (check-case-clauses (cdr v)
                        (check-expression (car v) pc env)
                        env)))

(define check-case-clauses
  (lambda (v pc env)
    (cond
     [(null? v)
      pc]
     [(is-else? (car v))
      (check-expression (cadar v) pc env)]
     [(pair? v)
      (check-case-clauses (cdr v)
                          (check-case-clause (car v) pc env)
                          env)])))

(define check-case-clause
  (lambda (v pc env)
    ;(check-quotations (car v) pc env)
    (check-expression (cadr v) pc env)))

(define check-let-expression
  (lambda (bindings body pc env)
    (let ([bindings-env (check-let-bindings bindings
                                            pc
                                            env)])
      (check-expression body
                        pc
                        bindings-env))))

(define check-let-bindings
  (lambda (v pc env)
    (cond
     [(null? v)
      env]
     [(pair? v)
      (check-let-bindings (cdr v)
                          pc
                          (check-let-binding (car v)
                                             pc
                                             env))])))

(define check-let-binding
  (lambda (v pc env)
    (alist-extend (car v)
                  (label-join (check-expression (cadr v)
                                                pc
                                                env)
                              pc)
                  env)))

(define check-letrec-expression
  (lambda (bindings body pc env)
    (check-expression body
                      pc
                      (check-letrec-bindings bindings
                                             pc
                                             env))))

(define check-letrec-bindings
  (lambda (v pc env)
    (cond
     [(null? v)
      env]
     [(pair? v)
      (check-letrec-bindings (cdr v)
                             pc
                             (check-letrec-binding (car v)
                                                   pc
                                                   env))])))

(define check-letrec-binding
  (lambda (v pc env)
    (alist-extend (car v)
                  (label-join (check-expression (cadr v)
                                                pc
                                                (alist-extend (car v)
                                                              'predefined
                                                              env))
                              pc)
                  env)))

(define check-unless-expression
  (lambda (test consequent pc env)
    (check-expression consequent
                      (check-expression test pc env)
                      env)))

(define check-quote-expression
  (lambda (v pc env)
    (check-quotation v pc env)))

(define check-quotations
  (lambda (v pc env)
    pc))
    
(define check-quotation
  (lambda (v pc env)
    (cond
      [(pair? v)
       pc]
                                        ;(and (check-quotation (car v) pc env)
                                        ;    (check-quotation (cdr v) pc env))]
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

(define formals-to-list-of
  (lambda (formals pc)
    (cond
     [(pair? formals)
      (cons pc
            (formals-to-list-of (cdr formals) pc))]
     [else
      '()])))

(define check-lambda
  (lambda (formals expression pc env)
    `(lambda-label ,pc
                   ,(formals-to-list-of formals pc)
                   ,(check-expression expression
                                      pc
                                      (check-lambda-formals
                                       formals
                                       pc
                                       env)))))

(define check-trace-lambda
  (lambda (name formals expression pc env)
    (check-expression expression
                      pc
                      (check-lambda-formals formals pc env))))

(define check-lambda-formals
  (lambda (v pc env)
    (cond
     [(null? v)
      env]
     [(pair? v)
      (alist-extend (car v) pc
                    (check-lambda-formals (cdr v) pc env))])))

(define join-all-labels
  (lambda (vs pc env)
    (cond
     [(null? vs)
      '(label () (confidentiality . 0))]
     [else
      (label-join (check-expression (car vs) pc env)
                  (join-all-labels (cdr vs) pc env))])))

(define check-application
  (lambda (v vs pc env)
    (let ([ret-label (check-expression v pc env)])
      (if (equal? 'predefined ret-label)
          (join-all-labels vs pc env)
          (let ([end-label (lambda-label-end ret-label)]
                [begin-label (lambda-label-begin ret-label)])
            (if (label-flows-to pc begin-label)
                (begin (check-expressions-with-label-list vs
                                                          (lambda-label-params ret-label)
                                                          pc
                                                          env)
                       end-label)
                (errorf 'check-application
                        "pc more restrictive than begin label in application with ~s -> ~s~nFor: ~s~n"
                        pc
                        begin-label
                        v)))))))

(define check-expressions-with-label-list
  (lambda (actuals formals pc env)
    (cond
     [(null? actuals)
      pc]
     [(pair? actuals)
      (let ([actual-label (check-expression (car actuals)
                                            pc ; TODO should this be (car labels) ?
                                            env)])
        (if (label-flows-to actual-label
                            (car formals))
            (label-join (check-expressions-with-label-list (cdr actuals)
                                                           (cdr formals)
                                                           pc
                                                           env)
                        (car formals))
            (errorf 'check-expressions-with-label-list
                    "Mismatched labels in parameters with ~s -> ~s~nFor: ~s~n"
                    actual-label
                    (car formals)
                    (car actuals))))])))

;;; end of week-4_a-syntax-checker-for-Scheme.scm

"week-4_a-syntax-checker-for-Scheme.scm"
