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
    (is-given-type? v 3 'label-expression)))

(define label-1
  (lambda (v)
    (list-ref v 1)))

(define label-2
  (lambda (v)
    (list-ref v 2)))

(define is-load?
  (lambda (v)
    (is-given-type? v 2 'load)))

(define load-1
  (lambda (v)
    (list-ref v 1)))

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
;;; Auxiliary checkers
;;;;;;;;;;

(define check-number
  (lambda (n pc env)
    pc))

(define check-boolean
  (lambda (b pc env)
    pc))

(define check-character
  (lambda (c pc env)
    pc))

(define check-string
  (lambda (s pc env)
    pc))


;;;;;;;;;;
;;; Auxiliaries:
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

