;(define check-integrity-flows-to
;  (trace-lambda check-integrity (l1 l2)
;    (let ([readers1 (cadr l1)]
;	  [readers2 (cadr l2)])
;      #t)))

;(define check-confidentiality-flows-to
;  (trace-lambda check-confidentiality (l1 l2)
;    (let ([writers1 (caddr l1)]
;	  [writers2 (caddr l2)])
;      #t)))


;; Label Bnf
;; <Label> ::= (<Label>*)
;;           | <value-label>
;;           | <lambda-label>

;; <value-label> ::= (label integrity confidentiality)

;; <lambda-label> ::= (lambda-label <value-label> (<value-label>*) <value-label>)

(define is-given-type?
  (lambda (v length type)
    (and (proper-list-of-given-length? v length)
         (equal? (car v) type))))

(define is-value-label?
  (lambda (v)
    (is-given-type? v 3 'label)))

(define is-lambda-label?
  (lambda (v)
    (is-given-type? v 4 'lambda-label)))

(define value-label-integrity
  (lambda (v)
    (list-ref v 1)))

(define value-label-confidentiality
  (lambda (v)
    (list-ref v 2)))

(define lambda-label-begin
  (lambda (v)
    (list-ref v 1)))

(define lambda-label-params
  (lambda (v)
    (list-ref v 2)))

(define lambda-label-end
  (lambda (v)
    (list-ref v 3)))

(define check-label
  (lambda (v)
    (cond
     [(is-value-label? v)
      (check-value-label (value-label-integrity v)
                         (value-label-confidentiality v))]
     [(is-lambda-label? v)
      (check-lambda-label (lambda-label-begin v)
                          (lambda-label-params v)
                          (lambda-label-end v))]
     [(pair? v)
      (if (null? (cdr v))
          (check-label (car v))
          (and (check-label (car v))
               (check-label (cdr v))))]
     [else
      (printf "Not a recognized label: ~s~n" v)])))

(define check-value-label
  (lambda (integrity confidentiality)
    (and (or (null? integrity)
             (and (pair? integrity)
                  (equal? (car integrity) 'integrity)
                  (number? (cdr integrity))))
         (or (null? confidentiality)
             (and (pair? confidentiality)
                  (equal? (car confidentiality) 'confidentiality)
                  (number? (cdr confidentiality)))))))

(define check-lambda-label
  (lambda (begin-label params end-label)
    (and (check-label begin-label)
         (check-labels params)
         (check-label end-label))))

(define check-labels
  (lambda (vs)
    (if (null? vs)
        #t
        (and (check-label (car vs))
             (check-labels (cdr vs))))))
;;;;;;;;;;;;
;;; Flows and joins
;;;;;;;;;;;;

;;; Centralized-One-dimensional label model
(define check-integrity-flows-to
  (lambda (l1 l2)
    (let ([integrity1 (cadr l1)]
	  [integrity2 (cadr l2)])
      (if (null? integrity1)
	  #t
	  (and (not (null? integrity2))
	       (>= (cdr integrity1) (cdr integrity2)))))))

(define check-confidentiality-flows-to
  (lambda (l1 l2)
    (let ([confidentiality1 (caddr l1)]
	  [confidentiality2 (caddr l2)])
      (if (null? confidentiality1)
	  (null? confidentiality2)
	  (or (null? confidentiality2)
	      (<= (cdr confidentiality1) (cdr confidentiality2)))))))

(define join-integrity
  (lambda (l1 l2)
    (let ([integrity1 (cadr l1)]
	  [integrity2 (cadr l2)])
      (if (null? integrity1)
	  (if (null? integrity2)
	      '()
	      integrity2)
	  (if (null? integrity2)
	      integrity1
	      (cons 'integrity
		    (min (cdr integrity1) (cdr integrity2))))))))
  
(define join-confidentiality
  (lambda (l1 l2)
	  (let ([confidentiality1 (caddr l1)]
		[confidentiality2 (caddr l2)])
	    (if (or (null? confidentiality1)
		    (null? confidentiality2))
		'()
		(cons 'confidentiality
		      (max (cdr confidentiality1) (cdr confidentiality2)))))))
  
(define label-join
  (lambda (l1 l2)
    `(label ,(join-integrity l1 l2) ,(join-confidentiality l1 l2))))

(define label-flows-to
  (lambda (l1 l2)
    (and (check-integrity-flows-to l1 l2)
	 (check-confidentiality-flows-to l1 l2))))


;;;;;;;;;;;;;;;;;;;
;;; Unit test
;;;;;;;;;;;;;;;;;;;

(define toptop
  '(label () ()))

(define botbot
  '(label (integrity . 0) (confidentiality . 0)))

(define topbot
  '(label () (confidentiality . 0)))

(define bottop
  '(label (integrity . 0) ()))

(define topone
  '(label () (confidentiality . 1)))

(define onetop
  '(label (integrity . 0) ()))

(define toptwo
  '(label () (confidentiality . 2)))

(define twotop
  '(label (integrity . 2) ()))

(define test-flows-to-equal
  (lambda (p1 p2 expected)
    (if (equal? (label-flows-to (eval p1) (eval p2)) expected)
	#t
	(errorf
	 'test-flows-to-equal
	 "~s flows to ~s was expected to be ~s~n"
	 p1 p2 expected))))

(define test-flows-to
  (and (test-flows-to-equal 'toptop 'botbot #f)
       (test-flows-to-equal 'toptop 'topbot #f)
       (test-flows-to-equal 'toptop 'bottop #t)
       (test-flows-to-equal 'toptop 'toptop #t)

       (test-flows-to-equal 'botbot 'botbot #t)
       (test-flows-to-equal 'botbot 'topbot #f)
       (test-flows-to-equal 'botbot 'bottop #t)
       (test-flows-to-equal 'botbot 'toptop #f)

       (test-flows-to-equal 'bottop 'botbot #f)
       (test-flows-to-equal 'bottop 'topbot #f)
       (test-flows-to-equal 'bottop 'bottop #t)
       (test-flows-to-equal 'bottop 'toptop #f)

       (test-flows-to-equal 'topbot 'botbot #t)
       (test-flows-to-equal 'topbot 'topbot #t)
       (test-flows-to-equal 'topbot 'bottop #t)
       (test-flows-to-equal 'topbot 'toptop #t)

       (test-flows-to-equal 'topone 'botbot #f)
       (test-flows-to-equal 'topone 'topbot #f)
       (test-flows-to-equal 'topone 'bottop #t)
       (test-flows-to-equal 'topone 'toptop #t)

       (test-flows-to-equal 'onetop 'botbot #f)
       (test-flows-to-equal 'onetop 'topbot #f)
       (test-flows-to-equal 'onetop 'bottop #t)
       (test-flows-to-equal 'onetop 'toptop #f)

       (test-flows-to-equal 'onetop 'twotop #f)
       (test-flows-to-equal 'topone 'toptwo #t)

       (test-flows-to-equal 'twotop 'onetop #t)
       (test-flows-to-equal 'toptwo 'topone #f)))

(define test-join-equal
  (lambda (p1 p2 expected)
    (let ([result (label-join (eval p1) (eval p2))])
      (if (equal? result (eval expected))
	  #t
	  (errorf
	   'test-join-equal
	   "~s join ~s was expected to be ~s but was ~s ~n"
	   p1 p2 expected result)))))
  
(define test-join
  (and (test-join-equal 'toptop 'botbot 'bottop)
       (test-join-equal 'botbot 'toptop 'bottop)
       (test-join-equal 'topbot 'toptop 'toptop)
       (test-join-equal 'topbot 'topbot 'topbot)))
