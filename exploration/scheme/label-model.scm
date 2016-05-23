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


(define bot-inte)
(define bot-conf)

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

(define label-flows-to
  (lambda (l1 l2)
    (and (check-integrity-flows-to l1 l2)
	 (check-confidentiality-flows-to l1 l2))))

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
