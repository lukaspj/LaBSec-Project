(load "framework.scm")

(define foo
  (label-expression '(label () (confidentiality . 0))
		    10))

(define bar
  (label-expression '(label (integrity . 0) ())
		    30))

(define baz
  (label-expression '(label (integrity . 305) (confidentiality . 100050))
		    20))

(define test-cond
  (label-expression '(label (integrity . 5) ())
		    (cond [foo
			   bar]
			  [foo
			   #t]
			  [baz
			   5]
			  [else
			   baz])))
