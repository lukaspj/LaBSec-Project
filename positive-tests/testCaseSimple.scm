(load "framework.scm")

(define foo
  (label-expression '(label () (confidentiality . 0))
		    10))

(define bar
  (label-expression '(label () ())
		    30))

(define baz
  (label-expression '(label (integrity . 305) (confidentiality . 100050))
		    20))

(define test-case
  (label-expression '(label (integrity . 44) ())
		    (case foo
		      [(2 3 4)
		       bar]
		      [else
		       baz])))
