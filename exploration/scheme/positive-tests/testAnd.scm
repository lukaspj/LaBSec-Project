(load "framework.scm")

(define foo
  (label-expression '(label (integrity . 7) (confidentiality . 2))
		    10))

(define bar
  (label-expression '(label (integrity . 5) (confidentiality . 10))
		    30))

(define test-and
  (label-expression '(label (integrity . 2) (confidentiality . 10))
		    (and foo bar)))
