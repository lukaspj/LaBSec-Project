(load "framework.scm")

(define foo
  (label-expression '(label (integrity . 0) (confidentiality . 0))
	 42))

(define bar
  (label-expression '(label (integrity . 1) (confidentiality . 0))
		    24))

(define test
  (label-expression '(label (integrity . 1) (confidentiality . 0))
		    (or bar
			foo)))

