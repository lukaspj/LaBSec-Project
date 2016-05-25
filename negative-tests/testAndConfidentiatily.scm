(load "framework.scm")

(define foo
  (label-expression '(label (integrity . 0) (confidentiality . 1))
	 42))

(define bar
  (label-expression '(label (integrity . 0) (confidentiality . 1))
		    24))

(define test
  (label-expression '(label (integrity . 0) (confidentiality . 0))
		    (and bar
			 foo)))

