(load "./../framework.scm")

(define foo
  (label-expression '(label (integrity . 0) (confidentiality . 0))
	 42))

(define test
  (label-expression '(label (integrity . 1) ())
		    (letrec ([a foo])
		      a)))
