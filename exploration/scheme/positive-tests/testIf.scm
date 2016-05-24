(load "framework.scm")

(define foo
  (label-expression '(label (integrity . 7) (confidentiality . 2))
		    10))

(define bar
  (label-expression '(label () (confidentiality . 5))
		    30))

(define baz
  (label-expression '(label (integrity . 30) (confidentiality . 0))
                    40))

(define test-if
  (label-expression '(label (integrity . 2) (confidentiality . 10))
		    (if foo
			bar
			baz)))
