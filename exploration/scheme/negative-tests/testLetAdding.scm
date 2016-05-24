(load "framework.scm")

(define foo
  (label-expression '(label (integrity . 7) (confidentiality . 2))
		    10))

(define bar
  (label-expression '(label () (confidentiality . 5))
		    30))

(define baz
  (label-expression '(label (integrity . 30) (confidentiality . 0))))

(define test-let
  (label-expression '(label (integrity . 0) ())
		    (let ([a (label-expression '(label (integrity 32) (confidentiality . 10))
					       baz)])
			(+ a foo))))
