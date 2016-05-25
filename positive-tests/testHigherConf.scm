(load "framework.scm")

(define lowintconf
  (label-expression '(label (integrity . 0) (confidentiality . 0))
		    10))

(define lowinthighconf
  (label-expression '(label (integrity . 0) ())
		    lowintconf))
