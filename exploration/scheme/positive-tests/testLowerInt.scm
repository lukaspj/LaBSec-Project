(load "framework.scm")

(define lowintconf
  (label-expression '(label () ())
		    10))

(define lowinthighconf
  (label-expression '(label (integrity . 0) ())
		    lowintconf))
