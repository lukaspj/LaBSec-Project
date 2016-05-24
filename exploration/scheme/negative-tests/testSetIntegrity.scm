(load "framework.scm")

(define a
  (label-lambda '(label (integrity . 0) ())
                ()
                '(label (integrity . 0) ())
                42))

(set! a 3)
