(load "framework.scm")

(define foo
  (label-lambda bottop
                ()
                bottop
                42))

(define bar
  (label-lambda botbot
                ()
                botbot
                (foo)))
