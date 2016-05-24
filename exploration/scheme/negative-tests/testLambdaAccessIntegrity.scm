(load "framework.scm")

(define foo
  (label-lambda topbot
                ()
                topbot
                42))

(define bar
  (label-lambda botbot
                ()
                botbot
                (foo)))
