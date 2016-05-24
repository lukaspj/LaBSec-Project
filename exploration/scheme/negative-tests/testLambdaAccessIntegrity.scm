(load "framework.scm")

(define foo
  (label-lambda botbot
                ()
                botbot
                42))

(define bar
  (label-lambda topbot
                ()
                topbot
                (foo)))
