(load "framework.scm")

(principals 'alice 'bob)

(define test1
  (label-expression '(label ((readers alice ())) ((writers bob ())))
		    42))

(define verify-exampels
  (lambda ()
    (verify-constraints "exampels.scm")))
