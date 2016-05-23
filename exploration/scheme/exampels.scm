(load "framework.scm")

(principals 'alice 'bob)

(define test1
  (label-expression '(label (integrity . 0) (confidentiality . 0))
		    42))

(define test2
  (label-expression '(label (integrity . 0) ())
		    test1))

(define test3
  test1)

(define test4
  (label-expression '(label (integrity . 1) ())
		    test3))


(define verify-exampels
  (lambda ()
    (verify-constraints "exampels.scm")))
