(load "framework.scm")

(define foo
  (label-expression '(label () (confidentiality . 5))
                    #t))

(define bar
  (label-expression '(label () (confidentiality . 2))
                    42))

(define test
  (label-expression '(label () (confidentiality . 2))
                    (if foo
                        bar
                        40)))
