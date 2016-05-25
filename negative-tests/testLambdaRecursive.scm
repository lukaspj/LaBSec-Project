(load "framework.scm")

(define foo
  (label-lambda '(label () (confidentiality . 0))
                ([x '(label () ())])
                '(label () ())
                (foo (- x 1))))
(foo 3)
