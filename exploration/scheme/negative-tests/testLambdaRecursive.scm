(load "framework.scm")

(define foo
  (label-lambda '(label (integrity . 0) ())
                ([x '(label () ())])
                '(label () (confidentiality . 0))
                (if (< x 0)
                    0
                    (+ 1 (foo (- x 1))))))

(foo 3)
