(load "framework.scm")

(define foo
  (label-lambda '(label () ())
                (x '(label () ()))
                '(label () ())
                (if (< x 0)
                    0
                    (+ 1 (foo (- x 1))))))

(foo 3)
