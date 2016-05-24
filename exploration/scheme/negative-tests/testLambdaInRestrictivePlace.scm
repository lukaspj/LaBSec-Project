(load "framework.scm")


(define foo
  (label-expression '(label () (confidentiality . 5))
                    #t))


(define bar
  (if foo
      (let ([baz 40])
        (label-lambda '(label () (confidentiality . 0))
                      ([x '(label () (confidentiality . 0))])
                      '(label () (confidentiality . 0))
                      (+ baz x)))
      (lambda (x) x)))

(bar 2)
