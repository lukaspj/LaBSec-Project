(define f
  42)

(define foo
  (label-lambda '(label () (confidentiality . 0))
                ([x '(label () (confidentiality . 0))])
                '(label () (confidentiality . 0))
                x))

(printf "~s~n" (foo f))
