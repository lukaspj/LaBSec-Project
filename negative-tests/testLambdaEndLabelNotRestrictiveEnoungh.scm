(load "framework.scm")

(define foo
  (label-lambda
   '(label (integrity . 0) (confidentiality . 0))
   ([x '(label () (confidentiality . 5))])
   '(label (integrity . 0) (confidentiality . 0))
   x))


                         
