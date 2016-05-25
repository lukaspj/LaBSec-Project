(define foo
  (lambda ()
    (letrec ([visit (lambda (n)
                      (if (= n 0)
                          0
                          (visit (- n 1))))])
      (visit 3))))
