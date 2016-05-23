(load "framework.scm")

(define negative-tests
  '("negative-tests/test1.scm"
    "negative-tests/test2.scm"
    "negative-tests/test3.scm"
    "negative-tests/test4.scm"))

(define (try thunk)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (x) (if (error? x) (k #f) (raise x)))
        thunk))))

(define test-all
  (lambda (tests)
    (cond
     [(null? tests)
      #t]
     [else
      (let ([current (try (lambda () (verify-constraints (car tests))))]
	    [rest (test-all (cdr tests))])
	(if current
	    (printf "No error in ~s~n" (car tests))
	    (and (not current)
		 rest)))])))
