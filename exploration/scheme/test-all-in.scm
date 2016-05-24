(load "framework.scm")

(define negative-tests
  '("negative-tests/testAndConfidentiality.scm"
    "negative-tests/testAndIntegrity.scm"
    "negative-tests/testOrConfidentiality.scm"
    "negative-tests/testOrIntegrity.scm"
    "negative-tests/testLetVariableAccess.scm"
    "negative-tests/testLetrecFunctionAccess.scm"
    "negative-tests/testLetrecVariableAccess.scm"
    "negative-tests/testVariableAccess.scm"
    "negative-tests/testLambdaAccessIntegrity.scm"
    "negative-tests/testLambdaAccessConfidentiality.scm"))

(define try
  (lambda (thunk)
    (call/cc
     (lambda (k)
       (with-exception-handler
        (lambda (x) (if (error? x) (k #f) (raise x)))
        thunk)))))

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
