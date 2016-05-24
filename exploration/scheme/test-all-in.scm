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
    "negative-tests/testLetAdding.scm"
    "negative-tests/testLambdaAccessIntegrity.scm"
    "negative-tests/testLambdaAccessConfidentiality.scm"
    "negative-tests/testLambdaEndLabelNotRestrictiveEnoungh.scm"
    "negative-tests/testLambdaInRestrictivePlace.scm"))

(define positive-tests
  '("positive-tests/testHigherConf.scm"
    "positive-tests/testLowerInt.scm"
    "positive-tests/testAnd.scm"
    "positive-tests/testOr.scm"
    "positive-tests/testIf.scm"
    "positive-tests/testLetAdding.scm"))

(define try
  (lambda (thunk)
    (call/cc
     (lambda (k)
       (with-exception-handler
        (lambda (x) (if (error? x) (k #f) (raise x)))
        thunk)))))

(define test-all-negative
  (lambda (tests)
    (cond
     [(null? tests)
      #t]
     [else
      (let ([current (try (lambda () (verify-constraints (car tests))))]
	    [rest (test-all-negative (cdr tests))])
	(if current
	    (printf "No error in ~s~n" (car tests))
	    (and (not current)
		 rest)))])))

(define test-all-positive
  (lambda (tests)
    (cond
     [(null? tests)
      #t]
     [else
      (let ([current (try (lambda () (verify-constraints (car tests))))]
	    [rest (test-all-positive (cdr tests))])
	(if current
	    (and current rest)
	    (printf "Errors in ~s~n" (car tests))))])))
