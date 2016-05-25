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
    "negative-tests/testLambdaEndLabelNotRestrictiveEnough.scm"
    "negative-tests/testLambdaInRestrictivePlace.scm"
    "negative-tests/testPlusIntegrity.scm"
    "negative-tests/testLambdaRecursive.scm"
    "negative-tests/testCaseConfidentiality.scm"
    "negative-tests/testCaseIntegrity.scm"
    "negative-tests/testCondConfidentiality.scm"
    "negative-tests/testCondIntegrity.scm"))

(define positive-tests
  '("positive-tests/testHigherConf.scm"
    "positive-tests/testLowerInt.scm"
    "positive-tests/testAnd.scm"
    "positive-tests/testOr.scm"
    "positive-tests/testIf.scm"
    "positive-tests/testLet.scm"
    "positive-tests/testLetAdding.scm"
    "positive-tests/testCond.scm"
    "positive-tests/testCondSimple.scm"
    "positive-tests/testCondSimple2.scm"
    "positive-tests/testCase.scm"
    "positive-tests/testCaseSimple.scm"
    "positive-tests/testRecursion.scm"
    "positive-tests/testLetrecRecursion.scm"
    "positive-tests/testLambda.scm"))

(define try
  (lambda (thunk)
    (call/cc
     (lambda (k)
       (with-exception-handler
        (lambda (x) (if (error? x) (k #f) (raise x)))
        thunk)))))

(define test-all-negative
  (lambda ()
    (letrec ([visit (lambda (tests)
                      (cond
                       [(null? tests)
                        #t]
                       [else
                        (let ([current (try (lambda ()
                                              (verify-constraints (car tests))))]
                              [rest (visit (cdr tests))])
                          (if current
                              (printf "No error in ~s~n" (car tests))
                              (and (not current)
                                   rest)))]))])
      (visit negative-tests))))

(define test-all-positive
  (lambda ()
    (letrec ([visit (lambda (tests)
                      (cond
                       [(null? tests)
                        #t]
                       [else
                        (let ([current (try (lambda ()
                                              (verify-constraints (car tests))))]
                              [rest (visit (cdr tests))])
                          (if current
                              (and current rest)
                              (printf "Errors in ~s~n" (car tests))))]))])
      (visit positive-tests))))
