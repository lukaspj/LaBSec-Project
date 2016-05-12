(load "code1.scm")

(define bar
  (label 'private 'bar
	 42))

(define foo
  (label 'public 'foo
	 bar))

(define verify-application
  (lambda ()
    (verify-constraints "application.scm")))
