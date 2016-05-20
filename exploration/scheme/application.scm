(load "framework.scm")

(define secret
  (label 'private
	 42))

(define foo
  (label 'private
	 (lambda (abc)
	   42)))

(define do-long-computation
  (lambda () 2))
(define do-short-computation
  (lambda () 2))

(define timing-vulnerable-program
  (lambda (n)
    (if (> secret n)
	(do-long-computation)
	(do-short-computation))))

(define sideeffect-vulnerable-program
  (lambda (p)
    (printf "~s~n" p)))

(define sideeffect-vulnerable-program-2
  (lambda (p)
    (set! s p)))

(define sideeffect-vulnerable-program-3
  (lambda (p)
    (errorf 'erro "as")))

(timing-vulnerable-program 42)
(sideeffect-vulnerable-program secret)
(sideeffect-vulnerable-program-2 secret)
;(sideeffect-vulnerable-program-3 secret)


(define verify-application
  (lambda ()
    (verify-constraints "application.scm")))
