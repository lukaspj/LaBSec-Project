(define foo
  (lambda ()
    (foo)))

(define gcd
  (lambda (a b)
    (cond [(= b 0) a]
	  [else (gcd b (modulo a b))])))
