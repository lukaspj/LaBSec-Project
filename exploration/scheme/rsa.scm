;;; Key gen
(define euler
  (lambda (p q)
    (* (- p 1)
       (- q 1))))

(define gcd
  (lambda (a b)
    (cond [(= b 0) a]
	  [else (gcd b (modulo a b))])))

(define pick-e
  (lambda (euler max)
    (let ([e-candidate (random max)])
      (if (= (gcd euler e-candidate) 1)
	  e-candidate
	  (pick-e euler)))))

(define eea
  (lambda (a b)
    (letrec ([visit (lambda (r0 r1 s0 s1 t0 t1)
		      (if (= r1 0)
			  (list r0 s0 t0)
			  (let* ([q (floor (/ r0 r1))]
				 [r2 (- r0 (* q r1))]
				 [s2 (- s0 (* q s1))]
				 [t2 (- t0 (* q t1))])
			    (visit r1 r2 s1 s2 t1 t2))))])
      (visit a b 1 0 0 1))))
				
(define find-d
  (lambda (e euler)
    (mod (cadr (eea e euler)) euler)))

;;; Encrypt and Decrypt

(define modulo_expt_power_of_two
  (lambda (a_init b_init c)
    (letrec ([visit (lambda (a b)
		      (if (= b 1)
			  (list a)
			  (let ([n (visit a (- b 1))])
			    (cons (mod (* (car n) (car n)) c) n))))])
      (visit a_init b_init))))


(define modulo_expt
  (lambda (a b c)
    (let* ([b_str (number->string b 2)]
	   [power_of_twos (modulo_expt_power_of_two a
						    (string-length b_str)
						    c)])
      (letrec ([visit (lambda (str powers acc)
			(if (null? str)
			    (mod acc c)
			    (if (equal? (car str) #\1)
				(visit (cdr str) (cdr powers) (* (car powers) acc))
				(visit (cdr str) (cdr powers) acc))))])
	(visit (string->list b_str) power_of_twos 1)))))
    


(define encrypt
  (lambda (m key)
    (modulo_expt m (car key) (cdr key))))

(define decrypt
  (lambda (m key)
    (modulo_expt m (car key) (cdr key))))

;;; Test with small keys

(define prime-p 5)
(define prime-q 7)

(define key-n (* prime-p prime-q))

(define euler-n (euler prime-p prime-q))

(define key-e 11)

(define key-d (find-d key-e euler-n))

;;; Test with large keys

(define large-p
  2074722246773485207821695222107608587480996474721117292752992589912196684750549658310084416732550077)

(define large-q
  2908511952812557872434704820397229928450530253990158990550731991011846571635621025786879881561814989)

(define large-n
  (* large-p large-q))

(define large-euler
  (euler large-p large-q))


(define large-e 39937)
(define large-d (find-d large-e large-euler))  


(define public-key (cons large-e large-n))
(define private-key (cons large-d large-n))

;;; Brute force key

(define (isqrt n)
  (let loop ((xn 1))
    (let ((xn1 (quotient (+ xn (quotient n xn)) 2)))
      (if (= xn1 xn)
          xn1
          (loop xn1)))))

(define factor
  (lambda (n)
    (letrec ([visit (lambda (d i)
		      (if (integer? (/ n d))
			  d
			  (if (= i 2000000)
			      (begin
				(printf "~s~n" d)
				(visit (- d 2) 0))
			      (visit (- d 2) (1+ i)))))])
      (visit (isqrt n) 0))))
