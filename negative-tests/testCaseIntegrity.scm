(load "framework.scm")

(define foo
  (label-expression '(label () (confidentiality . 0))
		    10))

(define bar
  (label-expression '(label (integrity . 0) ())
		    30))

(define baz
  (label-expression '(label (integrity . 305) (confidentiality . 100050))
		    20))

(define test-let
  (label-expression '(label (integrity . 44) ())
		    (case foo
		      [(1 2)
		       bar]
		      [baz
		       (case 4
			 [5
			  (and foo bar)]
			 [else foo])]
		      [foo
		       (if bar
			   foo
			   #t)]
		      [else
		       baz])))
