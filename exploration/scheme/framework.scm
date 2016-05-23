'Skip__IFC__Check

(load "./checker.scm")

(define label-expression
  (lambda (label expr)
    expr))

(define principals
  (lambda p
    p))


(define verify-constraints
  (lambda (file)
    (check-file file '(label () (confidentiality . 0)) alist-mt)))


;;; label bnf proposal
;;; 
;;; label     ::= ('label (readers*) (writers*) expression)
;;; 
;;; readers   ::= ('readers principal (principal*))
;;; 
;;; writers  ::= ('writers principal (principal*))
;;; 
;;; principal ::= 'bottom
;;;             | 'top
;;;             | <some defined symbol>


