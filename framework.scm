'Skip__IFC__Check

(load "./checker.scm")

(define label-expression
  (lambda (label expr)
    expr))

(define principals
  (lambda p
    p))

(define-syntax label-lambda
  (lambda (x)
    (syntax-case x ()
      ((_ begin ((i v) ...) end e1 e2 ...)
       (syntax (lambda (i ...) e1 e2 ...))
      ))))

(define verify-constraints
  (lambda (file)
    (check-file file '(label () (confidentiality . 0))
                (get-env-from-predefs labels_of_predefined_functions
                                      alist-mt))))

(define get-env-from-predefs
  (lambda (predefs env)
    (cond
     [(null? predefs)
      env]
     [else
      (alist-extend (caar predefs)
                    (cdar predefs)
                    (get-env-from-predefs (cdr predefs) env))])))
    

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


