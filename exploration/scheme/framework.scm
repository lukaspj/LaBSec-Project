'Skip__IFC__Check

(load "checker.scm")

(define label-expression
  (lambda (label expr)
    expr))

(define principals
  (lambda p
    p))

;;; reads an entire file as a list of Scheme data
;;; use: (read-file "filename.scm")
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))

(define verify-constraints
  (lambda (file)
    (verify (read-file file))))

(define verify
  (lambda (program)
    (check-program program '(label () ()) alist-mt)))






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


