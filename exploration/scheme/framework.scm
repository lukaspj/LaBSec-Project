'Skip__IFC__Check

(load "checker.scm")

(define label
  (lambda (label val)
    val))

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
    (check-program program 'bottom alist-mt)))






;;; label bnf proposal
;;; 
;;; label     ::= ('label readers* writers*)
;;; 
;;; readers   ::= ('readers principal (principal*))
;;; 
;;; writers  ::= ('writers principal (principal*))
;;; 
;;; principal ::= 'bottom
;;;             | 'top
;;;             | <some defined symbol>


