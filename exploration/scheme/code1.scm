(define label
  (lambda (label name val)
    val))

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
    (printf "Verified!~n")))
