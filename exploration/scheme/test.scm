;; (define label-lambda
;;   (lambda (labels body)
;;     body))


;; (define test
;;   (label-lambda '((x . (label () ())))
;;                 (lambda (x) x)))

(define-syntax label-lambda
  (syntax-rules ()
    ((label-lambda <params>)
     (printf "~s~n" (quote <params>)))))

(define test
  (label-lambda (x '(label () ())
                 y '(label () ()))))

(define-syntax lambda-let
  (lambda (x)
    (syntax-case x ()
      ((_ ((i v) ...) e1 e2 ...)
       (syntax ((lambda (i ...) e1 e2 ...) v ...)))
      )))
      ;((_ name ((i v) ...) e1 e2 ...)
       ;(syntax ((letrec ((name (lambda (i ...) e1 e2 ...))) name)
        ;        v ...))))))

(lambda-let ([a '(label () ())])
            (printf "~s~n" a))



(define-syntax label-lambda
  (lambda (x)
    (syntax-case x ()
      ((_ begin ((i v) ...) end e1 e2 ...)
       (syntax (lambda (i ...) e1 e2 ...))
      ))))

(define foo
  (label-lambda '(label () ())
                ([a '(label () ())]
                 [b '(label () ())])
                '(label () ())
                (printf "foo ~s ~s ~n" a b)))
