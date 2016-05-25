(load "framework.scm")

; Open-web-page -> hash(declassify) -> log-in -> download-data -> store-data

;; (define fetch-data-from-web-page
;;   (lambda (username password)
;;     (store-data
;;      "s-stuff.png"
;;      (download-data
;;       "strange-stuff.png"
;;       (log-in username (hash password))))))

;; (define download-data
;;   (lambda (file access-token)
;;     "Some data array"))

;; (define log-in
;;   (lambda (username passwordhash)
;;     '(auth 1002)))

;; (define hash
;;   (lambda (v)
;;     v))

;; (define store-data
;;   (lambda (filename data)
;;     (printf "~s: ~s ~n" filename data)))

(define fetch-data-from-web-page
  (label-lambda '(label () ())
                ([username '(label () ())]
                 [password '(label () ())])
                '(label () ())
                (store-data
                 "s-stuff.png"
                 (download-data
                  "strange-stuff.png"
                  (log-in username (hash password))))))

(define download-data
  (label-lambda '(label () ())
                ([file '(label () ())]
                 [access-token '(label () ())])
                '(label () ())
                "Some data array"))

(define log-in
  (label-lambda '(label () ())
                ([username '(label () ())]
                 [password '(label () ())])
                '(label () ())
                '(auth 1002)))

(define hash
  (label-lambda '(label () ())
                ([v '(label () ())])
                '(label () ())
                v))

(define store-data
  (label-lambda '(label () ())
                ([filename '(label () ())]
                 [data '(label () ())])
                '(label () ())
                (printf "~s: ~s ~n" filename data)))
