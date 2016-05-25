(load "framework.scm")

; Open-web-page -> hash(declassify) -> log-in -> download-data -> store-data

(define fetch-data-from-web-page
  (lambda (username password)
    (store-data
     "s-stuff.png"
     (download-data
      "strange-stuff.png"
      (log-in username (hash password))))))

(define download-data
  (lambda (file access-token)
    "Some data array"))

(define log-in
  (lambda (username passwordhash)
    '(auth 1002)))

(define hash
  (lambda (v)
    v))

(define store-data
  (lambda (filename data)
    (printf "~s: ~s ~n" filename data)))
