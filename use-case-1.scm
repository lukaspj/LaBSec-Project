(load "framework.scm")
(load "rsa-secure.scm")

;; Open-web-page -> hash(declassify) -> log-in -> download-data -> store-data
;; 0 : all (public)
;; 1 : users
;; 2 : mods
;; 3 : admins

;; (define fetch-data-from-web-page
;;   (lambda (username password)
;;     (store-data
;;      "s-stuff.png"
;;      (download-data
;;       "strange-stuff.png"
;;       (log-in username (hash password))))))

;; (define download-data
;;   (lambda (file access-token)
;;     1201203911239129812398123918239129812938129388124071274087019327859018273590132))

;; (define log-in
;;   (lambda (username passwordhash)
;;     '(auth 1002)))

;; (define hash
;;   (lambda (v)
;;     v))

;; (define store-data
;;   (lambda (filename data)
;;     (printf "~s: ~s ~n" filename data)))

(define download-user-data
  (label-lambda '(label (confidentiality . 0))
                ([file '(label (confidentiality . 1))]
                 [access-token '(label (confidentiality . 1))])
                '(label (confidentiality . 1))
                1201203911239129812398123918239129812938129388124071274087019327859018273590132))

(define log-in
  (label-lambda '(label (confidentiality . 0))
                ([username '(label (confidentiality . 0))]
                 [passwordhash '(label (confidentiality . 3))])
                '(label (confidentiality . 1))
                '(auth 1002)))

(define hash
  (label-lambda '(label (confidentiality . 0))
                ([v '(label)])
                '(label (confidentiality . 3))
                (declassify '(label (confidentiality . 3)) v)))

(define store-data
  (label-lambda '(label (confidentiality . 0))
                ([filename '(label (confidentiality . 0))]
                 [data '(label (confidentiality . 0))])
                '(label (confidentiality . 0))
                (declassify '(label (confidentiality . 0))
                            (printf "~s: ~s ~n" filename data)))) ;; Fix need of declassify here

(define fetch-data-from-web-page
  (label-lambda '(label (confidentiality . 0))
                ([username '(label (confidentiality . 0))]
                 [password '(label)])
                '(label (confidentiality . 1))
                (store-data
                 "s-stuff.png"
                 (download-user-data
                           "strange-stuff.png"
                           (log-in username
                                   (hash password)))
                          public-key)))
