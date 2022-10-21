#lang racket/base

(require file/md5)

(provide
 generate-signature)


(define (generate-signature method url uuid cseconds)
  (md5
   (string-append method
                  "X-QP-AppId:10001"
                  "X-QP-Nonce:" uuid
                  "X-QP-Timestamp:" (number->string cseconds)
                  url
                  "3c6e0b8a9c15224a8228b9a98ca1531d")))
