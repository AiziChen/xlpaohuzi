#lang racket/base

(require file/md5)

(provide
 generate-signature
 alist->form)


(define (generate-signature method url uuid cseconds)
  (bytes->string/utf-8
   (md5
    (string-append method
                   "X-QP-AppId:10001"
                   "X-QP-Nonce:" uuid
                   "X-QP-Timestamp:" (number->string cseconds)
                   url
                   "3c6e0b8a9c15224a8228b9a98ca1531d"))))

(define (alist->form alist)
  (cond
    [(null? alist) ""]
    [(null? (cdr alist))
     (string-append (symbol->string (caar alist))
                    "="
                    (cdar alist))]
    [else
     (string-append (symbol->string (caar alist))
                    "="
                    (cdar alist)
                    "&"
                    (alist->form (cdr alist)))]))
