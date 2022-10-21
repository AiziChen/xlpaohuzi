#lang racket/base

(require koyo/json
         uuid
         web-server/servlet
         web-server/http/redirect
         json
         racket/format
         racket/string
         "utils.rkt"
         "api.rkt")


(provide
 get-signature
 generate-token-by-code
 generate-token-by-refresh-token)

(define (get-signature req)
  (let* ([raw (request-post-data/raw req)]
         [data (bytes->jsexpr raw)]
         [method (hash-ref data 'method #f)]
         [url (hash-ref data 'url #f)])
    (cond
      [(and method url)
       (define cseconds (current-milliseconds))
       (define muuid (uuid-string))
       (define sig (generate-signature method url muuid cseconds))
       (response/json
        (hasheq 'code 200
                'data (hasheq 'X-QP-Timestamp cseconds
                              'X-QP-Nonce muuid
                              'X-QP-Signature sig)))]
      [else
       (response/json
        (hasheq 'code 500
                'msg "parameter `method` and `url` must specify"
                'data (hasheq)))])))

(define (generate-token-by-code req code)
  (cond
    [(non-empty-string? code)
     (define auth-rs (wc-sns-oauth2 code))
     (cond
       [(and auth-rs (hash-ref auth-rs 'access_token #f))
        (define access-token (hash-ref auth-rs 'access_token))
        (define refresh-token (hash-ref auth-rs 'refresh_token))
        (define openid (hash-ref auth-rs 'openid))
        (define user-rs (wc-sns-userinfo access-token openid))
        (cond
          [(and user-rs (hash-ref user-rs 'unionid #f))
           =>
           (lambda (unionid)
             (response/json
              (hasheq 'code 200
                      'data (hasheq 'openid openid
                                    'access-token access-token
                                    'refresh-token refresh-token
                                    'unionid unionid))))]
          [else
           (response/json
            (hasheq 'code 500
                    'msg "get wechat user information error"))])]
       [else
        (response/json
         (hasheq 'code 500
                 'msg "get wechat access-token error"
                 'data (hasheq)))])]
    [else
     (response/json
      (hasheq 'code 500
              'msg "parameter `code` must specify or parameter `code` can not be null"
              'data (hasheq)))]))

(define (generate-token-by-refresh-token req refresh-token)
  (cond
    [(non-empty-string? refresh-token)
     (define auth-rs (wc-sns-oauth2-refresh-token refresh-token))
     (cond
       [(and auth-rs (hash-ref auth-rs 'access_token #f))
        (define access-token (hash-ref auth-rs 'access_token))
        (define refresh-token (hash-ref auth-rs 'refresh_token))
        (define openid (hash-ref auth-rs 'openid))
        (define user-rs (wc-sns-userinfo access-token openid))
        (cond
          [(and user-rs (hash-ref user-rs 'unionid #f))
           =>
           (lambda (unionid)
             (response/json
              (hasheq 'code 200
                      'data (hasheq 'openid openid
                                    'access-token access-token
                                    'refresh-token refresh-token
                                    'unionid unionid))))]
          [else
           (response/json
            (hasheq 'code 500
                    'msg "get wechat user information error"))])
        (response/json
         (hasheq 'code 200
                 'data (hasheq 'openid openid
                               'access-token access-token
                               'refresh-token refresh-token)))]
       [else
        (response/json
         (hasheq 'code 500
                 'msg "refresh wechat access-token error"
                 'data (hasheq)))])]
    [else
     (response/json
      (hasheq 'code 500
              'msg "parameter `code` must specify or parameter `code` can not be null"
              'data (hasheq)))]))
