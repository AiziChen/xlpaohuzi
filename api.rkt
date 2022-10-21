#lang racket/base

(require net/http-easy
         uuid
         net/uri-codec
         "utils.rkt")

(define *wc-app-id* "wx14d7331491ff4a26")
(define *wc-secret* "dc543dc42c45ed28a1732b011daee3d1")

(provide
 wc-sns-oauth2
 wc-sns-oauth2-refresh-token
 wc-sns-userinfo
 app-wechat-login)


(define (wc-sns-oauth2 code)
  (define res
    (get "https://api.weixin.qq.com/sns/oauth2/access_token"
         #:params
         `((appid . ,*wc-app-id*)
           (secret . ,*wc-secret*)
           (grant_type . "authorization_code")
           (code . ,code))))
  (and (= (response-status-code res) 200)
       (response-json res)))


(define (wc-sns-oauth2-refresh-token refresh-token)
  (define res
    (get "https://api.weixin.qq.com/sns/oauth2/refresh_token"
         #:params
         `((refresh_token . ,refresh-token)
           (appid . ,*wc-app-id*)
           (grant_type . "refresh_token"))))
  (and (= (response-status-code res) 200)
       (response-json res)))


(define (wc-sns-userinfo access-token openid)
  (define res
    (get "https://api.weixin.qq.com/sns/userinfo"
         #:params
         `((access_token . ,access-token)
           (openid . ,openid))))
  (and (= (response-status-code res) 200)
       (response-json res)))


(define (app-wechat-login openid unionid wc-access-token wc-refresh-token)
  (define muuid (uuid-string))
  (define params
    `((androidId . "208a1c1b7bdb3a12")
      (appId . "10001")
      (chId . "")
      (clientId . "0")
      (deviceId . "67401041480114|89860321167553961863|90:F0:52:8D:A4:6F")
      (deviceType . "0")
      (imei . "867401041480122")
      (mac . "90:F0:52:8D:A4:6F")
      (oaid . "624b17337c6ab54ffa7155f9eb4e8581")
      (openId . ,openid)
      (subChId . "")
      (unionId . ,unionid)
      (uuid . "208a1c1b7bdb3a12")
      (wechatAccessToken . ,wc-access-token)
      (wechatAppId . ,*wc-app-id*)
      (wechatRefreshToken . ,wc-refresh-token)))
  (define sorted-params
    (sort params
          (lambda (v1 v2)
            (symbol<? (car v1) (car v2)))))
  (define encoded-params
    (alist->form sorted-params))
  (define cseconds (current-seconds))
  (define url "https://api.wxianlai.com/uc/v2/login/wechat")
  (define res
    (get url
         #:params sorted-params
         #:headers
         (hasheq 'Accept-Encoding "identity"
                 'X-QP-AppId "10001"
                 'X-QP-AppVersion "5.0.1"
                 'X-QP-ClientType "2"
                 'X-QP-Gid "0"
                 'X-QP-Nonce muuid
                 'X-QP-OS "android"
                 'X-QP-Signature
                 (generate-signature "GET"
                                     (string-append url "?" encoded-params)
                                     muuid
                                     cseconds)
                 'X-QP-Timestamp (number->string cseconds)
                 'Host "api.wxianlai.com"
                 'Connection "Keep-Alive")))
  (and (= (response-status-code res) 200)
       (response-json res)))
