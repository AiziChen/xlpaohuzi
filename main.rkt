#lang racket/base

(require koyo/dispatch
         koyo/url
         koyo/cors
         koyo/static
         web-server/dispatch
         web-server/web-server
         web-server/servlet-dispatch
         web-server/http/json
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         racket/list
         "abstract.rkt")

(define-values (dispatch url roles)
  (dispatch-rules+roles
   [("") (lambda (_) (response/jsexpr "server is on."))]
   [("api" "get-signature")
    #:method "post"
    get-signature]

   [("api" "generate-token-by-code" (string-arg))
    #:method "get"
    generate-token]

   [("api" "generate-token-by-refresh-token" (string-arg))
    #:method "get"
    generate-token-by-refresh-token]))



(current-cors-origin "*")

(define (stack handler)
  (wrap-cors handler))


(define dispatchers
  (list
   (dispatch/servlet (stack dispatch))
   (make-static-dispatcher "static")))

(define stop
  (serve
   #:dispatch (apply sequencer:make (filter-map values dispatchers))
   #:listen-ip #f
   #:port 1700))

(with-handlers
  ([exn:break? (lambda (_) (stop))])
  (sync/enable-break never-evt))
 

