#lang racket
(require html-parsing
         net/url
         json)

;; GET requests for HTML/JSON

(provide get-html
         get-json)

; String -> Xexpr
(define (get-html s)
  (html->xexp
   (get-pure-port
    (string->url s))))


; String -> JSON
(define (get-json url)
  (read-json
   (get-pure-port
    (string->url url))))
