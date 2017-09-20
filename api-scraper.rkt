#lang racket
(require "http-requests.rkt"
         "storage.rkt"
         "config.rkt"
         "config-utility.rkt"
         2htdp/universe
         2htdp/image
         net/url)


;(provide
 ; for scraping a JSON API given a valid Config
 ;scrape-json)


; paginated scraper:
; - base url: String
; - number of results: Number
;   number of entries per URL
; - page adjustment: [Number -> Number]
;   allow calculating total number of pages once
;   the total number of results has been retrieved
; - query terms: collection of terms to search for
; - storage: String|[String -> ...] allows the storage/access of scraped info
;   also differentiate between stored data & in memory data
; - limiter: ... allows the control of requests per second


(struct scraped [url key title])
; Scraped is a struct
; (make-scraped String String String)

; ScraperState is one of:
; '()
; (cons Scraped ScraperState)

;;;; FUNCTIONS
; Config -> ScraperState
; launches the scraper given a valid Config
;(define (main cfg)
;  ; DB-Connection
;  (define db-init cfg)
;
;  ; Hash
;  (define first-page (get-first-page cfg db-init))
;
;  ;
;
;  (big-bang ss
;            [on-tick scrape-page 1 pages-len] ; pages-len is limit of on-tick
;            [to-draw render-results]))



; Config -> [Number -> String]
; allows for creation of a new API url based on a page or result number
(define (build-api-url js-cfg)
  (define base (get-conf-at 'base-url))
  (define path (get-conf-at 'path))
  (define queries (get-conf-at 'queries))
  
  ; Attr* Symbol -> String
  (define (attr-val loa sym)
    (second (assoc sym loa)))
  
  (lambda (page-num)
    (url->string
     (url (attr-val base 'protocol)
          #f ; user
          (attr-val base 'host)
          #f ; port
          #t ; path-absolute?
          (map (lambda (p)
                 (path/param p '()))
               path)
          (map (lambda (q)
                 ; car must be used with pairs
                 (if (symbol=? (car q) 'start)
                     (cons 'start (number->string page-num))
                     q))
               queries)
          #f)))) ;fragment

(module+ test
  (require rackunit)
  (define TEST-LINK
    (string-append
     "http://api.indeed.com/ads/apisearch?publisher=2939261607256398"
     "&q=web+developer&l=nashville%2Ctn&radius=50&start=0&limit=25"
     "&filter=1&co=us&userip=1.2.3.4&useragent=Mozilla%2F%2F4.0%28"
     "Firefox%29&v=2&format=json"))
  (define build (build-api-url MAIN-CONFIG))
  (check-equal? (build 0) TEST-LINK))


; Attr* JSON -> Hash
; uses attr* from 'response-targets in Config to create
; a hash the from API response
(define (get-results-data cfg js)
  (define targets (get-conf-at 'results-data))

  ; Symbol -> List
  (define (key-attr->assoc sy)
    (assoc sy targets))
  
  ; Atom -> String
  (define (enforce-string val)
    (cond
      [(boolean? val)
       (boolean->string val)]
      [(symbol? val)
       (symbol->string val)]
      [else val]))

  ; specialize for clause with "in-list"
  (for/hash ([k (in-list targets)]) ; for clause not specialized?
    (define keys-attr (key-attr->assoc (first k)))
    (values (first keys-attr)
            (enforce-string
             (hash-ref js (second keys-attr))))))


; Config DB-Connection -> Hash
(define (get-first-page/save-results cfg d-b)
  (define meta-data (get-conf-at 'meta-data))
  (define total-results-target (second (assoc 'total-results meta-data)))
  (define results-target (second (assoc 'page-results meta-data)))

  ; Number -> String
  (define base-url (build-api-url cfg))

  ; JSON
  (define response-json (get-json (base-url 1)))
  (displayln (hash-ref response-json 'totalResults))
  ; Number
  (define pages-len (hash-ref response-json
                              total-results-target))

  ; List-of-Scraped
  (define results (map (lambda (result)
                         (get-results-data cfg result))
                       (hash-ref response-json results-target)))

  ; writes to DB
  (define scraped-state (insert-page-results results d-b))

  ; return Hash
  (hash 'db d-b
        'length pages-len
        'scraped scraped-state
        'url base-url))


; Boolean -> String
(define (boolean->string bool)
  (if bool "true" "false"))


;; Hash -> List-of-Hash
;(define (rest-requests.v2 ha)
;  (define results-len (hash-ref ha 'results-len))
;  (define pages (- (ceiling (/ (hash-ref ha 'length)
;                               'results-len)) 1))
;  ; Number -> List-of-Hash
;  (define (get-results n)
;    (hash-ref (get-json ((hash-ref ha 'url) n)) 'results))
;  ; Number -> List-of-List-of-Hash
;  (define (get-results* n)
;    (save-results->key* (get-results (* results-len (+ 1 n)))
;                        (hash-ref ha 'db)))
;  ;(cons (hash-ref ha 'keys)
;  (foldr (lambda (rest-keys first-keys)
;           (append first-keys rest-keys))
;         (hash-ref ha 'keys)
;         (build-list pages get-results*))) ; first page twice?


; JSON DB-Connection -> List-of-Hash
;(define (save-results->key* js d-b)
;  (define posts (map create-job-post js))
;  (insert-post* posts d-b))
