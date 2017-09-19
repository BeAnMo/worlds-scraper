#lang racket
(require net/url
         xml
         "config.rkt")

(provide
 ; Reads config.rkt for the given symbol
 ; avoid searching for symbols within Assoc*
 get-config-element
 get-conf-at)

; Config is an Sexpr of Shape:
; '(scraper Html-Config
;           Json-Config)

; Json-Config is an Sexpr of Shape:
; (list 'json Json-Url-Config Json-Response-Config)

; Json-Url-Config is:
;(list 'url-params
;      (base-url [List-of-Assoc])
;      (path ... String)
;      (queries [List-of-Pair]))
      
; Config -> Sexpr or False
; retrieves an Sexpr starting with a given symbol from the given Config,
; false if not present
(define (get-config-element xe sym)
  (cond
    [(empty? xe) #f]
    [else
     (define head (first xe))
     (define tail (get-config-element (rest xe) sym))
     (cond
       [(and (symbol? head)
             (symbol=? head sym))
        xe]
       [(list? head)
        (or (get-config-element head sym)
            tail)]
       [else tail])]))

; Symbol -> List
(define (get-conf-at sym)
  ; Config Symbol -> List
  (define (get-element conf sym)
    (cond
      [(empty? conf) #f]
      [else
       (define head (first conf))
       (define tail (rest conf))
       (define recur-tail (get-element tail sym))
       (cond
         [(and (symbol? head)
               (symbol=? head sym))
          (first tail)]
         [(list? head)
          (or (get-element head sym)
              recur-tail)]
         [else recur-tail])]))

  (get-element MAIN-CONFIG sym))


(module+ test
  (require rackunit)
  (check-equal? (get-conf-at 'constraint?) #f)
  (check-equal? (get-conf-at 'meta-data)
                '((total-results totalResults)
                  (page-results results)
                  (results-length "25")))
  (check-equal? (get-conf-at 'path) '("ads" "apisearch"))
  (check-equal? (get-conf-at 'db)
                '(sqlite3
                  (location
                   "/home/bammer/projects/racket/worlds-scraper/test.db")
                  (table "job_posts")
                  (fields
                   ((title "TEXT")
                    (company "TEXT")
                    (location "TEXT")
                    (date "TEXT")
                    (url "TEXT")
                    (key "TEXT")
                    (can_apply "TEXT"))
                   (constraint? #f)))))