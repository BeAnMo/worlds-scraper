#lang racket

;; Scraping inputs

(provide MAIN-CONFIG
         JSON-CONFIG
         DB-CONFIG)

; Config is one of:
; (list Symbol Atom)
; (list Symbol [List-of-Atom])
; (list Symbol [List-of-Assoc])
; (list Symbol Config)
; (list Symbol [List-of-Config])

; should be able to convert Config to JSON/XML

(define DB-CONFIG
  '(sqlite3 (location "/home/bammer/projects/racket/worlds-scraper/test.db")
            (table "job_posts")
            (fields ((title "TEXT")
                     (company "TEXT")
                     (location "TEXT")
                     (date "TEXT")
                     (url "TEXT")
                     (key "TEXT")
                     (can_apply "TEXT"))
                    (constraint? #f))))
            
                     
                     
                     
; use sqlite? if false, no config
; conifg:
; table name & table fields
; only concerned with storing data now

(define STORAGE-CONFIG
  `(db ,DB-CONFIG))

; search and cities are query inputs
(define SEARCH-TERMS '("web+developer"))
(define CITIES '(("atlanta" "ga")
                 ("austin" "tx")
                 ("chicago" "il")
                 ("denver" "co")
                 ("nashville" "tn")
                 ("new+york" "ny")
                 ("portland" "or")
                 ("san+francisco" "ca")
                 ("seattle" "wa")))

; html scraping
(define HTML-META-DATA
  '((base-url "...")))

(define HTML-TARGETS
  ; should be able to check for either:
  ; all elements of given tag name
  ; attribute-name + attribute-value - id="thisId"
  '((id "job_summary")))


; json scraping
(define JSON-URL-CONFIG
  '(url-params (base-url ((protocol "http") ; http for api?
                          (host "api.indeed.com")))
               ; map with path/param
               (path ("ads" "apisearch")) 
               (queries ((publisher . "2939261607256398")
                         (q . "web developer")
                         (l . "nashville,tn")
                         (radius . "50")
                         (start . "0")
                         (limit . "25")
                         (filter . "1")
                         (co . "us")
                         (userip . "1.2.3.4")
                         (useragent . "Mozilla//4.0(Firefox)")
                         (v . "2")
                         (format . "json")))))


(define JSON-RES-CONFIG
  ; scraper key + actual json-key
  '(response-targets (meta-data ((total-results totalResults)
                                 (page-results results)
                                 (results-length "25")))
                     (results-data ((title jobtitle)
                                    (company company)
                                    (location formattedLocation)
                                    (date date)
                                    (url url)
                                    (key jobkey)
                                    (can_apply indeedApply)))))

(define JSON-CONFIG
  `(json ,JSON-URL-CONFIG
         ,JSON-RES-CONFIG))

(define MAIN-CONFIG
  `(scraper ,STORAGE-CONFIG
            ,JSON-CONFIG))