#lang racket
(require db
         "config.rkt"
         "config-utility.rkt")

(provide
 ; String -> DB-Connecton
 initialize-db

 ; Hash DB-Statement DB-Connection -> Hash
 insert-page-result

 ; List-of-Hash DB-Connection
 insert-page-results

 ; DB-Connection -> List-of-Vector
 get-key-url*)

; Config references
(define PATH (get-conf-at 'location))
(define TABLE (get-conf-at 'table))
(define FIELDS (get-conf-at 'fields))
(define CONSTRAINT (get-conf-at 'constraint?))


(define (create-page-results-statement d-b)
  ; how to abstract?
  (define fields->string
    (foldl (lambda (f base)
             (string-append base ", " (symbol->string (first f))))
           (symbol->string (first (first FIELDS)))
           (rest FIELDS)))
  
  (prepare d-b
           (string-append "INSERT INTO " TABLE " ("
                          fields->string ") "
                          "VALUES (" (foldr (lambda (f b)
                                              (string-append b ", ?"))
                                            "?" (rest FIELDS)) ")")))
  

; Config -> DB-Connection
(define (initialize-db)
  (define d-b (sqlite3-connect #:database PATH #:mode 'create))
  
  (unless (table-exists? d-b TABLE)
    (query-exec d-b (field*->create-query TABLE FIELDS CONSTRAINT)))
  d-b)


; Hash DB-Connection DB-Statement -> Hash
(define (insert-page-result result d-b stmt)
  (define prepped (map (lambda (r)
                         (hash-ref result r))
                       (sort (hash-keys result) symbol<?)))

  (apply query-exec
         (cons d-b
               (cons stmt prepped)))
  ; Hash to be used for ScraperState
  ; should return a struct from given result data
  (hash 'a 'test))

  
; List-of-Hash DB-Connection -> List-of-Hash
(define (insert-page-results loh d-b)
  ; Hash -> Hash
  (define (insert result)
    (insert-page-result result
                        d-b
                        (create-page-results-statement d-b)))
  
  (call-with-transaction
   d-b
   (lambda ()
     (map insert loh))
   #:isolation 'serializable))


; DB-connection -> List-of-Vector
(define (get-key-url* d-b)
  (query-rows d-b
              "SELECT key, url FROM job_posts"))


; Config -> String
; creates a insert query from Config DB fields
(define (field*->create-query table f* c?)
  ; Assoc* -> String
  (define (create-table-query f*)
    (define field (first f*))
    (cond
      [(empty? (rest f*))
       (string-append (symbol->string (first field))
                      " "
                      (second field))]
      [else
       (string-append (symbol->string (first field))
                      " "
                      (second field)
                      ", "
                      (create-table-query (rest f*)))]))
  
  (string-append "CREATE TABLE " table " "
                 "(id INTEGER PRIMARY KEY, "
                 (create-table-query f*)
                 (if c?
                     (string-append ", CONSTRAINT uniq_ "
                                    c? "UNIQUE (" c? "))")
                     ")")))

(module+ test
  (require rackunit)
  (define f* (get-conf-at 'fields))
  (define c? (get-conf-at 'constraint))
  (define t (get-conf-at 'table))
  (define TEST-CREATE-NO-CONSTRAINT
    (string-append "CREATE TABLE job_posts "
                   "(id INTEGER PRIMARY KEY, "
                   "can_apply TEXT, "
                   "company TEXT, "
                   "date TEXT, "
                   "key TEXT, "
                   "location TEXT, "
                   "title TEXT, "
                   "url TEXT)"))
  (define TEST-CREATE-CONSTRAINT
    (string-append "CREATE TABLE job_posts "
                   "(id INTEGER PRIMARY KEY, "
                   "title TEXT, "
                   "company TEXT, "
                   "location TEXT, "
                   "date TEXT, "
                   "url TEXT, "
                   "key TEXT, "
                   "can_apply TEXT, "
                   "CONSTRAINT uniq_key UNIQUE "
                   "(key))"))
  (check-equal? (field*->create-query t f* c?)
                TEST-CREATE-NO-CONSTRAINT))