#lang racket
(require db
         "config.rkt"
         "config-utility.rkt")

(provide
 ; String -> DB-Connecton
 initialize-db

 ; Hash DB-Statement DB-Connection -> Hash
 insert-post

 ; List-of-Hash DB-Connection
 insert-post*

 ; DB-Connection -> List-of-Vector
 get-key-url*)


; DB-Connection -> DB-Statement
(define (post-statement d-b)
  (prepare d-b
           (string-append
            "INSERT INTO job_posts "
            "(title, company, location, "
            "date, url, key, can_apply) "
            "VALUES (?, ?, ?, ?, ?, ?, ?)")))


; Config -> DB-Connection
(define (initialize-db db-cfg)
  (define path (get-conf-at 'location))
  (define d-b (sqlite3-connect #:database path #:mode 'create))

  (define table-name (get-conf-at 'table))
  (define fields (get-conf-at 'fields))
  (define constraint (get-conf-at 'constraint?))
  
  (unless (table-exists? d-b table-name)
    (query-exec d-b (field*->create-query table-name fields constraint)))
  d-b)


; Hash DB -> Hash
(define (insert-post.v1 post d-b)
  (define key (hash-ref post 'key))
  (define url (hash-ref post 'url))
  (query-exec d-b
              (string-append "INSERT INTO job_posts "
                             "(title, company, location, "
                             "date, url, key, can_apply) "
                             "VALUES(?, ?, ?, ?, ?, ?, ?)")
              (hash-ref post 'title)
              (hash-ref post 'company)
              (hash-ref post 'location)
              (hash-ref post 'date)
              url
              key
              (boolean->string (hash-ref post 'can_apply)))
  (hash (string->symbol key) url))

(define (insert-post post stmt d-b)
  (define key (hash-ref post 'key))
  (define url (hash-ref post 'url))
  
  (query-exec d-b
              stmt
              (hash-ref post 'title)
              (hash-ref post 'company)
              (hash-ref post 'location)
              (hash-ref post 'date)
              url
              key
              (boolean->string (hash-ref post 'can_apply)))
  (hash (string->symbol key) url))
  
; List-of-Hash DB-Connection -> List-of-Hash
(define (insert-post* loh d-b)
  (call-with-transaction
   d-b
   (lambda ()
     (map (lambda (post)
            (insert-post post (post-statement d-b) d-b)) loh))
   #:isolation 'serializable))


; DB-connection -> List-of-Vector
(define (get-key-url* d-b)
  (query-rows d-b
              "SELECT key, url FROM job_posts"))


; Boolean -> String
(define (boolean->string bool)
  (if bool "true" "false"))


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
                   "title TEXT, "
                   "company TEXT, "
                   "location TEXT, "
                   "date TEXT, "
                   "url TEXT, "
                   "key TEXT, "
                   "can_apply TEXT)"))
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