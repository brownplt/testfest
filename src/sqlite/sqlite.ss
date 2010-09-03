#lang scheme
(require "sqlite-ffi.ss"
         (only-in (lib "foreign.ss")
                  register-finalizer))

; Struct
(define-struct (exn:sqlite exn) ())
(define-struct db (handle) #:mutable)
(define-struct statement (db handle) #:mutable)

(define (open-statement? v)
  (and (statement? v)
       (statement-handle v)
       #t))

; Contracts  
(define sqlite-datum/c
  (or/c integer? number? string? bytes? false/c))

(provide/contract
 [exn:sqlite? (any/c . -> . boolean?)]
 [statement? (any/c . -> . boolean?)]
 [open-statement? (any/c . -> . boolean?)]
 [db? (any/c . -> . boolean?)]
 [sqlite-datum/c contract?]
 
 [open ((or/c path? (symbols ':memory: ':temp:)) . -> . db?)]
 [close (db? . -> . void?)]
 
 [errmsg (db? . -> . string?)]
 [last-insert-rowid (db? . -> . integer?)]
 [changes-count (db? . -> . integer?)]
 [total-changes-count (db? . -> . integer?)]
 
 [prepare (db? string? . -> . open-statement?)]
 [load-params ((open-statement?) () #:rest (listof sqlite-datum/c) . ->* . void?)]
 [step (open-statement? . -> . (or/c (vectorof sqlite-datum/c) false/c))]
 [step* (open-statement? . -> . (listof (vectorof sqlite-datum/c)))]
 [run ((open-statement?) () #:rest (listof sqlite-datum/c) . ->* . void?)] ; params
 [reset (open-statement? . -> . void?)]
 [finalize (open-statement? . -> . void?)]
 [statement-names (open-statement? . -> . (vectorof string?))]
 
 [exec ((db? string? ((vectorof string?) (vectorof sqlite-datum/c) . -> . integer?)) () #:rest (listof sqlite-datum/c) . ->* . void?)]
 [exec/ignore ((db? string?) () #:rest (listof sqlite-datum/c) . ->* . void?)]
 [insert ((db? string?) () #:rest (listof sqlite-datum/c) . ->* . integer?)]
 [select ((db? string?) () #:rest (listof sqlite-datum/c) . ->* . (listof (vectorof sqlite-datum/c)))]
 
 [with-transaction* (db? (symbols 'none 'deferred 'immediate 'exclusive) (any/c . -> . any/c) . -> . any/c)])

(provide with-transaction/lock
         with-transaction)

; Library Helpers
(define (wrap-finalizer o f)
  (register-finalizer o f)
  o)

(define (sqlite-error fmt . args)
  (raise (make-exn:sqlite
          (string->immutable-string
           (format "SQLite Error: ~a"
                   (apply format fmt args)))
          (current-continuation-marks))))

;; handle-status : (U db #f) integer -> integer
;;
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status db s)
  (if (or (= s SQLITE_OK)
          (= s SQLITE_ROW)
          (= s SQLITE_DONE))
      s
      (sqlite-error "~a" (lookup-status-message db s))))

;; lookup-status-message : (U db #f) integer -> string
(define (lookup-status-message db s)
  (if (and (eq? s SQLITE_ERROR) db)
      (errmsg db)
      (cdr (assoc s
                  `([,SQLITE_ERROR . "Generic error, perhaps call errmsg?"]
                    [,SQLITE_INTERNAL . "An internal logic error in SQLite"]
                    [,SQLITE_PERM . "Access permission denied"]
                    [,SQLITE_ABORT . "Callback routine requested an abort"]
                    [,SQLITE_BUSY . "The database file is locked"]
                    [,SQLITE_LOCKED . "table in the database is locked"]
                    [,SQLITE_NOMEM . "A malloc() failed"]
                    [,SQLITE_READONLY . "Attempt to write a readonly database"]
                    [,SQLITE_INTERRUPT . "Operation terminated by sqlite3_interrupt()"]
                    [,SQLITE_IOERR . "Some kind of disk I/O error occurred"]
                    [,SQLITE_CORRUPT . "The database disk image is malformed"]
                    [,SQLITE_NOTFOUND . "(Internal Only) Table or record not found"]
                    [,SQLITE_FULL . "Insertion failed because database is full"]
                    [,SQLITE_CANTOPEN . "Unable to open the database file"]
                    [,SQLITE_PROTOCOL . "Database lock protocol error"]
                    [,SQLITE_EMPTY . "Database is empty"]
                    [,SQLITE_SCHEMA . "The database schema changed"]
                    [,SQLITE_TOOBIG . "Too much data for one row of a table"]
                    [,SQLITE_CONSTRAINT . "Abort due to contraint violation"]
                    [,SQLITE_MISMATCH . "Data type mismatch"]
                    [,SQLITE_MISUSE . "Library used incorrectly"]
                    [,SQLITE_NOLFS . "Uses OS features not supported on host"]
                    [,SQLITE_AUTH . "Authorization denied"]
                    [,SQLITE_FORMAT . "Auxiliary database format error"]
                    [,SQLITE_RANGE . "2nd parameter to sqlite3_bind out of range"]
                    [,SQLITE_NOTADB . "File opened that is not a database file"])))))

; FFI Wrappers
(define (open db-path)
  (let*-values
      ([(db-path)
        (cond
          [(symbol? db-path)
           (case db-path
             ; Private, temporary in-memory
             [(:memory:) #":memory:"]
             ; Private, temporary on-disk
             [(:temp:) #""])]                   
          [(relative-path? db-path)
           (path->bytes (build-path (current-directory) db-path))]
          [else
           (path->bytes db-path)])]
       [(db-ptr open-status) (sqlite3_open db-path)]
       [(the-db)
        (wrap-finalizer 
         (make-db db-ptr)
         close)])
    (when (handle-status the-db open-status)
      the-db)))

(define (close db)
  (let ([o-handle (db-handle db)])
    (set-db-handle! db #f)
    (when o-handle
      (handle-status db (sqlite3_close o-handle)))
    (void)))

(define (prepare db sql)
  (let*-values 
      ([(stmt prep-status tail)
        (sqlite3_prepare_v2 (db-handle db) sql)]
       [(the-stmt) (wrap-finalizer (make-statement db stmt)
                                   finalize)])
    (when (string=? sql tail)
      (sqlite-error "Syntax error in ~s" tail))
    (when (not (zero? (string-length tail)))
      (sqlite-error "You should only prepare one statement at a time! ~s" tail))
    (when (handle-status db prep-status)
      (if stmt
          the-stmt
          ;; the pointer is null; SQLite didn't raise an
          ;; error but should have!
          (sqlite-error "sqlite3_prepare_v2 returned a NULL pointer")))))

(define (load-params stmt . params)
  (let* ((handle (statement-handle stmt))
         (parameter-count (sqlite3_bind_parameter_count handle)))
    (when (not (= (length params) parameter-count))
      (raise-mismatch-error
       'load-params
       (format "Given ~a params when statement requires ~a params" (length params) parameter-count)
       params))
    (begin
      (reset stmt)
      (let loop ((i 1) (params params))
        (if (null? params)
            (void)
            (begin
              (let* ([param (car params)])
                (handle-status
                 #f
                 (cond
                   [(integer? param)
                    (sqlite3_bind_int64 handle i param)]
                   [(number? param)
                    (sqlite3_bind_double handle i param)]
                   [(string? param)
                    (sqlite3_bind_text handle i param)]
                   [(bytes? param)
                    (sqlite3_bind_blob handle i param)]
                   [else
                    (sqlite3_bind_null handle i)])))
              (loop (add1 i) (cdr params))))))))

(define (step stmt)
  (let ([s (handle-status
            #f
            (sqlite3_step (statement-handle stmt)))]
        [string-up
         (lambda (s)
           (if s (string-upcase s) #f))])
    (cond
      [(= s SQLITE_ROW)
       (let* ((cols (sqlite3_column_count (statement-handle stmt)))
              (vec (make-vector cols))
              (handle (statement-handle stmt)))
         (let loop ((i 0))
           (if (= i cols)
               vec
               (begin
                 (vector-set!
                  vec
                  i
                  (let ([typ (sqlite3_column_type handle i)])
                    (cond
                      [(= typ SQLITE_NULL)
                       #f]
                      [(= typ SQLITE_INTEGER)
                       (sqlite3_column_int64 handle i)]
                      [(= typ SQLITE_FLOAT)
                       (sqlite3_column_double handle i)]
                      [(= typ SQLITE_TEXT)
                       (sqlite3_column_text handle i)]
                      [(= typ SQLITE_BLOB)
                       (sqlite3_column_blob handle i)]
                      [else
                       ; XXX This should never really be used
                       (match (string-up (sqlite3_column_decltype handle i))
                         ["NULL" 
                          #f]
                         ["INTEGER"
                          (sqlite3_column_int64 handle i)]
                         [(or "FLOAT" "REAL")
                          (sqlite3_column_double handle i)]
                         [(or #f "STRING" "TEXT" (regexp "^VARCHAR *\\(.+\\)$") (regexp "^CHARACTER VARYING *\\(.+\\)$"))
                          (sqlite3_column_text handle i)]
                         ["BLOB"
                          (sqlite3_column_blob handle i)])])))
                 (loop (add1 i))))))]
      [(= s SQLITE_DONE)
       #f])))

(define (reset stmt)
  (handle-status #f (sqlite3_reset (statement-handle stmt)))
  (void))

(define (finalize stmt)
  (let ([o-handle (statement-handle stmt)])
    (set-statement-handle! stmt #f)
    (when o-handle
      (handle-status #f (sqlite3_finalize o-handle)))
    (void)))

(define (lock-type->string lock-type)
  (case lock-type
    [(none) ""]
    [(deferred) "DEFERRED"]
    [(immediate) "IMMEDIATE"]
    [(exclusive) "EXCLUSIVE"]))

;; with-transaction* : db symbol (ec -> 'a) -> 'a
(define (with-transaction* db lock-type body-f)
  (let ((end #f))
    (dynamic-wind
     (lambda ()
       (set! end (lambda ()
                   (exec/ignore db "ROLLBACK TRANSACTION")))
       (exec/ignore db
                    (format "BEGIN ~a TRANSACTION"
                            (lock-type->string lock-type))))
     (lambda ()
       (let/ec fail
         (begin0
           (body-f fail)
           (set! end
                 (lambda ()
                   (exec/ignore db "COMMIT TRANSACTION"))))))
     (lambda ()
       (end)))))

(define-syntax with-transaction/lock
  (syntax-rules ()
    [(_ (db lock-type fail) body ...)
     (with-transaction* db 'lock-type (lambda (fail) body ...))]))
(define-syntax with-transaction
  (syntax-rules ()
    [(_ (db fail) body ...)
     (with-transaction/lock (db none fail) body ...)]))

(define (errmsg db)
  (sqlite3_errmsg (db-handle db)))
(define (changes-count db)
  (sqlite3_changes (db-handle db)))
(define (total-changes-count db)
  (sqlite3_total_changes (db-handle db)))
(define (last-insert-rowid db)
  (sqlite3_last_insert_rowid (db-handle db)))

; Scheme functions
(define (step* stmt)
  (let loop ([r (list)])
    (let ([c (step stmt)])
      (if (not c)
          (reverse r)
          (loop (cons c r))))))


(define (insert db sql . params)
  (apply exec/ignore db sql params)
  (last-insert-rowid db))

(define (select db sql . params)
  (let ([results empty]
        [names #f])
    (apply exec db sql 
           (lambda (ns row)
             (set! names ns)
             (set! results (list* row results))
             0)
           params)
    (if names
        (list* names (reverse results))
        empty)))

(define (statement-names stmt)
  (define handle (statement-handle stmt))
  (list->vector 
   (for/list ([i (in-range (sqlite3_column_count handle))])
     (sqlite3_column_name handle i))))

(define (run* statement callback . params)
  (when (not (empty? params))
    (apply load-params statement params))
  (let ([names (statement-names statement)])
    (let loop ([row (step statement)])
      (when row
        (if (zero? (callback names row))
            (loop (step statement))
            (sqlite-error "Callback routine requested an abort")))))
  (void))

(define (run statement . params)
  (apply run* statement (lambda (names row) (void)) params))

(define (exec db sql callback . params)
  (define statement (prepare db sql))
  (apply run* statement callback params)
  (finalize statement))
(define (exec/ignore db sql . params)
  (apply exec db sql (lambda (names row) 0) params))