#lang racket
(require 
 "types.rkt"
 "sqlite/sqlite.ss")

(provide 
 let-prepare
 (all-from-out "types.rkt"))

(define db #f)

(provide/contract (set-db! (db? . -> . any)))
(define (set-db! db_)
  (if db
      (error 'set-db! "set-db! already called")
      (set! db db_)))

(provide init-db!)
(define (init-db!)
  (exec/ignore db "CREATE TABLE user (id INTEGER PRIMARY KEY, name STRING UNIQUE, enabled INTEGER, password_hash TEXT, admin INTEGER)")
  (exec/ignore db "CREATE TABLE assignment (id INTEGER PRIMARY KEY, name STRING UNIQUE, enabled INTEGER, test_lang TEXT, solution_lang TEXT, single_test_suite INTEGER, solution STRING, interface STRING)")

  (exec/ignore db "CREATE TABLE test_suite (id INTEGER PRIMARY KEY, user_id INTEGER, asgn_name STRING, submission STRING, time INTEGER, status STRING, status_text STRING)")
  (exec/ignore db "CREATE TABLE solution (id INTEGER PRIMARY KEY, user_id INTEGER, asgn_name STRING, submission STRING, time INTEGER, status STRING, status_text)")
  (exec/ignore db "CREATE TABLE report (id INTEGER PRIMARY KEY, test_suite_id INTEGER, solution_id INTEGER, assignment_id INTEGER, time INTEGER, output_id STRING, success INTEGER)"))

(define (db->solution vec)
  (match vec
    [`#(,(? integer? id) ,(? integer? user-id) ,(? string? asgn-name)
                         ,(? string? submission) ,(? integer? time) 
                         ,(? string? status) ,(? string? status-text))
     (solution
      id user-id asgn-name
      submission
      time
      (string->symbol status)
      status-text)]
    [_ #f]))


(define-syntax (let-prepare stx)
  (syntax-case stx ()
    [(_ ([id str] ...) body ...)
     #'(let ([id (prepare db str)] ...)
       (dynamic-wind
        void
        (lambda () body ...)
        (lambda ()
          (finalize id) ...)))]))

(define ((equal-to? x) y)
  (equal? x y))

(define (db-boolean? x)
  (and (integer? x) (or (= x 0) (= x 1))))

(define (db->boolean x)
  (match x
    [0 #f]
    [1 #t]
    [_ (error 'db->boolean "unexpected ~a" x)]))

(define (boolean->db x)
  (match x
    [#t 1]
    [#f 0]
    [_ (error 'boolean->db "unexpected ~a" x)]))

(define (db->user vec)
  (match vec
    [`#(,(? integer? id) ,(? string? name) ,(? db-boolean? enabled)
                         ,(? string? password-hash) ,(? db-boolean? admin))
     (user id name (db->boolean enabled) password-hash (db->boolean admin))]
    [_ (error 'db->user "unexpected ~s" vec)]))

(define (db->asgn vec)
  (match vec
    [`#(,(? integer? id) ,(? string? name) ,(? db-boolean? enabled) 
                         ,(? string? test-lang) ,(? string? soln-lang)
                         ,(? db-boolean? single-test-suite?) ,(? string? solution)
                         ,(? string? interface))
     (assignment id name (db->boolean enabled) test-lang soln-lang
                 interface (db->boolean single-test-suite?) solution)]
    [_ (error 'db->asgn "unexpected ~s" vec)]))

(provide/contract (login (string? string? . -> . (or/c user? false?))))
(define (login name password-hash)
  (let-prepare ([stmt "SELECT * FROM user WHERE name=?"])
    (load-params stmt name)
    (match (map db->user (step* stmt))
      [`(,(? user? u))
       (if (and (string=? (user-password-hash u) password-hash)
                (user-enabled? u))
           u
           #f)]
      [_ #f])))

(provide/contract (all-asgns (-> (listof assignment?))))
(define (all-asgns)
  (let-prepare ([stmt "SELECT * FROM assignment"])
    (map db->asgn (step* stmt))))

(provide/contract (active-asgns (-> (listof assignment?))))
(define (active-asgns)
  (let-prepare ([stmt "SELECT * FROM assignment WHERE enabled=1"])
    (map db->asgn (step* stmt))))

(provide/contract (asgn-by-name (string? . -> . assignment?)))
(define (asgn-by-name name)
  (let-prepare ([stmt "SELECT * FROM assignment WHERE name=?"])
    (load-params stmt name)
    (match (step* stmt)
      [`(,(? vector? vec)) (db->asgn vec)]
      [_ (error 'asgn-by-name "unknown assignment ~a" name)])))

(provide/contract (num-tests (string? . -> . integer?)))
(define (num-tests name)
  (let-prepare ([stmt "SELECT id FROM test_suite WHERE asgn_name=? AND status=\"ta-ok\""])
    (load-params stmt name)
    (length (step* stmt))))

(provide/contract (new-login (string? string? . -> . boolean?)))
(define (new-login name password-hash)
  (with-handlers
      ([exn:sqlite? (lambda (exn) #f)]) ; UNIQUE violated (dup. user name)
    (let-prepare 
        ([stmt "INSERT INTO user (name, enabled, password_hash, admin) VALUES (?,?,?,?)"])
      (run stmt name (boolean->db #t) password-hash (boolean->db #f))
      #t)))

(provide/contract (set-user-admin! (string? boolean? . -> . any)))
(define (set-user-admin! user is-admin?)
  (let-prepare ([stmt "UPDATE user SET admin=? WHERE name=?"])
    (run stmt (boolean->db is-admin?) user)))

(provide/contract (new-assignment (assignment? . -> . assignment?)))
(define (new-assignment a)
  (with-handlers
      ([exn:sqlite? (lambda (exn) #f)])
    (let-prepare
        ([stmt "INSERT INTO assignment (name, enabled, test_lang, solution_lang, single_test_suite, interface, solution) VALUES (?,?,?,?,?,?,?)"])
      (with-transaction*
       db
       'exclusive
       (lambda (fail)
         (run stmt 
              (assignment-name a)
              (boolean->db (assignment-enabled? a))
              (assignment-test-lang a)
              (assignment-solution-lang a)
              (boolean->db (assignment-single-test-suite? a))
              (assignment-interface a)
              (assignment-solution a))
         (struct-copy assignment a
                      [id (last-insert-rowid db)]))))))


(define (db->test-suite vec)
  (match vec
    [`#(,(? integer? id) ,(? integer? user-id) ,(? string? asgn-name) 
                         ,(? string? submission) ,(? integer? time) 
                         ,(? string? status) ,(? string? status-text))
     (test-suite id user-id asgn-name submission time 
                 (string->symbol status) status-text)]
    [_ #f]))


(provide/contract (test-suites-by-user (user? string? . -> . (listof test-suite?))))
(define (test-suites-by-user u asgn-name)
  (let-prepare ([stmt "SELECT id,user_id,asgn_name,submission,time,status,status_text FROM test_suite WHERE asgn_name=? AND user_id=?"])
    (load-params stmt asgn-name (user-id u))
    (map db->test-suite (step* stmt))))

(provide/contract (solutions-by-user (user? string? . -> . (listof solution?))))
(define (solutions-by-user u asgn-name)
  (let-prepare ([stmt "SELECT id, user_id, asgn_name, submission, time, status, status_text FROM solution WHERE asgn_name=? AND user_id=?"])
    (load-params stmt asgn-name (user-id u))
    (map db->solution (step* stmt))))

(provide/contract (new-test-suite (test-suite? . -> . any)))
(define (new-test-suite t)
  (let-prepare
      ([stmt "INSERT INTO test_suite (user_id, asgn_name, submission, time, status, status_text) VALUES (?, ?, ?, ?, ?, ?)"])
    (run stmt (test-suite-user-id t) (test-suite-asgn-name t) 
         (test-suite-submission t)
         (test-suite-time t) 
         (symbol->string (test-suite-status t))
         (test-suite-status-text t))))


(provide/contract (pending-test-suite (-> (or/c false? test-suite?))))
(define (pending-test-suite)
  (let-prepare ([stmt "SELECT * FROM test_suite WHERE status=\"submitted\" ORDER BY time"])
    (let ([vec (step stmt)])
      (if (vector? vec)
          (db->test-suite vec)
          #f))))

(provide/contract (pending-solution (-> (or/c false? solution?))))
(define (pending-solution)
  (let-prepare ([stmt "SELECT * FROM solution WHERE status=\"pending\" ORDER BY time"])
    (let ([vec (step stmt)])
      (if (vector? vec)
          (db->solution vec)
          #f))))

(provide/contract (test-suites-for-TA-approval (string? . -> . (listof test-suite?))))
(define (test-suites-for-TA-approval asgn-name)
  (let-prepare
      ([stmt "SELECT * FROM test_suite WHERE status=\"machine-ok\" AND asgn_name=?"])
    (load-params stmt asgn-name)
    (map db->test-suite (step* stmt))))

(provide/contract (current-enabled-tests (string? . -> . (listof test-suite?))))
(define (current-enabled-tests asgn-name)
  (let-prepare
      ([stmt "SELECT * FROM test_suite WHERE status=\"ta-ok\" AND asgn_name=?"])
    (load-params stmt asgn-name)
    (map db->test-suite (step* stmt))))
  
(provide/contract 
 (update-test-suite-status (integer? test-suite-status? string? . -> . any)))
(define (update-test-suite-status uid status status-text)
  (when (symbol=? status 'ta-ok)
    (let ([ts (test-suite-by-id uid)])
      (when (asgn-single-test-suite? (test-suite-asgn-name ts))
        (let-prepare ([stmt "UPDATE test_suite SET status=\"superseded\", STATUS_text=\"\" WHERE user_id=?"])
          (run stmt (test-suite-user-id ts))))))
  (let-prepare
       ([stmt "UPDATE test_suite SET status=?, status_text=? WHERE id=?"])
    (run stmt (symbol->string status) status-text uid)))

(provide/contract
 (new-solution (integer? string? string? . -> . any)))
(define (new-solution user-id asgn-name solution)
  (let-prepare
      ([stmt "INSERT INTO solution (user_id, asgn_name, submission, time, status, status_text) VALUES (?, ?, ?, ?, ?, ?)"])
    (run stmt user-id asgn-name solution (current-seconds) "pending" "")))

(provide/contract 
 (update-solution-status (integer? solution-status? string? . -> . any)))
(define (update-solution-status uid status status-text)
  (let-prepare
      ([stmt "UPDATE solution SET status=?, status_text=? WHERE id=?"])
    (run stmt (symbol->string status) status-text uid)))

(provide/contract (asgn-single-test-suite? (string? . -> . boolean?)))
(define (asgn-single-test-suite? asgn-name)
  (let-prepare ([stmt "SELECT single_test_suite FROM assignment WHERE name=?"])
    (load-params stmt asgn-name)
    (match (step* stmt)
      [`(#(,(? db-boolean? v))) (db->boolean v)]
      [r (error 'asgn-enabled? "unexpected ~a" r)])))

(define (test-suite-by-id uid)
  (let-prepare ([stmt "SELECT * from test_suite WHERE id=?"])
    (load-params stmt uid)
    (match (step* stmt)
      [`(,vec) (db->test-suite vec)]
      [r (error 'test-suite-by-id "unexpected ~a" r)])))

(provide/contract (asgn-enabled? (string? . -> . boolean?)))
(define (asgn-enabled? asgn-name)
  (let-prepare ([stmt "SELECT enabled FROM assignment WHERE name=?"])
    (load-params stmt asgn-name)
    (match (step* stmt)
      [`(#(,(? db-boolean? v))) (db->boolean v)]
      [r (error 'asgn-enabled? "unexpected ~a" r)])))

(provide/contract (set-asgn-enabled (string? boolean? . -> . any)))
(define (set-asgn-enabled asgn-name enabled?)
  (let-prepare
      [(stmt "UPDATE assignment SET enabled=? WHERE name=?")]
    (run stmt (boolean->db enabled?) asgn-name)))