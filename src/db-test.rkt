#lang racket
(require 
 (except-in rackunit run-test test-suite? test-suite)
 "db.rkt"
 "background.rkt"
 "sqlite/sqlite.ss")

(define-syntax (with-db stx)
  (syntax-case stx ()
    [(_ db body ...)
     #'(let ([db  (open ':memory:)])
         (set-db! db)
         (init-db!)
         body ...
         (close db))]))

(with-db 
 db
 (check-eq? #t (new-login "arjun@cs.brown.edu" "redbull64"))
 (check-eq? #f (new-login "arjun@cs.brown.edu" "redbull65"))
 (check-eq? #t (new-login "arjun2@cs.brown.edu" "redbull64"))
 (check-equal? (user 1 "arjun@cs.brown.edu" #t "redbull64" #f)
               (login "arjun@cs.brown.edu" "redbull64")))
