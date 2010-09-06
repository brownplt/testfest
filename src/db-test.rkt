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


(let loop ([i 0])
  (when (< i 20)
      (run-test 
       #<<HERE
(define-type AE 
  [num (n integer?)]
  [add (lhs AE?) (rhs AE?)]
  [sub (lhs AE?) (rhs AE?)])
 
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and (list? sexp) (= 3 (length sexp)))
     (case (first sexp)
       [(+) (add (parse (second sexp)) (parse (third sexp)))]
       [(-) (sub (parse (second sexp)) (parse (third sexp)))]
       [else (error 'parse "unknown operator")])]
    [else (error 'parse "unknown term")]))
 
(define (calc an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (lhs rhs) (+ (calc lhs) (calc rhs))]
    [sub (lhs rhs) (- (calc lhs) (calc rhs))]))
HERE
       "(test 3 2) (test 10 19) (test (+ 5 5) 10)"
       )
    (printf "Test ~a...~n" i)
    (loop (add1 i))))

  