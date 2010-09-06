#lang racket
(require 
 "sqlite/sqlite.rkt"
 "db.rkt")

(define db (open ':memory:))
(set-db! db)
(init-db!)

(define arjun (new-login "arjun" "arjun"))
(set-user-admin! "arjun" #t)

(define ae-solution #<<HERE
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
  )


(define ae
  (new-assignment
   (assignment #f "ae" #t "plai" #t ae-solution)))
