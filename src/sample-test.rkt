#lang racket
(require "sample.rkt" "db.rkt" "background.rkt")


(let loop ([i 0])
  (when (< i 200)
    (new-test-suite
     (test-suite #f 1 "ae"
                 "(test 3 2) (test 10 19) (test (+ 5 5) 10)"
                 (current-seconds) 'submitted ""))
    (loop (add1 i))))

(define thd (thread background-thread-proc))
(read)
    
