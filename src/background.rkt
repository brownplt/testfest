#lang racket
(require 
 racket/sandbox
 "db.rkt")

(provide background-thread-proc run-test)

(define (all-tests-passed? result)
  (and (list? result) (andmap (lambda (t) (eq? 'good (first t))) result)))

(define (result->string/port port result)
  (print result port)
  (newline port))

(define (results->string results)
  (let ([port (open-output-string)])
    (for ([result (in-list results)])
      (if (list? result)
          (for-each (λ (r) (result->string/port port r)) result)
          (result->string/port port result)))
    (get-output-string port)))

(define (check-solution sol)
  (let ([results
         (for/list ([ts (in-list (current-enabled-tests (solution-asgn-name sol)))])
           (run-test (solution-submission sol) (test-suite-submission ts)  #:abridged #t))])
    (if (andmap all-tests-passed? results)
        (update-solution-status (solution-id sol) 'ok "")
        (update-solution-status (solution-id sol) 'error (results->string results)))))

(define (check-test-suite ts)
  (let ([result
         (run-test (assignment-solution (asgn-by-name (test-suite-asgn-name ts)))
                   (test-suite-submission ts))])
    (if (all-tests-passed? result)
        (update-test-suite-status (test-suite-id ts) 'machine-ok "")
        (update-test-suite-status (test-suite-id ts) 'machine-error
                                  (results->string result)))))
  

(define (background-thread-proc)
  (cond
    [(pending-solution) => (lambda (sol) (check-solution sol) (collect-garbage))]
    [(pending-test-suite) => (lambda (ts) (check-test-suite ts) (collect-garbage))]
    [else (sleep 5)])
  (background-thread-proc))

(provide/contract
 (read-program (path? . -> .(listof syntax?))))
; read-program : path? -> (listof syntax?)
(define (read-program program-string)
  (let ([port (open-input-string program-string 'submission)])
    (port-count-lines! port) ; and columns!
    (let loop ([val (read-syntax (object-name port) port)])
      (if (eof-object? val)
          (begin
            (close-input-port port)
            empty)
          (cons val (loop (read-syntax (object-name port) port)))))))

(define (run-test solution test-suite
                  #:abridged [abridged? #f]
                  #:memory-limit [memory-limit 150]
                  #:cpu-limit [cpu-limit 50])
  (with-handlers
      ([exn? (lambda (exn) (error (exn-message exn)))])
    (parameterize
        ([use-compiled-file-paths null])
      (let ([evaluator (make-module-evaluator 
                        `(module my-module plai
                           ,@(read-program solution)))])
        (dynamic-wind
         void
         (lambda ()
           (evaluator
            (with-limits 
             memory-limit cpu-limit
             `(begin
                (require plai/private/command-line)
                (plai-ignore-exn-strings true)
                (local ()
                  (abridged-test-output ,abridged?)
                  ,@(map syntax->datum (read-program test-suite)))
                plai-all-test-results))))
         (lambda () (kill-evaluator evaluator)))))))