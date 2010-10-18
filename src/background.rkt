#lang racket
(require 
 racket/sandbox
 net/sendmail
 "db.rkt"
 "log.rkt")

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

(define (result->string result)
  (let ([port (open-output-string)])
    (result->string/port port result)
    (get-output-string port)))

(define ((log-test-suite p) ts)
  (fprintf p "~n;;; Test suite by ~s~n" (test-suite-user-id ts))
  (display (test-suite-submission ts) p)
  (newline p))

(define (check-solution sol)
  (let* ([all-tests (current-enabled-tests (solution-asgn-name sol))]
         [results
          (for/list ([ts (in-list all-tests)])
            (run-test (solution-submission sol) (test-suite-submission ts)  
                      #:abridged #t))]
         [passed? (andmap all-tests-passed? results)])
    (log
     (format "~a:~a:~a solution ~a" 
             (user-name (user-by-id (solution-user-id sol)))
             (solution-asgn-name sol)
             (solution-time sol)
             (if passed? "passed all tests" "failed some tests"))
     (lambda (p)
       (display (solution-submission sol) p)
       (display "\n;;;Test suites follow:\n" p)
       (for-each (log-test-suite p) all-tests)
       (newline p)))
    (if passed?
        (update-solution-status (solution-id sol) 'ok "")
        (update-solution-status (solution-id sol) 'error (results->string results)))))

(define (check-test-suite ts)
  (let* ([result
          (run-test (assignment-solution (asgn-by-name (test-suite-asgn-name ts)))
                    (test-suite-submission ts))]
         [u (user-by-id (test-suite-user-id ts))]
         [passed? (all-tests-passed? result)])
    (log (format "~a:~a:~a test suite ~a" 
                 (user-name u)
                 (test-suite-asgn-name ts)
                 (test-suite-time ts)
                 (if passed? "pending approval" "failed gold"))
         (lambda (p)
           (display (test-suite-submission ts) p)
           (newline p)))
    (if passed?
        (update-test-suite-status (test-suite-id ts) 'machine-ok "")
        (update-test-suite-status (test-suite-id ts) 'machine-error
                                  (result->string result)))))


(define (background-thread-proc)
  (cond
    [(pending-solution) => (lambda (sol) (check-solution sol) (collect-garbage))]
    [(pending-test-suite) => (lambda (ts) (check-test-suite ts) (collect-garbage))]
    [else (sleep 5)])
  (background-thread-proc))

(provide/contract
 (read-program (string? . -> .(listof syntax?))))

(define (read-program program-string #:read-syntax [read-syntax? #t])
  ; Heuristic to remove the #lang line. If the submission uses
  ; #reader or (module ...), this probably won't work.
  (let ([port (open-input-string 
               (regexp-replace #rx"#lang" program-string ";#lang")
               'submission)])
    (port-count-lines! port) ; and columns!
    (let loop ([val (read-syntax (object-name port) port)])
      (if (eof-object? val)
          (begin
            (close-input-port port)
            empty)
          (cons val (loop (if read-syntax? 
                              (read-syntax (object-name port) port)
                              (read port))))))))

; Mutators should begin with (allocator-setup filename heap-size).  Replace this
; with (allocator-setup ,collector.rkt ,(+ heap-size 10).  If 
; mutator-sexp is malformed, ignore the error and return mutator-sexp.  Running 
; the mutator will signal /some/ error.
(define (use-my-collector collector.rkt mutator-sexp)
  (if (empty? (rest mutator-sexp))
      mutator-sexp
      (match (syntax->datum (first mutator-sexp))
        [`(allocator-setup ,_ ,heap)
         (cons `(allocator-setup ,collector.rkt ,(+ heap 10))
               (rest mutator-sexp))]
        [else mutator-sexp])))


(define (write-module! path lang src)
  (with-output-to-file
    path
    (lambda ()
      (display (format "#lang ~a~n" lang))
      (if (string? src)
          (display src)
          (for ([exp (in-list src)])
            (if (syntax? exp)
                (write (syntax->datum exp))
                (write exp))
            (newline))))))

(provide run-gc-test)
(define (run-gc-test collector mutator
                     #:memory-limit [memory-limit 150]
                     #:cpu-limit [cpu-limit 50])
  (let* ([tmp-dir (path->string (make-temporary-file "testfest~a" 'directory))]
         [collector.rkt (build-path tmp-dir "collector.rkt")]
         [mutator.rkt (build-path tmp-dir "mutator.rkt")])
    (dynamic-wind
     void
     (lambda ()
       (write-module! collector.rkt 'plai/collector (read-program collector))
       (write-module! mutator.rkt 'plai/mutator
                      (use-my-collector "collector.rkt" (read-program mutator)))
       (parameterize ([current-directory tmp-dir]
                      [sandbox-path-permissions `((read ,tmp-dir))])
          (let ([evaluator (make-evaluator
                            'plai
                            #:requires 
                            '(plai/test-harness plai/private/gc-core))])
            (dynamic-wind
             void
             (lambda ()
               (with-handlers
                   ([exn? (lambda (exn) (evaluator '(heap-as-string)))])
                 (evaluator
                  (with-limits 
                   memory-limit cpu-limit
                   `(begin
                      (require "mutator.rkt")
                      'mutator-ran-successfully; s(heap-as-string)
                      )))))
               (lambda () (kill-evaluator evaluator))))))
     ; finally
     (lambda ()
       (delete-file collector.rkt)
       (delete-file mutator.rkt)
       (delete-directory tmp-dir)))))

; run-test : string string bool nat nat -> s-exp
(define (run-test solution test-suite
                  #:abridged [abridged? #f]
                  #:memory-limit [memory-limit 150]
                  #:cpu-limit [cpu-limit 50])
  (with-handlers
      ([exn? (lambda (exn) exn)])
    (let ([evaluator (make-module-evaluator 
                      `(module my-module plai
                         (require plai/private/command-line)
                         (disable-tests #t)
                         ,@(read-program solution)
                         (disable-tests #f)))])
      (dynamic-wind
       void
       (lambda ()
         (evaluator
          (with-limits 
           memory-limit cpu-limit
           `(begin
              (plai-ignore-exn-strings true)
              (local ()
                (abridged-test-output ,abridged?)
                ,@(map syntax->datum (read-program test-suite)))
              plai-all-test-results))))
       (lambda () (kill-evaluator evaluator))))))