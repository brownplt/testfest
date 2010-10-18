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
  (if (string? result)
      (display result port)
      (print result port))
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
         [asgn-kind (assignment-kind (asgn-by-name (solution-asgn-name sol)))]
         [results
          (for/list ([ts (in-list all-tests)])
            (match asgn-kind
              ["plai" (run-test (solution-submission sol)
                                (test-suite-submission ts)  
                                #:abridged #t)]
              ["gc" (run-gc-test (solution-submission sol)
                                 (test-suite-submission ts))]
              [unk (format "unknown assignment kind ~a" unk)]))]
         [passed? 
          (match asgn-kind
            ["plai" (andmap all-tests-passed? results)]
            ["gc" (andmap (lambda (x) (eq? x 'success)) results)]
            [unk (format "unknown assignment kind ~a" unk)])])
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

(define (check-test-suite/gc ts)
  (let* ([result
          (run-gc-test (assignment-solution (asgn-by-name (test-suite-asgn-name ts)))
                       (test-suite-submission ts))]
         [u (user-by-id (test-suite-user-id ts))]
         [passed? (eq? result 'success)])
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
    [(pending-solution) 
     => 
     (lambda (sol) (check-solution sol) (collect-garbage))]
    [(pending-test-suite) 
     => 
     (lambda (ts) 
       (match (assignment-kind (asgn-by-name (test-suite-asgn-name ts)))
         ["plai" (check-test-suite ts)]
         ["gc" (check-test-suite/gc ts)]
         [unk (update-test-suite-status 
               (test-suite-id ts) 'machine-error
               (format "unknown assignment kind: ~a" unk))])
       (collect-garbage))]
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
; with (allocator-setup ,collector.rkt ,(+ heap-size 25).  If 
; mutator-sexp is malformed, ignore the error and return mutator-sexp.  Running 
; the mutator will signal /some/ error.
(define (use-my-collector collector.rkt mutator-sexp)
  (if (empty? (rest mutator-sexp))
      mutator-sexp
      (match (syntax->datum (first mutator-sexp))
        [`(allocator-setup ,_ ,heap)
         (cons `(allocator-setup ,collector.rkt ,(+ heap 25))
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
       (parameterize ([current-directory tmp-dir]
                      [sandbox-path-permissions `((read ,tmp-dir))])
         (with-handlers
             ([exn? (lambda (exn) exn)])
           (let ([evaluator (make-evaluator
                             'plai
                             #:requires 
                             '(plai/test-harness plai/private/gc-core))])
             (dynamic-wind
              void
              (lambda ()
                (with-handlers
                    ([exn? (lambda (exn) 
                             (format "~a\n\n~a" exn (evaluator '(heap-as-string))))])
                  (write-module!
                   collector.rkt 'plai/collector
                   (read-program collector))
                  (write-module!
                   mutator.rkt 'plai/mutator
                   (use-my-collector "collector.rkt" (read-program mutator)))
                  (evaluator '(halt-on-errors #t))
                  (evaluator
                   (with-limits 
                    memory-limit cpu-limit
                    `(begin
                       (require "mutator.rkt")
                       'success; s(heap-as-string)
                       )))))
              (lambda () (kill-evaluator evaluator)))))))
     ; finally
     (lambda ()
       (when (file-exists? collector.rkt)
         (delete-file collector.rkt))
       (when (file-exists? mutator.rkt)
         (delete-file mutator.rkt))
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