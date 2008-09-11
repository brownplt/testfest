#!/usr/bin/env mzscheme
#lang scheme
(require scheme/sandbox)
(require plai/private/sandbox
         plai/private/test)

(define (display-results r)
  (cond
    [(string? r) (display r (current-error-port))]
    [(list? r)
     (for-each (λ (e) (fprintf (current-error-port) "~s~n" e)) r)]
    [else (display r (current-error-port))]))


;;; Attempt to read each line of the program and return a list of
;;; expressions.  If there is a read error, we return the exception
;;; raised.
(provide/contract
 (read-program (path? . -> . (or/c (listof syntax?) exn?))))
(define (read-program program-string)
  (with-handlers ([exn? (λ (exn) exn)])
    (let ([port (open-input-file program-string #:mode 'text)])
      (port-count-lines! port) ; and columns!
      (let loop ([val (read-syntax (object-name port) port)])
        (if (eof-object? val)
            (begin
              (close-input-port port)
              empty)
            (cons val (loop (read-syntax (object-name port) port))))))))

; If you have a procedure with 10 parameters, you probably missed some. 
(define (run-tests #:solution solution
                   #:test-suite test-suite
                   #:interface [interface false]
                   #:memory-limit [memory-limit 150]
                   #:cpu-limit [cpu-limit 50]
                   #:abridged-results [abridged-results false]
                   #:halt-on-errors [halt-on-errors false]
                   #:argument-seven [_ false]
                   #:argument-eight [__ false]
                   #:argument-nine [___ false]
                   #:argument-ten [____ false])
  (local ([define require-line
            (if interface
                `(only-in ,solution ,@interface)
                solution)]
          [define-values (solution-base _ __) 
            (split-path (path->complete-path solution))])
    (parameterize
        ([use-compiled-file-paths null]
         [sandbox-path-permissions 
          `([exists ,solution-base]
            [read ,solution])])
      (local ([define evaluator (make-evaluator 'plai)])
        (with-handlers
            ([exn?
              (lambda (exn)
                (display-results (evaluator 'plai-all-test-results))
                (display (count-errors (evaluator 'plai-all-test-results)))
                (error (exn-message exn)))])
          (evaluator
           (with-limits 
            memory-limit cpu-limit
            `(begin
               (require plai/private/command-line)
               (disable-tests true)
               (require ,require-line)
               (disable-tests false)
               ,(if halt-on-errors
                    '(halt-on-errors true)
                    '(halt-on-errors false))
               ,(if abridged-results 
                    '(abridged-test-output true)
                    '(abridged-test-output false))
               ,@(map syntax->datum (read-program test-suite))
               plai-all-test-results))))))))

(when (>  (vector-length (current-command-line-arguments)) 0)
  (local ([define cpu-limit 50]
          [define memory-limit 100]
          [define interface false]
          [define abridged-results false]
          [define error-on-errors false]
          [define halt-on-errors false])
    (command-line
     #:once-each
     [("-t" "--cpu-limit") sec "limit CPU usage"
                           (set! cpu-limit (string->number sec))]
     [("-a" "--abridge-results") "don't show expressions in test results"
                                 (set! abridged-results true)]
     [("-e" "--halt-on-errors") 
      "halt on errors" 
      (set! halt-on-errors true)]
     [("-r" "--error-on-errors")
      "signal an error if any errors occur"
      (set! error-on-errors true)]
     [("-m" "--memory-limit") mb "limit memory usage"
                              (set! memory-limit (string->number mb))]
     [("-i" "--interface") names "names to import from the solution"
                           (set! interface (read (open-input-string
                                                  (string-append "(" names ")"))))]
     #:args (solution test-suite)
     (let ([results
            (run-tests #:solution solution
                       #:test-suite test-suite
                       #:interface interface
                       #:memory-limit memory-limit
                       #:cpu-limit cpu-limit
                       #:halt-on-errors halt-on-errors
                       #:abridged-results abridged-results)])
       (display-results results)
       (let ([num-errors (count-errors results)])
         (when (> num-errors 0)
           (display num-errors))
         
         (when (and error-on-errors (> num-errors 0))
           (error "\nsome tests failed")))))))
