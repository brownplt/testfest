#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/configuration/responders
         web-server/http/request-structs
         "json.rkt"
         "db.rkt"
         "sqlite/sqlite.ss"
         "sample.rkt"
         "background.rkt")

(define (ok success? jsexpr)
  (when (not success?)
    (printf "Failed: ~s~n" jsexpr))
  (let ([port (open-output-bytes)])
    (parameterize ([current-output-port port])
      (write-json `#hasheq([success . ,success?] [value . ,jsexpr])))
    (make-response/full
     200
     #"OK"
     (current-seconds)
     #"application/json"
     empty
     (list (get-output-bytes port)))))

(define (request-command req)
  (printf "command: ~s~n" (url-query (request-uri req)))
  (match (url-query (request-uri req))
    [`((command . ,(? string? cmd))) (string->symbol cmd)]
    [_ #f]))


(define (asgn->json a)
  `#hasheq((id . ,(assignment-name a)) (enabled . ,(assignment-enabled? a))))

(define (test-suite->json t)
  `#hasheq([userid . ,(test-suite-user-id t)]
           [id . ,(test-suite-id t)]
           [asgnid . ,(test-suite-asgn-name t)]
           [code . ,(test-suite-submission t)]
           [time . ,(test-suite-time t)]
           [status_text . ,(test-suite-status-text t)]
           [status . ,(symbol->string (test-suite-status t))])) 
  
  
(define (main u req)
  (let ([binds (request-bindings req)])
    (match (request-command req)
      ['isadmin (ok #t (user-admin? u))]
      ['assignments (ok #t (map asgn->json (active-asgns)))]
      ['numtests (ok #t (num-tests (extract-binding/single 'id binds)))]
      ['testsuites
       (ok #t (map test-suite->json
                   (test-suites-by-user u (extract-binding/single 'asgnid binds))))]
      ['programs
       (ok #t (map (lambda (s)
                     `#hasheq([id . ,(solution-id s)]
                              [status . ,(symbol->string (solution-status s))]
                              [status_text . ,(solution-status-text s)]
                              [time . ,(solution-time s)]))
                   (solutions-by-user u (extract-binding/single 'asgnid binds))))]
      ['newtest
       (begin
         (new-test-suite
          (test-suite #f (user-id u) (extract-binding/single 'assignment binds)
                      (extract-binding/single 'test binds)
                      (current-seconds) 'submitted ""))
         (ok #t "test received; processing"))]
      ['newprog
       (begin
         (new-solution (user-id u)
                       (extract-binding/single 'asgnid binds)
                       (extract-binding/single 'prog binds))
         (ok #t ""))]
      [cmd (admin-main u cmd binds)])))

(define (admin-main u cmd binds)
  (if (user-admin? u)
      (match cmd
        ['allasgns  (ok #t (map asgn->json (all-asgns)))]
        ['pendingapproval
         (ok #t 
             (map test-suite->json 
                  (test-suites-for-TA-approval 
                   (extract-binding/single 'asgnid binds))))]
        ['currenttests
         (ok #t 
             (map test-suite->json 
                  (current-enabled-tests
                   (extract-binding/single 'asgnid binds))))]
        ['setteststatus
         (begin
           (update-test-suite-status
            (string->number (extract-binding/single 'id binds))
            (string->symbol (extract-binding/single 'status binds))
            (extract-binding/single 'status_text binds))
           (ok #t ""))]
        ['getgold
         (ok #t (assignment-solution (asgn-by-name (extract-binding/single 'id binds))))]
        ['isasgnenabled
         (ok #t (asgn-enabled? (extract-binding/single 'id binds)))]
        ['setasgnenabled
         (begin (set-asgn-enabled (extract-binding/single 'id binds)
                                  (json->jsexpr (extract-binding/single 'enable binds)))
                (ok #t ""))]
        [_ (ok #f "invalid command")])
      (ok #f "invalid command")))


(define (start req)
  (let ([binds (request-bindings req)])
    (if (and (exists-binding? 'username binds)
             (exists-binding? 'password binds))
        (let ([u (login (extract-binding/single 'username binds)
                        (extract-binding/single 'password binds))])
          (if (user? u)
              (main
               u
               (send/suspend
                (lambda (k-url)
                  (ok #t k-url))))
              (ok #f "login failed")))
        (ok #f "ill-formed request"))))

  
(define background-thread (thread background-thread-proc))

(serve/servlet
 start
 #:servlet-path "/login"
 #:extra-files-paths '("../static/web")
 #:file-not-found-responder
 (lambda (req)
   (if (empty? (url-path (request-uri req)))
       (file-response 200 "OK" "../static/web/index.html")
       (make-response/basic
        404 #"File Not Found"
        (current-seconds) TEXT/HTML-MIME-TYPE empty))))