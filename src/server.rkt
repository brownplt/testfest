#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/configuration/responders
         web-server/http/request-structs
         net/cookie
         net/sendmail
         "json.rkt"
         "db.rkt"
         "sqlite/sqlite.ss"
         "background.rkt")

(define (ok success? jsexpr #:k-url [k-url #f])
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
     (if k-url
         (let ([cookie (cookie:add-max-age (set-cookie "session" k-url) 3600)])
           (list (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie)))))
         empty)
     (list (get-output-bytes port)))))

(define (request-command req)
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
      ['ping (ok #t "pong")]
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
      ['newasgn
       (begin
         (new-assignment #f
                         (extract-binding/single 'asgnid binds)
                         #f
                         (extract-binding/single 'kind binds)
                         (json->jsexpr (extract-binding/single 'singletestsuite binds))
                         (extract-binding/single 'solution binds))
         (ok #t "assignment created; refresh the page"))]
      ['logout 
       (send/finish (ok #t "" #:k-url "expired"))]
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
                (ok #t "" ))]
        [_ (ok #f "invalid command")])
      (ok #f "invalid command")))


(define (start req)
  (let ([binds (request-bindings req)])
    (cond
      [(and (exists-binding? 'username binds)
            (exists-binding? 'password binds))
       (let ([u (login (extract-binding/single 'username binds)
                       (extract-binding/single 'password binds))])
         (if (user? u)
             (main
              u
              (send/suspend (lambda (k-url) (ok #t k-url #:k-url k-url))))
             (ok #f "login failed")))]
      [(exists-binding? 'forgot binds) 
       (let ([username (extract-binding/single 'forgot binds)]
             [password (create-password)])
         (change-password username password)
         (let ([msg-port
                (send-mail-message/port "arjun@cs.brown.edu" "[TestFest] password changed"
                                        (list username) empty empty)])
           (fprintf 
            msg-port "Your new password is ~a" password))
         (ok #t "new password sent; check your email"))]
      [else (ok #f "ill-formed request")])))
      



(let ([port 8080]
      [db-path "testfest.db"])
  (command-line
   #:once-each
   [("-p" "--port") p "serve on port" (set! port (string->number p))]
   [("-d" "--db-path") p "testfest.db" (set! db-path p)]
   #:args ()
   (set-db! (open (string->path db-path)))
   (printf "http://localhost:~a/~n" port)
   (let ([background-thread (thread background-thread-proc)])
     (serve/servlet
      start
      #:port port
      #:launch-browser? #f
      #:banner? #f
      #:servlet-path "/login"
      #:extra-files-paths '("res")
      #:file-not-found-responder
      (lambda (req)
        (if (empty? (url-path (request-uri req)))
            (file-response 200 "OK" "../static/web/index.html")
            (make-response/basic
             404 #"File Not Found"
             (current-seconds) TEXT/HTML-MIME-TYPE empty)))))))