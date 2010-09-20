#!/usr/bin/env racket
#lang racket
(require
 net/sendmail
 "sqlite/sqlite.rkt"
 "db.rkt"
 "config.rkt")

(define db-path "testfest.db")

(define suffix #f)

(define notify? #t)

(command-line
 #:once-each
 [("-d" "--db-path") p "database path" (set! db-path p)]
 [("-s" "--suffix") p "suffix for usernames" (set! suffix p)]
 [("--no-notify") "do not notify users" (set! notify? #f)]
 #:args arg-users
 (set-db! (open (string->path db-path)))
 (let ([args (if suffix 
                 (map (lambda (a) (string-append a suffix)) arg-users)
                 arg-users)])
   (for ([username (in-list args)])
     (let ([password (create-password)])
       (new-login username password) ; exception on unique-violation
       (printf "~a:~a~n" username password)
       (when notify?
         (let ([msg-port
                (send-mail-message/port email-from-address "[TestFest] new account"
                                        (list username) empty empty)])
           (fprintf 
            msg-port
            (string-append
             "TestFest is accessible at " testfest-public-hostname "\n\n"
             "Your username is ~a\n\nYour password is ~a\n") username password)
           (close-output-port msg-port)))))))