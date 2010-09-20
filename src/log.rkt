#lang racket
(require
 net/sendmail
 racket/date
 "config.rkt")

(provide log)
(define (log subject writer)
  (if logging-email-address
      (let ([msg-port
             (send-mail-message/port 
              email-from-address
              subject
              (list logging-email-address) empty empty)])
        (writer msg-port)
        (close-output-port msg-port))
      (begin
        (printf "Subject: ~a~nTime: ~a~n~n" subject (date->string (current-date)))
        (writer (current-output-port))
        (newline))))