#lang racket
(require net/sendmail)

(provide log)
(define (log subject writer)
  (let ([msg-port
         (send-mail-message/port 
          "arjun@cs.brown.edu" 
          subject
          (list "arjun+testfest@cs.brown.edu") empty empty)])
    (writer msg-port)
    (close-output-port msg-port)))