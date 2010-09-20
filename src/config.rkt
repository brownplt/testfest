#lang racket
(provide/contract
 [logging-email-address (or/c string? false?)]
 [email-from-address string?]
 [testfest-public-hostname string?])

; If #f, then logs to stderr. 
(define logging-email-address "arjun+testfest@cs.brown.edu")

; Students will receive password reset notifications and new account information
; from this email address.  This is usually the email address of the administrator.
(define email-from-address "arjun@cs.brown.edu")

; Students are emailed with their login information and the URL to TestFest.
; This is the TestFest URL reported in the message.
(define testfest-public-hostname "http://cs173.cs.brown.edu")