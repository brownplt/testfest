#lang racket

(define (test-suite-status? sym)
  (and (memq sym 
             '(submitted machine-ok machine-error ta-ok ta-error superseded))
       #t))

(define (solution-status? sym)
  (and (memq sym '(ok pending error))
       #t))

(struct user (id name [enabled? #:mutable] [password-hash #:mutable] 
                 [admin? #:mutable]) #:transparent)

(struct assignment (id name enabled? kind single-test-suite? solution)
  #:transparent)

(struct test-suite (id user-id asgn-name submission time status status-text))

(struct solution (id user-id asgn-name submission time status status-text))

(provide test-suite-status? solution-status?)

(provide/contract
  
 (struct user
   ([id integer?]
    [name string?]
    [enabled? boolean?]
    [password-hash string?]
    [admin? boolean?]))
 
 (struct assignment
   ([id (or/c false? integer?)]
    [name string?]
    [enabled? boolean?]
    [kind string?]
    [single-test-suite? boolean?]
    [solution string?]))
 
 (struct test-suite
   ([id (or/c false? integer?)]
    [user-id integer?]
    [asgn-name string?]
    [submission string?]
    [time integer?]
    [status test-suite-status?]
    [status-text string?]))
 
 (struct solution
   ([id (or/c false? integer?)]
    [user-id integer?]
    [asgn-name string?]
    [submission string?]
    [time integer?]
    [status solution-status?]
    [status-text string?]))
 )


#|(struct report (id test-suite-id solution-id assignment-id time output-id
                   success?))|#

