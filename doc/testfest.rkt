#lang scribble/manual

@title{TestFest}

@section{Installation}

@itemlist[
          
  @item{Use 'cabal build' to create the thing.}
           
  @item{Run the command 'tourney-server sample' to initialize the database and
        create a sample assignment.}

  @item{Visit 'http://testfest' with your Web browser. Login as 'arjun', with the
        password 'arjun'.}
  ]

@section{Configuration}

Create two new accounts, 'tourney' and 'student' on tesfest:

% useradd tourney
% useradd student

The user tourney needs the ability to sudo to student. Edit the sudoers file:

% visudo

and append the line

tourney ALL=(student) NOPASSWD: ALL


Ensure that port 5984 on testfest is firewalled.  Port 5984 gives full
read/write access to the CouchDB database.

@section{Maintenance}

TestFest uses CouchDB as its database.  CouchDB includes a Web-based
management console called Futon that's good enough for many queries and
administration tasks. To open Futon, first forward testfest:5984 to a
local port:

@verbatim{
$ ssh -N -L 5984:127.0.0.1:5984 testfest # localhost may not work
  }

Then, visit @url{http://localhost:5984/_utils} from a Web browser.

@section{Robust Gold Solutions}

@subsection{Specifying Interfaces}

  Each @racketid[define-type]
definition introduces a plethora of names that can be very annoying to
get right.  Here's an easy solution:

1. Create a new PLAI Scheme file with the support code for the assignment

2. Ensure the file compiles correctly--don't fill in a full solution
with helper functions.  Instead, replace the ...'s that we have in our
stubs with something like @racket[#f].

3. In the same directory as the aforementioned stub file, create this file:

@verbatim{
#lang racket
(require "ae-solution.rkt")

(define-syntax (exports-of stx)
 (syntax-case stx ()
   [(_ module-name)
    (let ([exports (syntax-local-module-exports 
                     (syntax->datum #'module-name))])
      #`(quote #,(cdaddr exports)))]))

(display (exports-of "ae-solution.rkt"))
}

Replace "xinterp-eager-exports" to whatever you called your file from
steps 1-2.  The massive list that it prints is the list of exports.
Paste this into the "interface" text box under "Create New
Assignment."  Remember to removing the parenthesis that enclose it;
testfest expectes a whitespace-separated list.

@subsection{Catching Errors}

@racketid[test/exn] only catches errors that students explicitly raise themselves:

@racketblock[
(test/exn (/ 1 0) "by zero") ; test fails
(test/exn (error "division by zero") "by zero") ; test succeeds
]


@racketid[test/exn] is able to distinguish student-raised error's from other
errors in racket because error in @tt{#lang plai} raises a different type
of exception.  So, if you have an error condition in your program that
test suites should not be able to test, raise the error using Racket's
error procedure.  For example:

@racketblock[
(require (rename-in racket/base [error racket-error]))

(define (parse sexp)
 (match sexp
   [(? number? n) (num n)]
   [(list '+ l r)
    (add (parse l) (parse r))]
   [_ (racket-error 'parse "invalid expression: ~e" sexp)]))
   ]