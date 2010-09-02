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
   ]DO NOT USE SPACES IN ASSIGNMENT NAMES (ensure all letters and underscores)

Prerequisites
-------------

1. CouchDB.  On Mac, get CouchDBX 0.8.1:

http://jan.prima.de/~jan/plok/archives/142-CouchDBX-Revival.html

You double-click to start and it works.  It's incredible.  Click on the play icon, then the magnifying glass icon to see the "Futon" interface at localhost:5984/_utils

2. For the demo, you need DrScheme 4.1.3 and the PLAI software.  In DrScheme, File -> "Install .plt file" and enter the URL:

http://www.cs.brown.edu/~arjun/tmp/plai-4.1.3.plt

For the demo, you need to ensure that mzscheme is in your path (e.g. /Applications/PLT Scheme/bin)

Binaries
--------

I've compiled binaries for 32-bit Intel on Mac OS X 10.5:

http://www.cs.brown.edu/~arjun/tmp/testfest-2.5.2-intelmac.tgz

I think they are self-contained.  Let me know if you get dyn-linking errors.

Setup
-----

1. Ensure that CouchDB is running.  If you can visit http://localhost:5984/_utils, it is running.

You first need to setup the database:

> cd testfest
> ./run sample

If it works correctly, the last line of output will be "Creating views..."

2. ./run to run the server at http://localhost:8080

3. Login as 'arjun' with the password 'arjun' to login as a TA.  As a TA, you can do everything a student can do.  In addition, you can create new assignments, review test suites, etc.

4. You will see a blue bar along the top.  The LHS has a list of assignments (currently, just the "ae" assignment).  The RHS has a "Create new Assignment" "Settings" and "Logout."

Example
-------

1. Select "ae", select "Test Suites" and submit a new test suite.  For example, try this test suite that doesn't exercise the solution at all:

(test 5 5)
(test/exn (/ 1 0) "by zero")

Spam the Refresh button until it says "pending approval by TAs."   You can now click "Pending Tests" and approve it.

2. Submit a solution to run against the test suite you just craeted.  Select "Solutions" on the left and and select "Submit a program."

You may not know what a solution for AE looks like. That's fine.  Since you're logged in as a TA, if you click on "Solution," you'll see the "gold solution" that filters student-submitted tests.  (There are no newlines in the sample, sorry.)  You can simply submit this.

Furiously click on the refresh button until it says "All tests passed."

More Information
----------------

There are three executables in the package I sent.  You can run them with the --help flag to get a cryptic usage guide. Of course, you should ask me if anything is unclear.

In particular, You'll want to use the 173tourney-accounts executable to create accounts for students.

For example, to create accounts, I do:

./173tourney-accounts --suffix=@cs.brown.edu --create-accounts student1 student2 ...

That creates the accounts student1@cs.brown.edu, student2@cs.brown.edu, ... with randomly generated passwords.

I then use the same program to email students their usernames and passwords:

./cs173tourney-accounts --suffix=@cs.brown.edu --notify-users student1 student2 ...

Emailing uses /usr/sbin/sendmail.  This may not work on your Mac laptop, but probably will work on your Linux server.
