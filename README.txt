Requirements
------------

TestFest requires Racket 5.1.3 or higher:

  http://racket-lang.org/download/

TestFest requires sendmail to send account notification messages to students.

We've used TestFest on a Debian/Xen virtual machine with 256 MB RAM.  Running
students' code usually requires more resources than running the Web service
itself.

Installation
------------

From the command-line, create a sample assignment:

  $ cd src
  $ racket sample.rkt

If there are no errors, you'll see the following message:

  Created sample database at testfest.db. Username and password are 
  arjun@cs.brown.edu

From the same directory, start the TestFest server:

  $ racket server.rkt

Visit http://localhost:8080 and login with the credentials above.

Try TestFest
------------

Login as a TA, select the "ae" assignment, and then select "Test Suites" on the
left.  TAs can submit tests and solutions in exactly the same manner as
students. Submit the following test:

  (test (calc (parse '{+ 3 7})) 10)

As a TA, select "Pending Tests" and approve the test. You'll need to click
Refresh to update the list.

Return to "Test Suites" and Refresh to see that the test is accepted.

Select "Solutions" and submit the following correct solution to the AE
assignment:

  (define-type AE 
    [num (n integer?)]
    [add (lhs AE?) (rhs AE?)]
    [sub (lhs AE?) (rhs AE?)])
   
  (define (parse sexp)
    (cond
      [(number? sexp) (num sexp)]
      [(and (list? sexp) (= 3 (length sexp)))
       (case (first sexp)
         [(+) (add (parse (second sexp)) (parse (third sexp)))]
         [(-) (sub (parse (second sexp)) (parse (third sexp)))]
         [else (error 'parse "unknown operator")])]
      [else (error 'parse "unknown term")]))
   
  (define (calc an-ae)
    (type-case AE an-ae
      [num (n) n]
      [add (lhs rhs) (+ (calc lhs) (calc rhs))]
      [sub (lhs rhs) (- (calc lhs) (calc rhs))]))
  
Refresh a few times to see the solution pass the test suite.

Workflow
--------

After logging in, you'll see "ae" on the top-left toolbar. This is a sample
assignment to write an interpreter for (A)rithmetic (E)xpressions.  Click on
"ae" to reveal the assignment toolbar.

The assignment toolbar has "Test Suites" and "Solutions" on the left; these are
visible to students and TAs. The items on the right-hand side are only
visible to TAs.

Students submit test suites from the "Test Suite" pane. Submitted tests run
against the TA-written solution, which is visible to TAs under the "Solution"
pane. If the test suite passes the TA-written solution, it appears under
"Pending Tests". If the test suite signals an error, TAs are not notified.
However, students frequently blame TAs for writing faulty solutions or
specifications.  Sometimes, the TA solution is faulty,  so TestFest allows the
solution to be changed and allows previously accepted to be rejected.

While the assignment is out, TAs must periodically check and accept/reject
submitted tests under "Pending Tests". Once a test is accepted by the TA, it is
added to a global pool of tests. Submitted solutions are tested against all
tests in the pool.

Creating New Users
------------------

You can invite new users from the command-line:

  $ cd src
  $ racket users.rkt arjun@cs.brown.edu sk@cs.brown.edu

This command uses email addresses as usernames, generates new passwords,
and notifies users. A shorter variant:

  $ racket users.rkt --suffix "@cs.brown.edu" arjun sk
  
Creating New Assignments
------------------------

The TestFest Web application allows TAs to create new PLAI and PLAI-GC based
assignments. New kinds of assignment require some programming. You'll need to
add a case for checking test suites:

  https://github.com/brownplt/testfest/blob/master/src/background.rkt#L120

and for checking solutions:

  https://github.com/brownplt/testfest/blob/master/src/background.rkt#L48

TestFest represents students' submissions as "solution" and "test-suite"
records, both defined in types.rkt:

https://github.com/brownplt/testfest/blob/master/src/types.rkt#L41

For example, the run-test function sandboxes and runs a single test suite and
solution:

  https://github.com/brownplt/testfest/blob/master/src/background.rkt#L221 

Run-test produces an s-expression that represents test results. This
s-expression is printed to a string by result->string/port:

  https://github.com/brownplt/testfest/blob/master/src/background.rkt#L13

The result is stored in the solution-status field of the solution:

  https://github.com/brownplt/testfest/blob/master/src/background.rkt#L67

This result is displayed to students, so it must not print confidential
information, such as other students' tests and solutions.
