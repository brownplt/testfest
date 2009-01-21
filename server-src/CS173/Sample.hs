module CS173.Sample
  ( makeSample
  ) where

import CS173.Actions
import CS173.Tourney
import CS173.Data
import CS173.Config
import Database.CouchDB
import Control.Monad.Trans

makeSample :: Config -> IO ()
makeSample cfg = runConfig cfg $ do
  liftIO $ runCouchDB' $ do
    initDatabase
    newLogin (doc "arjun") "arjun"
    solutionId <- newSubmission aeSolution
    addAssignment $ Assignment (doc "ae") "Arithmetic Interpreter" True Nothing
                      aeTestCmd aeSolnCmd
                      "" "plai" True solutionId
    getAndUpdateDoc (db "users") (doc "arjun")
      (\u -> return $ u { userAdmin = True })
    return ()

aeNames = "parse calc AE AE? num num? num-n set-num-n! add add? \
\ add-lhs add-rhs set-add-lhs! set-add-rhs! sub sub? sub-lhs sub-rhs \
\ set-sub-lhs! set-sub-rhs!"

aeSolnCmd = "%p/plai-test.ss -ar -i \"" ++ aeNames ++ "\" %s %t"

aeTestCmd = "%p/plai-test.ss -ar -i \"" ++ aeNames ++ "\" %s %t"

aeSolution =
  "#lang plai \
\(define-type AE \
\  [num (n integer?)]\
\  [add (lhs AE?) (rhs AE?)]\
\  [sub (lhs AE?) (rhs AE?)])\
\ \
\(define (parse sexp)\
\  (cond\
\    [(number? sexp) (num sexp)]\
\    [(and (list? sexp) (= 3 (length sexp)))\
\     (case (first sexp)\
\       [(+) (add (parse (second sexp)) (parse (third sexp)))]\
\       [(-) (sub (parse (second sexp)) (parse (third sexp)))]\
\       [else (error 'parse \"unknown operator\")])]\
\    [else (error 'parse \"unknown term\")]))\
\ \
\(define (calc an-ae)\
\  (type-case AE an-ae\
\    [num (n) n]\
\    [add (lhs rhs) (+ (calc lhs) (calc rhs))]\
\    [sub (lhs rhs) (- (calc lhs) (calc rhs))]))"
