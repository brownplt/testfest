module CS173.Test.AssignmentAdmin where

import CS173.Actions
import CS173.Data
import Control.Monad
import Control.Monad.Trans

test = do
  initDatabase
  liftIO $ putStrLn "Database initialized"
  addAssignment (newAssignment "rinterp" "Rudimentary Interpreter")
  [Assignment{assignmentId=id}] <- getAllAssignments
  unless (id == "rinterp") $ fail "failed"
  return ()
