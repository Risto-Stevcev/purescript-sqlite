module Test.Main where

import Prelude (discard, Unit)
import Sqlite.Core (SQLITE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)
import Test.Unit.Console (TESTOUTPUT)
import Test.Sqlite.Core (main) as Sqlite
import Test.Sqlite.Trans (main) as SqliteTrans


main :: Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, sqlite :: SQLITE ) Unit
main = do
  Sqlite.main
  SqliteTrans.main
