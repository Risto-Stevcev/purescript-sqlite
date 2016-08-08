module Test.Sqlite.Trans where

import Prelude (class Eq, Unit, unit, pure, bind, (==), ($))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Test.Unit (test, suite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Sqlite.Trans (SqlRowsT, closeT, getT, runT, connectT)
import Sqlite.Core (SQLITE, DbMode(ReadWriteCreate))
import Data.Either (Either(..))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Tuple.Nested ((/\))
import Data.TemplateString ((<->))
import Control.Monad.Except.Trans (runExceptT)


instance loremIsForeign :: IsForeign Lorem where
  read obj = do
    n <- readProp "info" obj
    pure $ Lorem { info: n }

instance loremEq :: Eq Lorem where
  eq (Lorem a) (Lorem b) = a.info == b.info

data Lorem = Lorem { info :: String }

main :: forall e. Eff ( avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT, sqlite :: SQLITE | e ) Unit
main = runTest do
  suite "Sqlite.Trans" do
    test "functionality" do
      assert "true" true
      result <- runExceptT $ do
        db <- connectT ":memory:" ReadWriteCreate
        runT db "CREATE TABLE IF NOT EXISTS lorem (info TEXT)"
        runT db ("INSERT INTO lorem VALUES (${value})" <-> [ "value" /\ "fooish!" ])
        rows <- getT db "SELECT * from lorem" :: SqlRowsT Lorem
        closeT db
        pure rows

      case result of
        Right rows@[Lorem _] -> assert "gets the rows that are populated" $ rows == [Lorem {info: "fooish!"}]
        Right _  -> pure unit
        Left err -> assert (message err) false
