module Test.Sqlite.Trans where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Prelude (class Eq, Unit, bind, discard, pure, unit, ($), (<>), (=<<), (==))
import Sqlite.Core (SQLITE, DbMode(ReadWriteCreate))
import Sqlite.Trans (closeT, getT, runT, connectT)
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


instance decodeLorem :: Decode Lorem where
  decode obj = do
    n <- decode =<< readProp "info" obj
    pure $ Lorem { info: n }

instance loremEq :: Eq Lorem where
  eq (Lorem a) (Lorem b) = a.info == b.info

data Lorem = Lorem { info :: String }

main :: forall e. Eff ( avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT, sqlite :: SQLITE | e ) Unit
main = runTest do
  suite "Sqlite.Trans" do
    test "functionality" do
      let infoValue = "fooish!"
      assert "true" true
      result <- runExceptT $ do
        db <- connectT ":memory:" ReadWriteCreate
        _ <- runT db "CREATE TABLE IF NOT EXISTS lorem (info TEXT)"
        _ <- runT db ("INSERT INTO lorem VALUES (\"" <> infoValue <> "\")")
        rows <- getT db "SELECT * from lorem"
        closeT db
        pure rows

      case result of
        Right rows@[Lorem _] -> assert "gets the rows that are populated" $ rows == [Lorem {info: infoValue}]
        Right _  -> pure unit
        Left err -> assert (message err) false
