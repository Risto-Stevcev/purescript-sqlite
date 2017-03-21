module Test.Sqlite.Core where

import Control.Monad.Aff (liftEff', launchAff, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Data.Either (Either(..), isLeft)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.HObject.Primitive ((/^\))
import Prelude (class Eq, Unit, unit, pure, bind, map, show, (==), ($))
import Sqlite.Core (SqlRows, SQLITE, DbConnection, DbEvent(..), DbMode(..), connect, listen, close, get, stmtFinalize, stmtRun, stmtPrepare, run)
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


instance loremIsForeign :: IsForeign Lorem where
  read obj = do
    n <- readProp "info" obj
    pure $ Lorem { info: n }

instance loremEq :: Eq Lorem where
  eq (Lorem a) (Lorem b) = a.info == b.info

data Lorem = Lorem { info :: String }


testConnectionListeners
  :: forall e
   . DbConnection
  -> Eff ( err :: EXCEPTION, console :: CONSOLE | e ) Unit
testConnectionListeners db = do
  listen db (Open  (\_ -> launchAff $ assert "Db listener 'open' called" true))
  listen db (Close (\_ -> launchAff $ assert "Db listener 'close' called" true))
  listen db (Error (\err -> launchAff $ assert (message err) false))
  --listen db (Trace (\str -> log str))
  --listen db (Profile (\str time -> log $ "(" <> show time <> "ms) " <> str))
  pure unit


main :: forall e. Eff ( avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT, sqlite :: SQLITE | e ) Unit
main = runTest do
  suite "Sqlite.Core" do
    test "functionality" do
      db <- connect ":memory:" ReadWriteCreate
      liftEff' $ testConnectionListeners db

      run db "CREATE TABLE IF NOT EXISTS lorem (info TEXT)"

      stmt <- stmtPrepare db "INSERT INTO lorem VALUES ($value)"

      liftEff' $ forE 0 10 \i -> do
        launchAff $ stmtRun stmt [ "$value" /^\ i ]
        pure unit

      stmtFinalize stmt

      rows <- get db "SELECT * from lorem" :: SqlRows Lorem
      assert "Rows do not match expected output" $ rows == map (\x -> Lorem {info: show x}) [0,1,2,3,4,5,6,7,8,9]

      close db

    test "failed connect" do
      failDb <- attempt $ connect "someNonexistentFile" ReadOnly
      assert "Db connection failed" $ (isLeft failDb) == true

      case failDb of
        Right _  -> assert "Db connection should not have succeeded" false
        Left err -> assert "Db error object has the wrong message" $ (message err) == "SQLITE_CANTOPEN: unable to open database file"

    test "setVerbose" do
      -- setVerbose call can't fail
      -- This test should somehow check stacktrace
      -- of a failed sqlite method call
      pure unit
