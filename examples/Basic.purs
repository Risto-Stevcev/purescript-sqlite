module Example.Basic where

import Prelude (pure, bind, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Sqlite.Core (DbMode(ReadWriteCreate), SQLITE, SqlRows, close, get, run, connect)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.TemplateString ((<->))
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Data.Show (class Show, show)


instance loremIsForeign :: IsForeign Lorem where
  read obj = do
    n <- readProp "info" obj
    pure $ Lorem { info: n }

instance showLorem :: Show Lorem where
  show (Lorem a) = a.info

data Lorem = Lorem { info :: String }

main :: forall e. Eff (err :: EXCEPTION, console :: CONSOLE, sqlite :: SQLITE | e)
                  (Canceler ( sqlite :: SQLITE, console :: CONSOLE | e ))
main = launchAff $ do
  db <- connect ":memory:" ReadWriteCreate
  run db "CREATE TABLE IF NOT EXISTS lorem (info TEXT)"
  run db ("INSERT INTO lorem VALUES (${value})" <-> [ "value" /\ "fooish!" ])
  rows <- get db "SELECT * from lorem" :: SqlRows Lorem
  close db

  case rows of
    Left err -> log (message err)
    Right rows' -> log $ show rows' 
