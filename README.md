# purescript-sqlite

[![Latest release](http://img.shields.io/bower/v/purescript-sqlite.svg)](https://github.com/Risto-Stevcev/purescript-sqlite/releases)


## Example

```purescript
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
```

This example logs `[fooish!]` to the console.

For more examples, see the unit tests and examples folder.
