module Sqlite.Core where

import Prelude
import Data.Int.Bits as Bits
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class IsForeign, read)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, mkFn2, runFn2, runFn3, runFn4, runFn5)
import Data.HObject.Primitive (class Primitive)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)


foreign import _OPEN_READONLY :: Int
foreign import _OPEN_READWRITE :: Int
foreign import _OPEN_CREATE :: Int

-- | The file mode when connection to the database
data DbMode = ReadOnly | ReadWrite | Create | ReadOnlyCreate | ReadWriteCreate

modeToInt :: DbMode -> Int
modeToInt ReadOnly = _OPEN_READONLY
modeToInt ReadWrite = _OPEN_READWRITE
modeToInt Create = _OPEN_CREATE
modeToInt ReadOnlyCreate = _OPEN_READONLY `Bits.or` _OPEN_CREATE
modeToInt ReadWriteCreate = _OPEN_READWRITE `Bits.or` _OPEN_CREATE

-- | Corresponds to a database event
data DbEvent e a
  = Open    (Unit   -> Eff e a)
  | Close   (Unit   -> Eff e a)
  | Error   (Error  -> Eff e a)
  | Trace   (String -> Eff e a)
  | Profile (String -> Int -> Eff e a)

data SqlParam = SqlString String | SqlInt Int | SqlNumber Number | SqlBoolean Boolean

instance strSqlParam :: Primitive String SqlParam where
  mkPrim = SqlString

instance intSqlParam :: Primitive Int SqlParam where
  mkPrim = SqlInt

instance numSqlParam :: Primitive Number SqlParam where
  mkPrim = SqlNumber

instance boolSqlParam :: Primitive Boolean SqlParam where
  mkPrim = SqlBoolean

instance showSqlParam :: Show SqlParam where
  show (SqlString a)  = a
  show (SqlInt i)     = show i
  show (SqlNumber n)  = show n
  show (SqlBoolean b) = show b

type SqlParams = Array (Tuple String SqlParam)

type SqlQuery = String
type SqlRow  a = forall e. IsForeign a => Aff (sqlite :: SQLITE | e) a
type SqlRows a = forall e. IsForeign a => Aff (sqlite :: SQLITE | e) (Array a)


-- | Sets the debug mode for sqlite to verbose
setVerbose :: forall e. Eff (sqlite :: SQLITE | e) Unit
setVerbose = _setVerbose


connect
  :: forall e
   . String
  -> DbMode
  -> Aff (sqlite :: SQLITE | e) DbConnection
connect filename dbMode = makeAff $ runFn5 _connect filename mode false
  where
  mode = modeToInt dbMode

-- | Uses sqlite's built-in cache to avoid opening the same database multiple times
connectCached
  :: forall e
   . String
  -> DbMode
  -> Aff (sqlite :: SQLITE | e) DbConnection
connectCached filename dbMode = makeAff $ runFn5 _connect filename mode true
  where
  mode = modeToInt dbMode


close
  :: forall e
   . DbConnection
  -> Aff (sqlite :: SQLITE | e) Unit
close db = makeAff _close'
  where
  _close' onError onSuccess = runFn3 _close db onError $ onSuccess unit


-- | "lastID" result value should be used only for INSERT queries,
-- | "changes" result value should be used only for UPDATE and DELETE queries.
type RunResult = { lastID :: Int, changes :: Int }


run
  :: forall e
   . DbConnection
  -> SqlQuery
  -> Aff (sqlite :: SQLITE | e) RunResult
run db query = makeAff $ runFn4 _run db query


readRow :: forall a e. IsForeign a => Foreign -> Aff (sqlite :: SQLITE | e) a
readRow = read >>> runExcept >>> either (show >>> error >>> throwError) pure


getOne
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRow (Maybe a)
getOne db query = do
  row <- toMaybe <$> (makeAff $ runFn4 _getOne db query)
  maybe (pure Nothing) readRow row


get
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRows a
get db query = do
  rows <- makeAff $ runFn4 _get db query
  sequence $ readRow <$> rows


stmtPrepare
  :: forall e
   . DbConnection
  -> SqlQuery
  -> Aff (sqlite :: SQLITE | e) DbStatement
stmtPrepare db query = makeAff $ runFn4 _stmtPrepare db query


stmtBind
  :: forall e
   . DbStatement
  -> SqlParams
  -> Aff (sqlite :: SQLITE | e) Unit
stmtBind db query = makeAff bind
  where
  bind onError onSuccess = runFn4 _stmtBind db query onError $ onSuccess unit


stmtReset
  :: forall e
   . DbStatement
  -> Aff (sqlite :: SQLITE | e) Unit
stmtReset stmt = makeAff _stmtReset'
  where
  _stmtReset' onError onSuccess = runFn2 _stmtReset stmt $ onSuccess unit


stmtFinalize
  :: forall e
   . DbStatement
  -> Aff (sqlite :: SQLITE | e) Unit
stmtFinalize stmt = makeAff _stmtFinalize'
  where
  _stmtFinalize' onError onSuccess = runFn2 _stmtFinalize stmt $ onSuccess unit


stmtRun
  :: forall e
   . DbStatement
  -> SqlParams
  -> Aff ( sqlite :: SQLITE | e ) RunResult
stmtRun stmt params = makeAff $ runFn4 _stmtRun stmt params


stmtGetOne
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRow (Maybe a)
stmtGetOne stmt query = do
  row <- toMaybe <$> (makeAff $ runFn4 _stmtGetOne stmt query)
  maybe (pure Nothing) readRow row


stmtGet
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRows a
stmtGet stmt query = do
  rows <- makeAff $ runFn4 _stmtGet stmt query
  sequence $ readRow <$> rows


-- | Listener for the database open event
listen
  :: forall e a
   . DbConnection
  -> DbEvent e a
  -> Eff e a
listen db (Open  callback)   = runFn3 _listen db "open"    (_dbListener callback)
listen db (Close callback)   = runFn3 _listen db "close"   (_dbListener callback)
listen db (Error callback)   = runFn3 _listen db "error"   (_dbListener callback)
listen db (Trace callback)   = runFn3 _listen db "trace"   (_dbListener callback)
listen db (Profile callback) = runFn3 _listen db "profile" (_dbListenerFn2 $ mkFn2 callback)


foreign import data DbStatement :: *

foreign import data DbConnection :: *

foreign import data SQLITE :: !

-- | A boxed function that can be used as a listener. This is necessary
-- | due to the underling implementation of Eff functions.
foreign import data DbListener :: # ! -> *

-- | Creates a DbListener from a normal PureScript Eff function for the
-- | listen function.
foreign import _dbListener
  :: forall e a b
   . (a -> Eff e b)
  -> DbListener e

-- | Creates a DbListener for a callback that takes two arguments
foreign import _dbListenerFn2
  :: forall e a b c
   . (Fn2 a b (Eff e c))
  -> DbListener e

foreign import _setVerbose :: forall e. Eff (sqlite :: SQLITE | e) Unit

foreign import _connect
  :: forall e
   . Fn5
     String
     Int
     Boolean
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (DbConnection -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _close
  :: forall e
   . Fn3
     DbConnection
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _run
  :: forall e
   . Fn4
     DbConnection
     SqlQuery
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (RunResult -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _getOne
  :: forall e
   . Fn4
     DbConnection
     SqlQuery
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (Nullable Foreign -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff ( sqlite :: SQLITE | e ) Unit)

foreign import _get
  :: forall e
   . Fn4
     DbConnection
     SqlQuery
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (Array Foreign -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff ( sqlite :: SQLITE | e ) Unit)


foreign import _stmtPrepare
  :: forall e
   . Fn4
     DbConnection
     SqlQuery
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (DbStatement -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _stmtBind
  :: forall e
   . Fn4
     DbStatement
     SqlParams
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _stmtReset
  :: forall e
   . Fn2
     DbStatement
     (Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _stmtFinalize
  :: forall e
   . Fn2
     DbStatement
     (Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _stmtRun
  :: forall e
   . Fn4
     DbStatement
     SqlParams
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (RunResult -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _stmtGetOne
  :: forall e
   . Fn4
     DbStatement
     SqlParams
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (Nullable Foreign -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)

foreign import _stmtGet
  :: forall e
   . Fn4
     DbStatement
     SqlParams
     (Error -> Eff (sqlite :: SQLITE | e) Unit)
     (Array Foreign -> Eff (sqlite :: SQLITE | e) Unit)
     (Eff (sqlite :: SQLITE | e) Unit)


foreign import _listen
  :: forall e a
   . Fn3
     DbConnection
     String
     (DbListener e)
     (Eff e a)
