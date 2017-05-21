module Sqlite.Core where

import Prelude
import Data.Int.Bits as Bits
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, F)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.HObject.Primitive (class Primitive)
import Data.Maybe (Maybe(..), maybe)
import Data.Undefinable (Undefinable, toMaybe)
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
type SqlRow  a = forall e. Aff (sqlite :: SQLITE | e) (Maybe a)
type SqlRows a = forall e. Aff (sqlite :: SQLITE | e) (Array a)
type SqlRowReader a = Foreign -> F a


-- | Sets the debug mode for sqlite to verbose
setVerbose :: forall e. Eff (sqlite :: SQLITE | e) Unit
setVerbose = _setVerbose


connect
  :: forall e
   . String
  -> DbMode
  -> Aff (sqlite :: SQLITE | e) DbConnection
connect filename dbMode = runFn3 _connect filename mode true
  where
  mode = modeToInt dbMode

-- | Uses sqlite's built-in cache to avoid opening the same database multiple times
connectCached
  :: forall e
   . String
  -> DbMode
  -> Aff (sqlite :: SQLITE | e) DbConnection
connectCached filename dbMode = runFn3 _connect filename mode true
  where
  mode = modeToInt dbMode


close
  :: forall e
   . DbConnection
  -> Aff (sqlite :: SQLITE | e) Unit
close = _close


-- | "lastID" result value should be used only for INSERT queries,
-- | "changes" result value should be used only for UPDATE and DELETE queries.
type RunResult = { lastID :: Int, changes :: Int }


run
  :: forall e
   . DbConnection
  -> SqlQuery
  -> Aff (sqlite :: SQLITE | e) RunResult
run = runFn2 _run


readRow :: forall a e. SqlRowReader a -> Foreign -> Aff (sqlite :: SQLITE | e) a
readRow read = read >>> runExcept >>> either (show >>> error >>> throwError) pure


getOne
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRowReader a
  -> SqlRow a
getOne db query read = do
  row <- toMaybe <$> (runFn2 _getOne db query)
  maybe (pure Nothing) (map Just <<< readRow read) row


get
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRowReader a
  -> SqlRows a
get db query read = do
  rows <- runFn2 _get db query
  sequence $ readRow read <$> rows


stmtPrepare
  :: forall e
   . DbConnection
  -> SqlQuery
  -> Aff (sqlite :: SQLITE | e) DbStatement
stmtPrepare = runFn2 _stmtPrepare


stmtBind
  :: forall e
   . DbStatement
  -> SqlParams
  -> Aff (sqlite :: SQLITE | e) Unit
stmtBind = runFn2 _stmtBind


stmtReset
  :: forall e
   . DbStatement
  -> Aff (sqlite :: SQLITE | e) Unit
stmtReset = _stmtReset


stmtFinalize
  :: forall e
   . DbStatement
  -> Aff (sqlite :: SQLITE | e) Unit
stmtFinalize = _stmtFinalize


stmtRun
  :: forall e
   . DbStatement
  -> SqlParams
  -> Aff ( sqlite :: SQLITE | e ) RunResult
stmtRun = runFn2 _stmtRun


stmtGetOne
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRowReader a
  -> SqlRow a
stmtGetOne stmt query read = do
  row <- toMaybe <$> (runFn2 _stmtGetOne stmt query)
  maybe (pure Nothing) (map Just <<< readRow read) row


stmtGet
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRowReader a
  -> SqlRows a
stmtGet stmt query read = do
  rows <- runFn2 _stmtGet stmt query
  sequence $ readRow read <$> rows


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


foreign import data DbStatement :: Type

foreign import data DbConnection :: Type

foreign import data SQLITE :: Effect

-- | A boxed function that can be used as a listener. This is necessary
-- | due to the underling implementation of Eff functions.
foreign import data DbListener :: # Effect -> Type

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
   . Fn3
     String
     Int
     Boolean
     (Aff (sqlite :: SQLITE | e) DbConnection)

foreign import _close
  :: forall e
   . DbConnection
  -> (Aff (sqlite :: SQLITE | e) Unit)

foreign import _run
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff (sqlite :: SQLITE | e) RunResult)

foreign import _getOne
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff (sqlite :: SQLITE | e) (Undefinable Foreign))

foreign import _get
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff (sqlite :: SQLITE | e) (Array Foreign))


foreign import _stmtPrepare
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff (sqlite :: SQLITE | e) DbStatement)

foreign import _stmtBind
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff (sqlite :: SQLITE | e) Unit)

foreign import _stmtReset
  :: forall e
   . DbStatement
  -> (Aff (sqlite :: SQLITE | e) Unit)

foreign import _stmtFinalize
  :: forall e
   . DbStatement
  -> (Aff (sqlite :: SQLITE | e) Unit)

foreign import _stmtRun
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff (sqlite :: SQLITE | e) RunResult)

foreign import _stmtGetOne
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff (sqlite :: SQLITE | e) (Undefinable Foreign))

foreign import _stmtGet
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff (sqlite :: SQLITE | e) (Array Foreign))


foreign import _listen
  :: forall e a
   . Fn3
     DbConnection
     String
     (DbListener e)
     (Eff e a)
