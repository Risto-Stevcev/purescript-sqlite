module Sqlite.Core where

import Prelude (class Show, Unit, pure, bind, show, ($))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Data.Function.Uncurried (Fn3, Fn2, Fn0, runFn3, runFn2, runFn0, mkFn2)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class IsForeign, read)
import Data.HObject.Primitive (class Primitive)

-- | The file mode when connection to the database
data DbMode = ReadOnly | ReadWrite | Create | ReadOnlyCreate | ReadWriteCreate

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
type SqlRow a  = forall e. IsForeign a => Aff ( sqlite :: SQLITE | e ) (Either Error a)
type SqlRows a = forall e. IsForeign a => Aff ( sqlite :: SQLITE | e ) (Either Error (Array a))


-- | Sets the debug mode for sqlite to verbose
setVerbose :: Unit
setVerbose = runFn0 _setVerbose


connect
  :: forall e
   . String 
  -> DbMode
  -> Aff ( sqlite :: SQLITE | e ) DbConnection
connect filename mode = runFn3 _connect filename mode false

-- | Uses sqlite's built-in cache to avoid opening the same database multiple times
connectCached
  :: forall e
   . String
  -> DbMode
  -> Aff ( sqlite :: SQLITE | e ) DbConnection
connectCached filename mode = runFn3 _connect filename mode true


close
  :: forall e
   . DbConnection
  -> Aff (sqlite :: SQLITE | e) Unit
close = _close
 

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


run
  :: forall e
   . DbConnection
  -> SqlQuery
  -> Aff ( sqlite :: SQLITE | e ) Unit
run = runFn2 _run


get
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRows a
get db query = do
  attempted <- attempt $ runFn2 _get db query
  pure $ readGet attempted 

  where
    readGet :: Either Error Foreign -> Either Error (Array a)
    readGet (Left err)   = Left err
    readGet (Right rows) = either (\err -> Left (error $ show err)) Right $ read rows


getOne
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRow a
getOne db query = do
  attempted <- attempt $ runFn2 _getOne db query
  pure $ readGet attempted
 
  where
    readGet :: Either Error Foreign -> Either Error a
    readGet (Left err)   = Left err
    readGet (Right row) = either (\err -> Left (error $ show err)) Right $ read row




stmtPrepare
  :: forall e
   . DbConnection
  -> SqlQuery
  -> Aff ( sqlite :: SQLITE | e ) DbStatement
stmtPrepare = runFn2 _stmtPrepare
 

stmtBind
  :: forall e
   . DbStatement
  -> SqlParams
  -> Aff ( sqlite :: SQLITE | e ) Unit
stmtBind = runFn2 _stmtBind


stmtReset
  :: forall e
   . DbStatement
  -> Aff ( sqlite :: SQLITE | e ) DbStatement
stmtReset = _stmtReset
 

stmtFinalize
  :: forall e
   . DbStatement
  -> Aff ( sqlite :: SQLITE | e ) Unit
stmtFinalize = _stmtFinalize


stmtRun
  :: forall e
   . DbStatement
  -> SqlParams
  -> Aff ( sqlite :: SQLITE | e ) Unit
stmtRun = runFn2 _stmtRun


stmtGet
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRows a
stmtGet stmt query = do
  attempted <- attempt $ runFn2 _stmtGet stmt query
  pure $ readStmt attempted 

  where
    readStmt :: Either Error Foreign -> Either Error (Array a)
    readStmt (Left err)   = Left err
    readStmt (Right rows) = either (\err -> Left (error $ show err)) Right $ read rows


stmtGetOne
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRow a
stmtGetOne stmt query = do
  attempted <- attempt $ runFn2 _stmtGetOne stmt query
  pure $ readStmt attempted 

  where
    readStmt :: Either Error Foreign -> Either Error a
    readStmt (Left err)  = Left err
    readStmt (Right row) = either (\err -> Left (error $ show err)) Right $ read row




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

foreign import _setVerbose :: Fn0 Unit

foreign import _connect
  :: forall e
   . Fn3
     String
     DbMode
     Boolean
     (Aff ( sqlite :: SQLITE | e ) DbConnection)

foreign import _close
  :: forall e
   . DbConnection
  -> (Aff ( sqlite :: SQLITE | e ) Unit)

foreign import _listen
  :: forall e a
   . Fn3
     DbConnection
     String
     (DbListener e)
     (Eff e a)

foreign import _run
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff ( sqlite :: SQLITE | e ) Unit)

foreign import _get
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff ( sqlite :: SQLITE | e ) Foreign)

foreign import _getOne
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff ( sqlite :: SQLITE | e ) Foreign)

foreign import _stmtPrepare
  :: forall e
   . Fn2
     DbConnection
     SqlQuery
     (Aff ( sqlite :: SQLITE | e ) DbStatement)

foreign import _stmtBind 
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff ( sqlite :: SQLITE | e ) Unit)

foreign import _stmtReset
  :: forall e
   . DbStatement
  -> Aff ( sqlite :: SQLITE | e ) DbStatement

foreign import _stmtFinalize
  :: forall e
   . DbStatement
  -> Aff ( sqlite :: SQLITE | e ) Unit

foreign import _stmtRun 
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff ( sqlite :: SQLITE | e ) Unit)

foreign import _stmtGet
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff ( sqlite :: SQLITE | e ) Foreign)

foreign import _stmtGetOne
  :: forall e
   . Fn2
     DbStatement
     SqlParams
     (Aff ( sqlite :: SQLITE | e ) Foreign)
