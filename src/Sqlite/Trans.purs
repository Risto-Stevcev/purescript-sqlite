module Sqlite.Trans where

import Prelude (Unit, pure, bind, ($), (>>=))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Foreign.Class (class IsForeign)
import Sqlite.Core

type SqlRowT a  = forall e. IsForeign a => ExceptT Error (Aff ( sqlite :: SQLITE | e )) a
type SqlRowsT a = forall e. IsForeign a => ExceptT Error (Aff ( sqlite :: SQLITE | e )) (Array a)


valueToRight
  :: forall a e
   . Aff ( sqlite :: SQLITE | e ) a
  -> Aff ( sqlite :: SQLITE | e ) (Either Error a)
valueToRight x = do
  y <- x
  pure (Right y)


connectT
  :: forall e
   . String 
  -> DbMode
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) DbConnection
connectT filename mode = ExceptT (attempt $ connect filename mode) 

connectCachedT
  :: forall e
   . String
  -> DbMode
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) DbConnection
connectCachedT filename mode = ExceptT (attempt $ connectCached filename mode)

closeT
  :: forall e
   . DbConnection
  -> ExceptT Error (Aff (sqlite :: SQLITE | e)) Unit
closeT db = ExceptT (attempt $ close db)

runT
  :: forall e
   . DbConnection
  -> SqlQuery
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) Unit 
runT db query = ExceptT (attempt $ run db query)

getT
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRowsT a
getT db query = ExceptT (get db query)

getOneT
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRowT a
getOneT db query = ExceptT (getOne db query)

stmtPrepareT
  :: forall e
   . DbConnection
  -> SqlQuery
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) DbStatement
stmtPrepareT db query = ExceptT (attempt $ stmtPrepare db query)

stmtBindT
  :: forall e
   . DbStatement
  -> SqlParams
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) Unit
stmtBindT stmt params = ExceptT (attempt $ stmtBind stmt params)

stmtResetT
  :: forall e
   . DbStatement
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) DbStatement
stmtResetT db = ExceptT (valueToRight $ stmtReset db)

stmtFinalizeT
  :: forall e
   . DbStatement
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) Unit
stmtFinalizeT db = ExceptT (valueToRight $ stmtFinalize db)

stmtRunT
  :: forall e
   . DbStatement
  -> SqlParams
  -> ExceptT Error (Aff ( sqlite :: SQLITE | e )) Unit
stmtRunT stmt params = ExceptT (attempt $ stmtRun stmt params)

stmtGetT
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRowsT a
stmtGetT stmt query = ExceptT (stmtGet stmt query)

stmtGetOneT
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRowT a
stmtGetOneT stmt query = ExceptT (stmtGetOne stmt query)
