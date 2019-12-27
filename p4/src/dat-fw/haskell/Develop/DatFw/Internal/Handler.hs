
module Develop.DatFw.Internal.Handler
where
import Develop.DatFw.Internal.Types

import           Network.Wai
import           Network.HTTP.Types as H

import           Data.Monoid
import qualified Data.ByteString as B
import           Data.IORef
import           Control.Monad.IO.Class

lookupPostQuery :: MonadHandler m => m Query
lookupPostQuery = liftHandler $ do
    st <- get
    case hsPostQ st of
        Just q -> pure q
        Nothing -> do
            hdata <- HandlerFor pure
            q <- liftIO $ getPostQuery (handlerReq hdata)
            put st{ hsPostQ = Just q }
            pure q
  where
        getPostQuery :: Request -> IO Query
        getPostQuery req =
            parseQuery <$> getBody req
        getBody req = do
            b <- requestBody req
            if B.null b then pure B.empty
            else do
                bs <- getBody req
                pure $ b <> bs

--------------------------------------------------------------
-- Internal

askRunHandlerEnv :: MonadHandler m => m (RunHandlerEnv (HandlerSite m) (HandlerSite m))
askRunHandlerEnv = liftHandler $ HandlerFor $ pure . handlerEnv

get :: MonadHandler m => m HandlerState
get = liftHandler $ HandlerFor $ readIORef . handlerStR

put :: MonadHandler m => HandlerState -> m ()
put x = liftHandler $ HandlerFor $ flip writeIORef x . handlerStR

modify :: MonadHandler m => (HandlerState -> HandlerState) -> m ()
modify f = liftHandler $ HandlerFor $ flip modifyIORef f . handlerStR


modSession :: (SessionMap -> SessionMap) -> HandlerState -> HandlerState
modSession f st = st{ hsSession = f (hsSession st) }


