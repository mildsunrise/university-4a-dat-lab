
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
--
-- @author Jordi Forga
--
module Develop.DatFw.Handler
    (-- * MonadHandler
      HandlerFor
    , MonadHandler(..), HandlerSite, SubHandlerSite
    -- * Application information
    , getSite, getsSite, UrlRender, getUrlRender, getUrlRenderNoParams, applyUrlRenderTo, withUrlRenderer
    -- * Request information
    , getCurrentRoute, getRequest, lookupGetParam, lookupGetParams, lookupPostParam, lookupPostParams
    -- * Session
    , lookupSession, lookupSessionBS, getSession, setSession, setSessionBS, deleteSession
    -- ** Session/Messages
    , setMessage, getMessage
    -- ** Session/Ultimate destination
    , setUltDest, setUltDestRoute, setUltDestCurrent, redirectUltDest
    -- * Short-circuits
    -- ** Short-circuits / Errors
    , notFound, badMethod, invalidArgs, notAuthenticated, permissionDenied
    -- ** Short-circuits / Redirections
    , redirect, redirectRoute, redirectWith, redirectRouteWith
    -- * Misc
    , newIdent
    -- * Low level
    , sendServerResponse
    -- * Subsites
    , SubHandlerFor
    , getSubSite, getsSubSite, getRouteToMaster, getSubCurrentRoute
    )
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Internal.Handler
import Develop.DatFw.Content (Html)

import           Network.Wai
import           Network.HTTP.Types as H

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Control.Exception
import           Control.Monad.IO.Class

--------------------------------------------------------------
-- Application information

-- | Get the site application argument.
getSite :: MonadHandler m => m (HandlerSite m)
getSite =
    rheSite <$> askRunHandlerEnv

-- | Get a specific component of the master site application argument.
--   Analogous to the 'gets' function for operating on 'StateT'.
getsSite :: MonadHandler m => (HandlerSite m -> a) -> m a
getsSite f =
    f <$> getSite

-- | Get the URL rendering function.
getUrlRender :: MonadHandler m => m (UrlRender (Route (HandlerSite m)))
getUrlRender =
    rheUrlRender <$> askRunHandlerEnv

getUrlRenderNoParams :: MonadHandler m => m (Route (HandlerSite m) -> Text)
getUrlRenderNoParams = do
    urlRender <- getUrlRender
    pure $ flip urlRender []

-- | Provide a URL rendering function to the given function and return the result. Useful for processing templates.
applyUrlRenderTo :: MonadHandler m => (UrlRender (Route (HandlerSite m)) -> out) -> m out
applyUrlRenderTo f = do
    render <- getUrlRender
    pure $ f render

{-# DEPRECATED withUrlRenderer "Use 'applyUrlRenderTo' instead" #-}
withUrlRenderer :: MonadHandler m => (UrlRender (Route (HandlerSite m)) -> out) -> m out
withUrlRenderer = applyUrlRenderTo

--------------------------------------------------------------
-- Request information

-- | Get the route requested by the user. If this is a 404 response - where the user requested an invalid route- this function will return Nothing.
getCurrentRoute :: MonadHandler m => m (Maybe (Route (HandlerSite m)))
getCurrentRoute =
    rheRoute <$> askRunHandlerEnv

getRequest :: MonadHandler m => m Request
getRequest =
    liftHandler $ HandlerFor $ pure . handlerReq

-- | Lookup for query string parameters. Returns Nothing if the param does'nt exist.
lookupGetParam :: MonadHandler m => Text -> m (Maybe Text)
lookupGetParam name = do
    vals <- lookupGetParams name
    case vals of
        [] -> pure Nothing
        (v:_) -> pure (Just v)

lookupGetParams :: MonadHandler m => Text -> m [Text]
lookupGetParams name = do
    query <- queryString <$> getRequest
    let nameBytes = T.encodeUtf8 name
    pure (T.decodeUtf8 <$> catMaybes (snd <$> filter ((==) nameBytes . fst) query))

-- | Lookup for POST parameters. Returns Nothing if the param does'nt exist.
lookupPostParam :: MonadHandler m => Text -> m (Maybe Text)
lookupPostParam name = do
    vals <- lookupPostParams name
    case vals of
        [] -> pure Nothing
        (v:_) -> pure (Just v)

lookupPostParams :: MonadHandler m => Text -> m [Text]
lookupPostParams name = do
    query <- lookupPostQuery
    let nameBytes = T.encodeUtf8 name
    pure (T.decodeUtf8 <$> catMaybes (snd <$> filter ((==) nameBytes . fst) query))

--------------------------------------------------------------
-- Session

-- | Lookup for session data. Returns Nothing if the variable in the user's session is not setted.
lookupSession :: MonadHandler m => Text -> m (Maybe Text)
lookupSession =
    ((T.decodeUtf8 <$>) <$>) . lookupSessionBS

-- | Lookup for session data in binary format.
lookupSessionBS :: MonadHandler m => Text -> m (Maybe B.ByteString)
lookupSessionBS n = do
    st <- get
    pure $ M.lookup n (hsSession st)

-- | Get all session variables.
getSession :: MonadHandler m => m SessionMap
getSession = hsSession <$> get

-- | Set a variable in the user's session.
setSession :: MonadHandler m
           => Text -- ^ key
           -> Text -- ^ value
           -> m ()
setSession k = setSessionBS k . T.encodeUtf8

-- | Same as 'setSession', but uses binary data for the value.
setSessionBS :: MonadHandler m
             => Text
             -> B.ByteString
             -> m ()
setSessionBS k v =
    modify $ modSession $ M.insert k v

-- | Unset a user's session variable.
deleteSession :: MonadHandler m
              => Text
              -> m ()
deleteSession k =
    modify $ modSession $ M.delete k

--------------------------------------------------------------
-- Session/Ultimate destination

ultDest_SESSION_KEY :: Text
ultDest_SESSION_KEY = "__ULT"

-- | Set the ultimate destination in the user's session to the given URL.
setUltDest :: MonadHandler m => Text -> m ()
setUltDest url =
    setSession ultDest_SESSION_KEY url

-- | Set the ultimate destination in the user's session to the given route and query parameters.
setUltDestRoute :: MonadHandler m => Route (HandlerSite m) -> [(Text, Text)] -> m ()
setUltDestRoute route params = do
    urlRender <- getUrlRender
    setUltDest (urlRender route params)

-- | Set the ultimate destination in the user's session to the current route.
setUltDestCurrent :: MonadHandler m => m ()
setUltDestCurrent = do
    mburl <- getCurrentRoute
    case mburl of
        Nothing -> pure ()
        Just route -> do
            urlRender <- getUrlRender
            query <- queryString <$> getRequest
            let params = map (\(n,Just v) -> (T.decodeUtf8 n, T.decodeUtf8 v)) $ filter (isJust . snd) query
            setUltDest (urlRender route params)

-- | Redirect to the ultimate destination in the user's session. Clear the value from the session.
--
-- The ultimate destination is set with 'setUltDest' or 'setUltDestRoute'.
--
-- This function uses 'redirect', and thus will perform a temporary redirect to a GET request.
redirectUltDest :: MonadHandler m => Text  -- ^ URL default destination if nothing in session
                                     -> m a
redirectUltDest url = do
    mb <- lookupSession ultDest_SESSION_KEY
    case mb of
        Nothing -> redirect url
        Just u -> do
            deleteSession ultDest_SESSION_KEY
            redirect u


--------------------------------------------------------------
-- Session/Messages

message_SESSION_KEY :: Text
message_SESSION_KEY = "__MSG"

-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessage :: MonadHandler m => Html -> m ()
setMessage msg = do
    setSessionBS message_SESSION_KEY (BL.toStrict $ B.toLazyByteString $ renderHtmlBuilder msg)

-- | Gets the message in the user's session, if available, and then clears the variable.
-- If not available, returns Nothing.
getMessage :: MonadHandler m => m (Maybe Html)
getMessage = do
    mb <- lookupSessionBS message_SESSION_KEY
    case mb of
        Nothing -> pure Nothing
        Just b -> do
            deleteSession message_SESSION_KEY
            pure $ Just $ preEscapedText $ T.decodeUtf8 b


--------------------------------------------------------------
-- Short-circuits

notFound :: MonadHandler m => m a
notFound =
    liftIO $ throwIO (HCError NotFound)

badMethod :: MonadHandler m => m a
badMethod =  do
    req <- getRequest
    liftIO $ throwIO (HCError (BadMethod (requestMethod req)))

invalidArgs :: MonadHandler m => [Text] -> m a
invalidArgs iargs =
    liftIO $ throwIO (HCError (InvalidArgs iargs))

notAuthenticated :: MonadHandler m => m a
notAuthenticated =
    liftIO $ throwIO (HCError NotAuthenticated)

permissionDenied :: MonadHandler m => Text -> m a
permissionDenied msg =
    liftIO $ throwIO (HCError (PermissionDenied msg))

-- | Redirect to the given URL.
-- HTTP status code 303 for HTTP 1.1 clients and 302 for HTTP 1.0
-- (see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.4 HTTP/1.1 documentation>).
-- This is the appropriate choice for a get-following-post technique, which should be the usual use case.
--
-- If you want direct control of the final status code, or need a different status code, please use 'redirectWith'.
redirect :: MonadHandler m => Text -> m a
redirect url = do
    req <- getRequest
    let status = if httpVersion req == H.http10
                   then H.found302 else H.seeOther303
    redirectWith status url

-- | Redirect to the given route.
-- HTTP status code 303 for HTTP 1.1 clients and 302 for HTTP 1.0
-- (see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.4 HTTP/1.1 documentation>).
-- This is the appropriate choice for a get-following-post technique, which should be the usual use case.
--
-- If you want direct control of the final status code, or need a different status code, please use 'redirectRouteWith'.
redirectRoute :: MonadHandler m => Route (HandlerSite m) -> [(Text,Text)] -> m a
redirectRoute route params = do
    render <- getUrlRender
    redirect (render route params)

redirectWith :: MonadHandler m => H.Status -> Text -> m a
redirectWith status url =
    liftIO $ throwIO (HCRedirect status url)

redirectRouteWith :: MonadHandler m => H.Status -> Route (HandlerSite m) -> [(Text,Text)] -> m a
redirectRouteWith status route params = do
    render <- getUrlRender
    redirectWith status (render route params)


--------------------------------------------------------------
-- Misc

newIdent :: MonadHandler m => m Text
newIdent = do
    st <- get
    let i = hsId st
    put st{ hsId = i + 1 }
    pure $ T.pack $ 'i' : show i

--------------------------------------------------------------
-- Low level

-- | Send a Response. Please note: this function is rarely necessary, and will disregard any changes
-- to response headers and session that you have already specified.
-- This function short-circuits. It should be considered only for very specific needs. If you are not sure if you need it, you don't.
sendServerResponse :: MonadHandler m => Response -> m a
sendServerResponse response =
    liftIO $ throwIO (HCResponse response)


--------------------------------------------------------------
-- Subsites

getSubSite :: MonadHandler m => m (SubHandlerSite m)
getSubSite = liftSubHandler $ SubHandlerFor $ pure . rheSubSite . handlerEnv

getsSubSite :: MonadHandler m => (SubHandlerSite m -> a) -> m a
getsSubSite f = f <$> getSubSite

getRouteToMaster :: MonadHandler m => m (Route (SubHandlerSite m) -> Route (HandlerSite m))
getRouteToMaster = liftSubHandler $ SubHandlerFor $ pure . rheRouteToMaster . handlerEnv

getSubCurrentRoute :: MonadHandler m => m (Maybe (Route (SubHandlerSite m)))
getSubCurrentRoute = liftSubHandler $ SubHandlerFor $ pure . rheRoute . handlerEnv

