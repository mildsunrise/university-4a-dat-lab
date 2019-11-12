
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Develop.DatFw.Internal.Dispatch
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Internal.Classes
import Develop.DatFw.Handler
import Develop.DatFw.Content

import           Network.Wai
import qualified Network.HTTP.Types as H

import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import           Data.IORef
import           Control.Exception


-- Similar to Yesod's yesodRunner:: (ToTypedContent res, Yesod site) => HandlerT site IO res -> YesodRunnerEnv site -> Maybe (Route site) -> Application
dispatchHandler :: (WebApp a, ToTypedContent r) => HandlerFor a r -> DispatchEnv a -> Maybe (Route a) -> Application
dispatchHandler handler env croute request respond = do
    let site = envSite env
        dontSave smap = pure []
    (sessionMap, saveSession) <- case envSessionBackend env of
        Nothing -> pure (M.empty, dontSave)
        Just sBackend -> sBackend request
    let ar = appRoot site request
        urlRender route params = T.decodeUtf8 $ BL.toStrict $ B.toLazyByteString $
            case urlRenderOverride site route params of
                Nothing -> let (segs, params2) = renderRoute route
                           in joinPath site ar segs (params2 <> params)
                Just override -> override
    let onError :: ResponseError -> Request -> SessionMap -> IO HandlerResp
        onError err =
            let safe_rhenv = rhenv{ rheDefStatus = statusForError err, rheOnError = safeErrorHandler }
            in runHandler safe_rhenv (errorHandler err)
        rhenv = RunHandlerEnv
                { rheSite = site
                , rheSubSite = site
                , rheUrlRender = urlRender
                , rheRoute = croute
                , rheRouteToMaster = id
                , rheDefStatus = H.ok200
                , rheOnError = onError
                }

    hresp <- runHandler rhenv (siteMiddleware handler) request sessionMap
    resp <- makeResponse hresp saveSession
    respond resp
  where
        statusForError NotFound             = H.notFound404
        statusForError (BadMethod _)        = H.methodNotAllowed405
        statusForError (InvalidArgs _)      = H.badRequest400
        statusForError NotAuthenticated     = H.unauthorized401
        statusForError (PermissionDenied _) = H.forbidden403
        statusForError (InternalError _)    = H.internalServerError500
        safeErrorHandler :: ResponseError -> Request -> SessionMap -> IO HandlerResp
        safeErrorHandler err request session = do
            let c = toContent ("Error on error handler:" <> show err)
            pure (Normal H.internalServerError500 [] typePlain c session)

makeResponse :: HandlerResp -> SaveSession -> IO Response
makeResponse (Normal status headers contentType content session) saveSession = do
    sessionHeaders <- saveSession session
    let rheaders = ("Content-Type", contentType) : sessionHeaders ++ headers
    case content of
        ContentBuilder builder mlen -> do
            let finalHeaders = case mlen of
                    Just len -> ("Content-Length", B8.pack $ show len) : rheaders
                    Nothing -> rheaders
            pure $ responseBuilder status finalHeaders builder
        ---ContentSource ... -> responseStream status rheaders ((Content.Source) this.content).writeTo);
makeResponse (Direct resp) saveSession =
    pure resp

--------------- Handler dispatching  ---------------------

runHandler :: ToTypedContent r => RunHandlerEnv site site -> HandlerFor site r -> Request -> SessionMap -> IO HandlerResp
runHandler rhenv handler request session = do
    stateRef <- newIORef (HandlerState session Nothing 1)
    let hdata = HandlerData rhenv request stateRef
        toHCException :: SomeException -> HandlerContentException
        toHCException e = case fromException e of
                        Just hce -> hce
                        Nothing -> HCError (InternalError $ T.pack $ show e)
    result <- catch
        ((Right . toTypedContent) <$> unHandlerFor handler hdata)
        (\ e -> pure (Left (toHCException e)))
    finalSession <- hsSession <$> readIORef stateRef
    case result of
        Right (TypedContent t c) ->             -- Normal response
            pure (Normal (rheDefStatus rhenv) [] t c finalSession)
        Left (HCError err) ->                   -- Error
            rheOnError rhenv err request finalSession
        Left (HCRedirect status url) -> do      -- Redirect
            let disable_caching hs =
                    ("Cache-Control", "no-cache, must-revalidate")
                    : ("Expires", "Thu, 01 Jan 1970 05:05:05 GMT") : hs
            let headers = (if status == H.movedPermanently301 then id
                           else disable_caching) $
                              [ ("Location", T.encodeUtf8 url) ]
            pure (Normal status headers typePlain emptyContent finalSession)
        Left (HCResponse resp) ->               -- Low level reponse
            pure (Direct resp)


--------------- Subsites  ---------------------

class SubDispatch subsite site where
    subDispatch :: SubDispatchEnv subsite site -> Application

-- Similar to Yesod's subHelper:: Monad m => HandlerT subsite (HandlerT site m) TypedContent -> YesodSubRunnerEnv subsite site -> Maybe (Route subsite) -> Application
subDispatchHandler:: ToTypedContent a => SubHandlerFor subsite site a -> SubDispatchEnv subsite site -> Maybe (Route subsite) -> Application
subDispatchHandler (SubHandlerFor f) env croute request = do
    let handler = HandlerFor $ \ hd -> do
            let site = envSite $ subeMasterEnv env
                rhe = (handlerEnv hd){ rheSubSite = subeGetSubsite env site, rheRoute = croute, rheRouteToMaster = subeRouteToMaster env }
            toTypedContent <$> f hd{ handlerEnv = rhe }
    subeMasterDispatcher env handler (subeMasterEnv env) (subeRouteToMaster env <$> croute) request

