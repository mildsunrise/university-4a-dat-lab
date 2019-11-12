
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
-- @author Jordi Forga
--
module Develop.DatFw.Dispatch
    ( Dispatch(..), DispatchEnv(..), dispatchHandler
    , toApp, toAppWithEnv
    -- * Routing combinators
    , routing, route, routeSub, (<||>)
    , onStatic, onDynamic, onDynamicMulti, (<&&>)
    , onMethod, onMethod1, onMethod2, onMethod3, onMethod4, onMethod5
    , onAnyMethod, onAnyMethod1, onAnyMethod2, onAnyMethod3, onAnyMethod4, onAnyMethod5
    -- * Subsites
    , SubDispatch(..), SubDispatchEnv(..), subDispatchHandler
    , subrouting
    )
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Internal.Dispatch
import Develop.DatFw.Internal.Routing
import Develop.DatFw.Internal.Classes

import           Network.Wai
import qualified Network.HTTP.Types as H

import           Data.Monoid
import           Data.Text(Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

class WebApp a => Dispatch a where
    dispatch :: DispatchEnv a -> Application

--------------- Convert to WAI (Http Application)  ---------------------

-- | Convert the given argument into a WEB application, executable with any compatible server.
toApp :: Dispatch a => a -> IO Application
toApp site = do
    sessionBackend <- makeSessionBackend site
    let env = DispatchEnv site sessionBackend
    pure $ toAppWithEnv env

-- | Pure low level function to construct WEB application.
--
-- Useful when you need not standard way to run your app, or want to embed it
-- inside another app.
toAppWithEnv :: Dispatch a => DispatchEnv a -> Application
toAppWithEnv env@(DispatchEnv site _) request respond =
    case cleanPath site (pathInfo request) of
        Left path -> sendRedirect site path request respond
        Right path -> dispatch env (request { pathInfo = path }) respond
  where
        sendRedirect :: WebApp a => a -> [Text] -> Application
        sendRedirect app segments req respond = do
            let ar = appRoot app req
                -- Ensure that non-GET requests get redirected correctly.
                status = if requestMethod req == H.methodGet
                         then H.movedPermanently301 else H.temporaryRedirect307
                rqs = rawQueryString req
                dest = joinPath app ar segments []
                        <> (if B.null rqs then mempty else B.byteString rqs)
            respond (responseBuilder status [("Location", BL.toStrict (B.toLazyByteString dest))] mempty)


{---
--------------- Session backend  ---------------------

private static final Text SESSION_PREFIX = "dat.web.fw.servlet.";

private static SessionBackend servletSessionBackend = SessionBackend(
    new Comm1<Request,Pair<SessionMap,Comm2<SessionMap,Response,Response>>>(){
    public Pair<SessionMap,Comm2<SessionMap,Response,Response>> run(Request request_)
    {
        final dat.web.http.ServletM.ServletRequest request = (dat.web.http.ServletM.ServletRequest) request_;
        SessionMap session = new SessionMap();
        javax.servlet.http.HttpSession httpSes = request.servReq.getSession(false);
        if (httpSes != null) {
           for (java.util.Enumeration<Text> e = httpSes.getAttributeNames(); e.hasMoreElements();) {
               Text name = e.nextElement();
               if (name.startsWith(SESSION_PREFIX)) {
                   session.put(name.substring(SESSION_PREFIX.length()), (Text) httpSes.getAttribute(name));
                   httpSes.removeAttribute(name);
               }
           }
       }
       Comm2<SessionMap,Response,Response> saveSession = new Comm2<SessionMap,Response,Response>(){
           public Response run(SessionMap finalSession, Response resp) {
               if (finalSession.size() == 0) {
                   javax.servlet.http.HttpSession servSess = request.servReq.getSession(false);
                   if (servSess != null) servSess.invalidate();
               } else {
                   javax.servlet.http.HttpSession servSess = request.servReq.getSession(true);
                   for (java.util.Map.Entry<Text,Text> e : finalSession.entrySet()) {
                       servSess.setAttribute(SESSION_PREFIX + e.getKey(), e.getValue());
                   }
               }
               return resp;
           }
       };
       return Pair(session, saveSession);
    }
    });
---}


