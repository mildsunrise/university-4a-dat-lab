
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Develop.DatFw.Internal.Routing
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Internal.Dispatch
import Develop.DatFw.Internal.Classes
import Develop.DatFw.Handler
import Develop.DatFw.Content

import Network.Wai
import Network.HTTP.Types

import           Data.List
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Control.Monad.State
import           Control.Applicative


type RoutesM = StateT [Text] Maybe

type RoutesEnv f env r = (f TypedContent -> env -> Maybe r -> Application, f TypedContent)

routing :: WebApp site
         => (RoutesEnv (HandlerFor site) (DispatchEnv site) (Route site) -> RoutesM (DispatchEnv site -> Application))
         -> DispatchEnv site
         -> Application
routing routesM =
    let routesM' = routesM (dispatchHandler, badMethod)
    in \ env req ->
        case runStateT routesM' (pathInfo req) of
            Nothing -> dispatchHandler (notFound :: HandlerFor site ()) env Nothing req
            Just (f, _) -> f env req

subrouting :: (RoutesEnv (SubHandlerFor subsite site) (SubDispatchEnv subsite site) (Route subsite)
                -> RoutesM (SubDispatchEnv subsite site -> Application))
            -> SubDispatchEnv subsite site
            -> Application
subrouting routesM =
    let routesM' = routesM (subDispatchHandler, liftHandler badMethod)
    in \ env req ->
        case runStateT routesM' (pathInfo req) of
            Nothing -> subDispatchHandler (liftHandler notFound :: SubHandlerFor subsite site ()) env Nothing req
            Just (f, _) -> f env req

(<||>) :: (conf -> RoutesM a)
         -> (conf -> RoutesM a)
         -> conf -> RoutesM a
(x <||> y) c = x c <|> y c

route :: RoutesM (r' -> Route site, h' -> f TypedContent)
         -> r' -> [(Method, h')]
         -> RoutesEnv f env (Route site)
         -> RoutesM (env -> Application)
route rm r' meths (dispatcher, bm) =
    let mmap = M.fromList meths
        methmbf m = case meths of
                    [("",h')] -> Just h'
                    _ -> M.lookup m mmap        
    in do
        (pathrf, pathhf) <- rm
        [] <- get       -- Guard: no remaining path pieces
        let r = pathrf r'
        pure $ \ env req ->
            case methmbf (requestMethod req) of
                Just h' -> dispatcher (pathhf h') env (Just r) req
                Nothing -> dispatcher bm env (Just r) req

routeSub :: SubDispatch subsite site
         => RoutesM (r' -> Route subsite -> Route site, sf' -> site -> subsite)
         -> r' -> sf'
         -> RoutesEnv (HandlerFor site) (DispatchEnv site) (Route site)
         -> RoutesM (DispatchEnv site -> Application)
routeSub rm r' sf' (dispatcher, bm) = do
    (pathrf, pathsf) <- rm
    ps <- get
    let r = pathrf r'
        sf = pathsf sf'
    pure $ \ env req -> do
            let subenv = SubDispatchEnv
                    { subeMasterDispatcher = dispatcher
                    , subeGetSubsite = sf
                    , subeRouteToMaster = r
                    , subeMasterEnv = env
                    }
            subDispatch subenv req{ pathInfo = ps }

(<&&>) :: RoutesM (r'' -> r', h'' -> h')
        -> RoutesM (r'  -> r , h'  -> h )
        -> RoutesM (r'' -> r , h'' -> h )
(<&&>) =
    liftA2 $ \ (rf1, mf1) (rf2, mf2) -> (rf2 . rf1, mf2 . mf1)

onStatic :: [Text] -> RoutesM (r -> r, h -> h)
onStatic pre = do
    ps <- get
    case stripPrefix pre ps of
        Nothing -> lift Nothing
        Just ps' -> put ps' >> pure (id, id)

onDynamic :: PathPiece a => RoutesM ((a -> r) -> r, (a -> h) -> h)
onDynamic = do
    p:ps <- get
    x <- lift $ fromPathPiece p
    put ps
    pure (\ f -> f x, \ f -> f x)

onDynamicMulti :: PathMultiPiece a => RoutesM ((a -> r) -> r, (a -> h) -> h)
onDynamicMulti = do
    ps <- get
    x <- lift $ fromPathMultiPiece ps
    put []
    pure (\ f -> f x, \ f -> f x)

onMethod :: (Functor f, ToTypedContent o) => Method -> f o -> (Method, f TypedContent)
onMethod m h = (m, fmap toTypedContent h)

onMethod1 :: (Functor f, ToTypedContent o) => Method -> (a -> f o) -> (Method, a -> f TypedContent)
onMethod1 m h = (m, fmap (fmap toTypedContent) h)

onMethod2 :: (Functor f, ToTypedContent o) => Method -> (a -> b -> f o) -> (Method, a -> b -> f TypedContent)
onMethod2 m h = (m, fmap (fmap (fmap toTypedContent)) h)

onMethod3 :: (Functor f, ToTypedContent o) => Method -> (a -> b -> c -> f o) -> (Method, a -> b -> c -> f TypedContent)
onMethod3 m h = (m, fmap (fmap (fmap (fmap toTypedContent))) h)

onMethod4 :: (Functor f, ToTypedContent o) => Method -> (a -> b -> c -> d -> f o) -> (Method, a -> b -> c -> d -> f TypedContent)
onMethod4 m h = (m, fmap (fmap (fmap (fmap (fmap toTypedContent)))) h)

onMethod5 :: (Functor f, ToTypedContent o) => Method -> (a -> b -> c -> d -> e -> f o) -> (Method, a -> b -> c -> d -> e -> f TypedContent)
onMethod5 m h = (m, fmap (fmap (fmap (fmap (fmap (fmap toTypedContent))))) h)

onAnyMethod :: (Functor f, ToTypedContent o) => f o -> [(Method, f TypedContent)]
onAnyMethod h =
    [("", fmap toTypedContent h)]

onAnyMethod1 :: (Functor f, ToTypedContent o) => (a -> f o) -> [(Method, a -> f TypedContent)]
onAnyMethod1 h =
    [("", fmap (fmap toTypedContent) h)]

onAnyMethod2 :: (Functor f, ToTypedContent o) => (a -> b -> f o) -> [(Method, a -> b -> f TypedContent)]
onAnyMethod2 h =
    [("", fmap (fmap (fmap toTypedContent)) h)]

onAnyMethod3 :: (Functor f, ToTypedContent o) => (a -> b -> c -> f o) -> [(Method, a -> b -> c -> f TypedContent)]
onAnyMethod3 h =
    [("", fmap (fmap (fmap (fmap toTypedContent))) h)]

onAnyMethod4 :: (Functor f, ToTypedContent o) => (a -> b -> c -> d -> f o) -> [(Method, a -> b -> c -> d -> f TypedContent)]
onAnyMethod4 h =
    [("", fmap (fmap (fmap (fmap (fmap toTypedContent)))) h)]

onAnyMethod5 :: (Functor f, ToTypedContent o) => (a -> b -> c -> d -> e -> f o) -> [(Method, a -> b -> c -> d -> e -> f TypedContent)]
onAnyMethod5 h =
    [("", fmap (fmap (fmap (fmap (fmap (fmap toTypedContent))))) h)]

