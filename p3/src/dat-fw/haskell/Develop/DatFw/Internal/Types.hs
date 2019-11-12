
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

--
-- @author Jordi Forga
--
module Develop.DatFw.Internal.Types
where
import           Develop.DatFw.Content

import           Network.Wai
import           Network.HTTP.Types as H

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Typeable
import qualified Data.Map as M
import           Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.Text.Read
import qualified Data.ByteString as B
import           Data.Maybe
import           Data.IORef
import           Text.Read

---------------------------------------------------------------------------------
-- Foundation types

data family Route a :: *

class RenderRoute a where
    renderRoute :: Route a -> ([Text],[(Text,Text)])

class RenderRoute a => ParseRoute a where
    parseRoute :: ([Text],[(Text,Text)]) -> Maybe (Route a)

type UrlRender route = route -> [(Text, Text)] -> Text


type SessionMap = M.Map Text B.ByteString

type SaveSession = SessionMap -> IO [Header]

-- ^ Return the session data and a function to save the session
type SessionBackend = Request -> IO (SessionMap, SaveSession)


-- | Authorization result type.
data AuthzResult = AuthenticationRequired | Authorized | Unauthorized Text

-----------------------------------------------------------------

class PathPiece a where
    toPathPiece :: a -> Text
    fromPathPiece :: Text -> Maybe a

-- | See the documentation for 'readFromPathPiece'.
showToPathPiece :: Show a => a -> Text
showToPathPiece = T.pack . show

-- | A function for helping generate free 'PathPiece'
--   instances for enumeration data types 
--   that have derived 'Read' and 'Show' instances.
--   Intended to be used like this:
--
--   > data MyData = Foo | Bar | Baz
--   >   deriving (Read,Show)
--   > instance PathPiece MyData where
--   >   fromPathPiece = readFromPathPiece
--   >   toPathPiece = showToPathPiece
--
readFromPathPiece :: Read a => Text -> Maybe a
readFromPathPiece = readMaybe . T.unpack

parseIntegral :: (Integral a, Bounded a, Ord a) => T.Text -> Maybe a
parseIntegral s = n
    where
    n = case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (i, "") | i <= top && i >= bot -> Just (fromInteger i)
        _ -> Nothing
    Just witness = n
    top = toInteger (maxBound `asTypeOf` witness)
    bot = toInteger (minBound `asTypeOf` witness)

instance PathPiece Text where
    toPathPiece = id
    fromPathPiece = Just

instance PathPiece String where
    toPathPiece = T.pack
    fromPathPiece = Just . T.unpack

instance PathPiece Int where
    toPathPiece = T.pack . show
    fromPathPiece = parseIntegral

instance PathPiece Bool where
    toPathPiece False = "False"
    toPathPiece True = "True"
    fromPathPiece "False" = Just False
    fromPathPiece "True"  = Just True
    fromPathPiece _       = Nothing

instance (PathPiece a) => PathPiece (Maybe a) where
    fromPathPiece s = case T.stripPrefix "Just " s of
        Just r -> fmap Just $ fromPathPiece r
        _ -> case s of
            "Nothing" -> Just Nothing
            _ -> Nothing
    toPathPiece m = case m of
        Just s -> "Just " <> toPathPiece s
        _ -> "Nothing"


class PathMultiPiece a where
    toPathMultiPiece :: a -> [Text]
    fromPathMultiPiece :: [Text] -> Maybe a

instance PathPiece a => PathMultiPiece [a] where
    toPathMultiPiece = fmap toPathPiece
    fromPathMultiPiece = mapM fromPathPiece

instance ParseRoute a => PathMultiPiece (Route a) where
    toPathMultiPiece = fst . renderRoute
    fromPathMultiPiece r = parseRoute (r, [])


---------------------------------------------------------------------------------
-- Dispatch types

data DispatchEnv a = DispatchEnv
        { envSite :: a                                  -- the application
        , envSessionBackend :: Maybe SessionBackend     -- sessionBackend
        -- environment provided by the server ...
        }

data SubDispatchEnv subsite site = SubDispatchEnv
        { subeMasterDispatcher :: HandlerFor site TypedContent -> DispatchEnv site -> Maybe (Route site) -> Application
            -- It's the 'dispatchHandler' function instantiated with types 'site' and 'TypedContent'.
        , subeGetSubsite :: site -> subsite
        , subeRouteToMaster :: Route subsite -> Route site
        , subeMasterEnv :: DispatchEnv site
        }


---------------------------------------------------------------------------------
-- Handler types

data HandlerFor site a = HandlerFor { unHandlerFor :: HandlerData site site -> IO a }

type family HandlerSite (m :: * -> *) :: *
type family SubHandlerSite (m :: * -> *) :: *

class MonadIO m => MonadHandler m where
    liftHandler :: HandlerFor (HandlerSite m) a -> m a
    liftSubHandler :: SubHandlerFor (SubHandlerSite m) (HandlerSite m) a -> m a

instance Functor (HandlerFor site) where
    fmap f m = HandlerFor $ \ env -> do
        x <- unHandlerFor m env
        pure (f x)

instance Applicative (HandlerFor site) where
    pure x = HandlerFor $ \ _ -> pure x
    mf <*> mx = HandlerFor $ \ env -> do
        f <- unHandlerFor mf env
        x <- unHandlerFor mx env
        pure (f x)

instance Monad (HandlerFor site) where
    m >>= f = HandlerFor $ \ env -> do
        x <- unHandlerFor m env
        unHandlerFor (f x) env
    fail s = HandlerFor $ \ _ -> fail s

instance MonadIO (HandlerFor site) where
    liftIO io = HandlerFor $ \ _ -> io

type instance HandlerSite (HandlerFor site) = site
type instance SubHandlerSite (HandlerFor site) = site

instance MonadHandler (HandlerFor site) where
    liftHandler = id
    liftSubHandler (SubHandlerFor h) = HandlerFor h


data HandlerData sub site = HandlerData
        { handlerEnv :: RunHandlerEnv sub site  -- handler's environment
        , handlerReq :: Request                 -- request
        , handlerStR :: IORef HandlerState      -- state reference
        }

data RunHandlerEnv sub site = RunHandlerEnv
        { rheSite :: site                                       -- master site
        , rheSubSite :: sub                                     -- subsite
        , rheUrlRender :: UrlRender (Route site)                -- urlRender
        , rheRoute :: Maybe (Route sub)                         -- subsite's route
        , rheRouteToMaster :: Route sub -> Route site           -- subsite's route to master site's route
        , rheDefStatus :: Status                                -- defaultStatus
        , rheOnError :: ResponseError -> Request -> SessionMap -> IO HandlerResp -- onError
        }

data HandlerState = HandlerState
        { hsSession :: SessionMap       -- session
        , hsPostQ :: Maybe Query        -- POST parameters
        , hsId :: Int
        }

-- | Responses to indicate some form of an error occurred.
data ResponseError =
      NotFound
    | BadMethod H.Method
    | InvalidArgs [Text]
    | InternalError Text
    | NotAuthenticated
    | PermissionDenied Text

instance Show ResponseError where
    showsPrec _ NotFound  = showString "NotFound"
    showsPrec p (BadMethod x)        = showParen (p > 10) (showString "BadMethod " . showsPrec 11 x)
    showsPrec p (InvalidArgs x)      = showParen (p > 10) (showString "InvalidArgs " . showsPrec 11 x)
    showsPrec p (InternalError x)    = showParen (p > 10) (showString "InternalError " . showsPrec 11 x)
    showsPrec _ NotAuthenticated     = showString "NotAuthenticated"
    showsPrec p (PermissionDenied x) = showParen (p > 10) (showString "PermissionDenied " . showsPrec 11 x)


data HandlerResp =
    Normal
        Status                  -- status;
        [Header]                -- headers;
        ContentType             -- contentType;
        Content                 -- content;
        SessionMap              -- session;
    | Direct
        Response                --  response;

data HandlerContentException =
    HCError
        ResponseError   -- error;
    | HCRedirect
        Status          -- status;
        Text            -- url;
    | HCResponse
        Response        -- response;
    deriving (Typeable)

instance Show HandlerContentException where
    show (HCError e) = "HCError " <> show e
    show (HCRedirect s t) = "HCRedirect " <> show (s, t)
    show (HCResponse _) = "HCResponse"

instance Exception HandlerContentException


---------------------------------------------------------------------------------
-- Widget construction

-- | A function generating an 'Html' given a URL-rendering function.

data WidgetState route = WidgetState
        { wsTitle :: Maybe Html
        , wsHead :: UrlRender route -> Html
        , wsBody :: UrlRender route -> Html
        }

instance Monoid (WidgetState route) where
    mempty =
        WidgetState { wsTitle = Nothing, wsHead = mempty, wsBody = mempty }
    mappend x y =
        WidgetState
            { wsTitle = if isJust (wsTitle y) then wsTitle y else wsTitle x
            , wsHead = wsHead x `mappend` wsHead y
            , wsBody = wsBody x `mappend` wsBody y
            }

data WidgetData site = WidgetData
        { wdStateRef :: IORef (WidgetState (Route site))
        , wdHData :: HandlerData site site
        }

data WidgetFor site a = WidgetFor { unWidgetFor :: WidgetData site -> IO a }

type Widget site = WidgetFor site ()

instance Functor (WidgetFor site) where
    fmap f m = WidgetFor $ \ env -> do
        x <- unWidgetFor m env
        pure (f x)

instance Applicative (WidgetFor site) where
    pure x = WidgetFor $ \ _ -> pure x
    mf <*> mx = WidgetFor $ \ env -> do
        f <- unWidgetFor mf env
        x <- unWidgetFor mx env
        pure (f x)

instance Monad (WidgetFor site) where
    m >>= f = WidgetFor $ \ env -> do
        x <- unWidgetFor m env
        unWidgetFor (f x) env
    fail s = WidgetFor $ \ _ -> fail s

instance MonadIO (WidgetFor site) where
    liftIO io = WidgetFor $ \ _ -> io

instance Monoid (Widget site) where
    mempty =
        pure ()
    mappend x y =
        x *> y

type instance HandlerSite (WidgetFor site) = site
type instance SubHandlerSite (WidgetFor site) = site

instance MonadHandler (WidgetFor site) where
    liftHandler (HandlerFor h) = WidgetFor $ h . wdHData
    liftSubHandler (SubHandlerFor h) = WidgetFor $ h . wdHData


--------------------------------------------------------------
-- Subsites

data SubHandlerFor subsite site a = SubHandlerFor { unSubHandlerFor :: HandlerData subsite site -> IO a }

instance Functor (SubHandlerFor subsite site) where
    fmap f m = SubHandlerFor $ \ env -> do
        x <- unSubHandlerFor m env
        pure (f x)

instance Applicative (SubHandlerFor subsite site) where
    pure x = SubHandlerFor $ \ _ -> pure x
    mf <*> mx = SubHandlerFor $ \ env -> do
        f <- unSubHandlerFor mf env
        x <- unSubHandlerFor mx env
        pure (f x)

instance Monad (SubHandlerFor subsite site) where
    m >>= f = SubHandlerFor $ \ env -> do
        x <- unSubHandlerFor m env
        unSubHandlerFor (f x) env
    fail s = SubHandlerFor $ \ _ -> fail s

instance MonadIO (SubHandlerFor subsite site) where
    liftIO io = SubHandlerFor $ \ _ -> io

type instance HandlerSite (SubHandlerFor subsite site) = site
type instance SubHandlerSite (SubHandlerFor subsite site) = subsite

instance MonadHandler (SubHandlerFor subsite site) where
    liftHandler (HandlerFor f) = SubHandlerFor $ \ shd ->
        let srhe = handlerEnv shd
            rhe = srhe{ rheRoute = fmap (rheRouteToMaster srhe) (rheRoute srhe)
                      , rheRouteToMaster = id
                      , rheSubSite = rheSite srhe
                      }
        in f shd{ handlerEnv = rhe }
    liftSubHandler = id

