
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
--
-- @author Jordi Forga
--
module Develop.DatFw.Internal.Classes
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Handler
import Develop.DatFw.Content
import Develop.DatFw.Widget
import Develop.DatFw.Template

import           Network.Wai
import           Network.HTTP.Types as H
import           Web.Cookie
import           Network.Wai.Middleware.Approot(getApproot)

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import           Data.Monoid
import qualified Data.Map as M
import           Data.Time
import           System.Environment

---------------------------------------------------------------------------------
-- Foundation types

-- | Define settings for a WEB applications. All methods have intelligent defaults, and therefore no implementation is required.
class RenderRoute a => WebApp a where
    -- | An absolute URL to the root of the application. Do not include trailing slash.
    -- Default value: relative to request's context path.
    --
    -- If this is not true, you should override with a different implementation.
    appRoot :: a -> Request -> Text

    -- | A function used to clean up path segments.
    -- It returns Right with a clean path or Left with a new set of pieces the user should be redirected to.
    -- The default implementation enforces:
    --
    -- * No double slashes
    --
    -- * There is no trailing slash.
    --
    cleanPath :: a -> [Text] -> Either [Text] [Text]
    cleanPath site segs =
        let filtered = filterNulls segs
        in if fst filtered then Left (snd filtered)
               else Right (snd filtered)
      where
        filterNulls [] = (False, [])
        filterNulls segs =
            let tail2 = filterNulls (tail segs)
            in if T.length (head segs) == 0 then (True, snd tail2)
               else (fst tail2, head segs : snd tail2)

    -- | Builds an absolute URL by concatenating the application root with the pieces of a path and a query string, if any.
    -- Note that the pieces of the path have been previously cleaned up by 'cleanPath'.
    joinPath :: a -> Text -> [Text] -> [(Text, Text)] -> B.Builder
    joinPath site root segs params =
        T.encodeUtf8Builder root <> H.encodePathSegments (if null segs then [""] else segs)
          <> H.renderQueryBuilder True (goParam <$> params)
      where
        goParam :: (Text, Text) -> H.QueryItem
        goParam (name,value) = (T.encodeUtf8 name,
                                if T.length value == 0 then Nothing else Just (T.encodeUtf8 value))

    -- | Override the rendering function for a particular URL. One use case for
    -- this is to offload static hosting to a different domain name to avoid
    -- sending cookies.
    urlRenderOverride :: a -> Route a -> [(Text, Text)] -> Maybe B.Builder
    urlRenderOverride site route params = Nothing

    -- | Determines whether the current request is a write request.
    -- By default, this assumes you are following RESTful principles, and determines this from request method.
    -- In particular, all except the following request methods are considered write: GET HEAD OPTIONS TRACE.
    --
    -- This function is used to determine if a request is authorized; see 'isAuthorized'.
    isWriteRequest :: Route a -> HandlerFor a Bool
    isWriteRequest route = do
        let safe = [methodGet, methodHead, methodOptions, methodTrace]
        method <- requestMethod <$> getRequest
        pure $ notElem method safe

    -- | Determine if a request is authorized or not.
    --
    -- Return 'Authorized' if the request is authorized, 'Unauthorized' a message if unauthorized.
    -- If authentication is required, return 'AuthenticationRequired'.
    isAuthorized :: Route a -> Bool -> HandlerFor a AuthzResult
    isAuthorized route isWrite =
        pure Authorized

    -- | The default route for authentication.
    --
    -- Used in particular by 'authorizationCheck', but library users can do whatever they want with it.
    authRoute :: a -> Maybe (Route a)
    authRoute site =
        Nothing


    -- | Output error response pages.
    --
    -- Default value: 'defaultErrorHandler'.
    errorHandler :: ResponseError -> HandlerFor a TypedContent
    errorHandler = defaultErrorHandler

    -- | Create a session backend. Returning Nothing disables sessions.
    -- If you'd like to change the way that the session cookies are created, take a look at 'clientSessionBackend'.
    --
    -- Default: Uses clientsession with a 1 hour timeout.
    makeSessionBackend :: a -> IO (Maybe SessionBackend)
    makeSessionBackend site =
        Just <$> defaultClientSessionBackend 60

    -- | Applies some form of layout to the contents of a page.
    defaultLayout :: WidgetFor a () -> HandlerFor a Html
    defaultLayout wdgt = do
        page <- widgetToPageContent wdgt
        mbmsg <- getMessage
        applyUrlRenderTo [htmlTempl|
                <!DOCTYPE html>
                <html><head>
                  <title>#{pcTitle page}</title>
                  ^{pcHead page}
                </head><body>
                $maybe{ msg <- mbmsg }<p class="message error">#{msg}</p>$end
                ^{pcBody page}
                </body></html>
            |]

    -- | A middleware, which will wrap every handler function. This allows you to run code before and after a normal handler.
    --
    -- Default: performs authorization checks (via 'authorizationCheck').
    siteMiddleware:: HandlerFor a res -> HandlerFor a res
    siteMiddleware handler = do
        authorizationCheck
        handler


-- ------------------------------ Defaults ------------------------------

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: ResponseError -> HandlerFor a TypedContent
---defaultErrorHandler :: WebApp a => ResponseError -> HandlerFor a TypedContent
defaultErrorHandler NotFound =
    pure $ toTypedContent ("Not found" :: Text)
defaultErrorHandler (BadMethod meth) =
    pure $ toTypedContent ("Method " <> show meth <> " not supported")
defaultErrorHandler (InvalidArgs args) =
    pure $ toTypedContent ("Invalid Arguments:" <> show args)
defaultErrorHandler NotAuthenticated =
    pure $ toTypedContent ("Not authenticated" :: Text)
defaultErrorHandler (PermissionDenied msg) =
    pure $ toTypedContent ("Permission denied: " <> msg)
defaultErrorHandler (InternalError msg) =
    pure $ toTypedContent ("Internal Server Error: " <> msg)

defaultClientSessionBackend :: Int -- ^ max age in minutes
                            -> IO SessionBackend
defaultClientSessionBackend minutes =
  pure $ clientSessionBackend "_SESSION_" (60 * minutes)

clientSessionBackend :: B.ByteString -- ^ session cookie key
                     -> Int          -- ^ max age in seconds
                     -> SessionBackend
clientSessionBackend sessionName maxAge request = do
        -- NOTE: Avoids cookies from other apps in the same server (must be revised)
        env <- getEnvironment
        let path = maybe "/" B8.pack $ lookup "SCRIPT_NAME" env
        pure (loadSession, saveSession path)
    where
        loadSession = M.unions $ do -- List monad
            val <- [ cv | (hk, hv) <- requestHeaders request, hk == "Cookie",
                          (ck, cv) <- parseCookies hv, ck == sessionName ]
            maybe [] pure $ decodeCookie val
        saveSession path sess = do
            let cookieValue = encodeCookie sess
                setCookie = defaultSetCookie
                    { setCookieName = sessionName
                    , setCookieValue = cookieValue
                    , setCookiePath = Just path -- The application path from getenv("SCRIPT_NAME")
                    , setCookieMaxAge = Just $ secondsToDiffTime $ fromIntegral maxAge
                    }
            pure [("Set-Cookie", BL.toStrict $ B.toLazyByteString $ renderSetCookie setCookie)]
        decodeCookie :: B.ByteString -> Maybe SessionMap
        decodeCookie cookieValue =
            let pairs = do -- List monad
                        (k, Just v) <- parseQuery $ urlDecode False cookieValue
                        pure (T.decodeUtf8 k, v)
            in Just $ M.fromList pairs
        encodeCookie :: SessionMap -> B.ByteString
        encodeCookie sess =
            let pairs = M.toList sess
                f (n,v) = (T.encodeUtf8 n, Just v)
            in urlEncode False $ H.renderQuery False (f <$> pairs)

-- | Check if a given request is authorized via 'isWriteRequest' and 'isAuthorized'.
-- Redirects to 'authRoute' if authentication is required.
authorizationCheck :: WebApp a => HandlerFor a ()
authorizationCheck = do
    mburl <- getCurrentRoute
    case mburl of
        Nothing -> pure ()
        Just url -> do
            isWrite <- isWriteRequest url
            ar <- isAuthorized url isWrite
            case ar of
                Authorized -> pure ()
                AuthenticationRequired -> do
                    master <- getSite
                    case authRoute master of
                        Nothing -> notAuthenticated
                        Just authurl -> do
                            setUltDestCurrent
                            redirectRoute authurl []
                Unauthorized detail -> permissionDenied detail


