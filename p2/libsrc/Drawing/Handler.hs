{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedStrings #-}

#if defined(WAI_CGI) || defined(WAI_WARP)
#define WAI
#else
#error "Which handler ?"
#endif

module Drawing.Handler
where
import Drawing.Internal
import Drawing.Render

import Network.Wai
import Network.HTTP.Types
import Web.Cookie
#if defined(WAI_CGI)
import Network.Wai.Handler.CGI
---import Network.Wai.Handler.Launch
#elif defined(WAI_WARP)
import Network.Wai.Handler.Warp
#endif

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder as Bd

import Data.Monoid
import Data.Functor
import Data.Foldable
import Control.Applicative
import Data.String
import Data.Maybe
import Text.Read
import Data.IORef
import Data.Time.Clock

#ifndef WAI
--- !defined(MIN_VERSION) || !WAI(3,0,0)
responseBuilder :: Status -> [Header] -> Bd.Builder -> Response
responseBuilder status hs zbd =
    error "TODO"
    ---ResponseBuilder status hs Bd.lazyByteString
#endif

-- ****************************************************************

drawingOf :: Drawing -> IO ()
drawingOf d =
    simulationOf () (const $ const Nothing) (const d)

animationOf :: (Double -> Drawing) -> IO ()
animationOf f =
    simulationOf 0.0 (const . Just) f

simulationOf :: (Show a, Read a) => a -> (Double -> a -> Maybe a) -> (a -> Drawing) -> IO ()
#if defined(WAI_CGI)
simulationOf init time view = do
    let svgf = renderSvg . view
    time0 <- getCurrentTime
    run (service (Env init time (const $ const $ id) svgf))
#elif defined(WAI_WARP)
simulationOf init time view = do
    let svgf = renderSvg . view
    time0 <- getCurrentTime
    ref <- newIORef (State time0 init)
    run 8080 (service (Env init time svgf ref))
#endif

activityOf :: (Show a, Read a) => a -> (Double -> Double -> a -> a) -> (a -> Drawing) -> IO ()
#if defined(WAI_CGI)
activityOf init click view = do
    let svgf = renderSvg . view
    time0 <- getCurrentTime
    run (service (Env init (const $ const $ Nothing) click svgf))
#elif defined(WAI_WARP)
activityOf init click view = do
    let svgf = renderSvg . view
    time0 <- getCurrentTime
    ref <- newIORef (State time0 init)
    run 8080 (service (Env init time svgf ref))
#endif


data Env a = Env
        { eInit :: !a
        , eTime :: !(Double -> a -> Maybe a)
        , eClick :: !(Double -> Double -> a -> a)
        , eVisual :: !(a -> Content)
#if defined(WAI_WARP)
        , eStateref :: !(IORef (State a))
#endif
        }

data State a = State
        { sTime0 :: !UTCTime  -- Start time
        , sActual :: !a
        }
    deriving (Show, Read)

-- ****************************************************************
-- Main controller

service :: (Show a, Read a) => Env a -> Application
service1 env req respond = do
    let path = rawPathInfo req
    ---B8.putStrLn ("service: path=" <> path)
    case path of
        "" -> do
            let hs = [ (fromString "Location", B8.pack "draw.cgi/") ]
                resp = responseBuilder movedPermanently301 hs mempty
            respond resp
        "/" -> respond $ htmlResponse pageHtml
        "/init" -> handleInit env req respond
        "/step" -> handleStep env req respond
        _ -> respond $ errorResponse notFound404 ("Invalid path " <> show path)

service env req respond = do
    let path = pathInfo req
    if rawPathInfo req == "" then
            let hs = [ (fromString "Location", B8.pack "draw.cgi/") ]
            in respond $ responseBuilder movedPermanently301 hs mempty
    else case path of
        [] -> respond $ htmlResponse pageHtml
        ["init"] -> handleInit env req respond
        ["step"] -> handleStep env req respond
        ["click",sx,sy] -> handleClick env (read $ T.unpack sx) (read $ T.unpack sy) req respond
        _ -> respond $ errorResponse notFound404 ("Invalid path " <> show path)

handleInit :: Show a => Env a -> Application
handleStep :: (Show a, Read a) => Env a -> Application
handleClick :: (Show a, Read a) => Env a -> Double -> Double -> Application
#if defined(WAI_WARP)
handleInit env req respond = do
    (st, c) <- stInit env
    writeIORef (eStateref env) st
    respond $ contentResponse [] mimeJson c

handleStep env req respond = do
    oldst <- readIORef (eStateref env)
    (newst, c) <- stStep env oldst
    writeIORef (eStateref env) newst
    respond $ contentResponse [] mimeJson c
#else
-- defined(WAI_CGI)
handleInit env req respond = do
    (st, c) <- stInit env
    let cvalue = encodeCookieValue $ T.pack $ show st
        hs = [(fromString "Set-Cookie", fromString "state=" <> cvalue)]
    respond $ contentResponse hs mimeJson c

handleStep env req respond =
    case stateCookie req of
        Nothing -> handleInit env req respond
        Just s -> do
            let oldst = read $ T.unpack $ decodeCookieValue s
            (newst, c) <- stStep env oldst
            let cvalue = encodeCookieValue $ T.pack $ show newst
                hs = [(fromString "Set-Cookie", fromString "state=" <> cvalue)]
            respond $ contentResponse hs mimeJson c

handleClick env x y req respond =
    case stateCookie req of
        Nothing -> handleInit env req respond
        Just s -> do
            let oldst = read $ T.unpack $ decodeCookieValue s
            (newst, c) <- stClick env x y oldst
            let cvalue = encodeCookieValue $ T.pack $ show newst
                hs = [(fromString "Set-Cookie", fromString "state=" <> cvalue)]
            respond $ contentResponse hs mimeJson c
#endif

stInit :: Env a -> IO (State a, Content)
stInit env = do
    time0 <- getCurrentTime
    let st = eInit env
        svg = eVisual env st
    pure (State{ sTime0 = time0, sActual = st }, jsonContent 0.0 (Just svg))

stStep :: Env a -> State a -> IO (State a, Content)
stStep env oldst@State{ sTime0 = sTime0, sActual = sActual } = do
    now <- getCurrentTime
    let time = realToFrac (diffUTCTime now sTime0)
        newst = eTime env time sActual
    pure (oldst{ sActual = maybe sActual id newst }, jsonContent time (eVisual env <$> newst))

stClick :: Env a -> Double -> Double -> State a -> IO (State a, Content)
stClick env x y oldst@State{ sTime0 = sTime0, sActual = sActual } = do
    now <- getCurrentTime
    let time = realToFrac (diffUTCTime now sTime0)
        newst = eClick env x y sActual
    pure (oldst{ sActual = newst }, jsonContent time (Just $ eVisual env newst))

stateCookie :: Request -> Maybe ByteString
stateCookie req =
    case lookup (fromString "Cookie") $ requestHeaders req of
        Nothing -> Nothing
        Just hvalue -> lookup (fromString "state") $ parseCookies hvalue

encodeCookieValue :: Text -> ByteString
encodeCookieValue s =
    T.encodeUtf8 s

decodeCookieValue :: ByteString -> Text
decodeCookieValue b =
    T.decodeUtf8 b

jsonContent :: Double -> Maybe Content -> Content
jsonContent time Nothing =
    stringContent ("{ \"time\":" ++ show time ++ ", \"svg\":null }")
jsonContent time (Just svg) =
    stringContent ("{ \"time\":" ++ show time ++ ", \"svg\":")
    <> svg' <> stringContent " }"
  where
    svg' = lazyByteStringContent (go (Bd.toLazyByteString (cntBuilder svg)))
    go b =
        let bs = BL.split 10 b
        in BL.singleton 34 <> BL.intercalate (BL.pack [ 92, 110 ]) (go2 <$> bs) <> BL.singleton 34
    go2 b =
        let bs = BL.split 34 b
        in BL.intercalate (BL.pack [ 92, 34 ]) bs

mimeHtml :: ByteString
mimeHtml = fromString "text/html;charset=UTF-8"

mimeSvg :: ByteString
mimeSvg = fromString "image/svg+xml"

mimeJson :: ByteString
mimeJson = fromString "application/json"

htmlResponse :: T.Text -> Response
htmlResponse html =
    contentResponse [] mimeHtml (Content (fromIntegral $ T.length html) (Bd.byteString (T.encodeUtf8 html)))

contentResponse :: [Header] -> ByteString -> Content -> Response
contentResponse hs mime (Content len bd) =
    let headers = (fromString "Content-Length", B8.pack (show len))
                  : (fromString "Content-Type", mime) : hs
    in responseBuilder ok200 headers bd

errorResponse :: Status -> String -> Response
errorResponse status msg =
    let hs = [ (fromString "Content-Type", mimeHtml) ]
        html = "<!DOCTYPE html><html><head><title>Drawing Error</title></head><body>\n"
                <> "<center><h2>ERROR</h2><h3><FONT COLOR=\"red\">" <> msg <> "</FONT></h3></center>\n"
                <> "</body></html>\n"
    in responseBuilder status hs (stringUtf8 html)

-- ****************************************************************
-- Views

pageHtml :: T.Text
pageHtml =
    let html = "<!DOCTYPE html>\n"
            <> "<html><head>\n"
            <> "   <title>Drawing</title>\n"
            <> "   <link rel='stylesheet' type='text/css' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css'>\n"
            <> "   <link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/dat/static-files/draw-cgi/draw.css'>\n"
            <> "   <script type='text/javascript' src='https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js'></script>\n"
            <> "   <script type='text/javascript' src='http://soft0.upc.edu/dat/static-files/draw-cgi/draw.js'></script>\n"
            <> "</head><body><div class='container-fluid'>\n"
            <> "<div class='column' id='canvas'></div>\n"
            <> "<div class='column' id='panel'></div>\n"
            <> "   <script type='text/javascript'>\n"
            <> "     $(document).ready(function(){\n"
            <> "       $.get('http://soft0.upc.edu/dat/static-files/draw-cgi/panel.html', function(data){\n"
            <> "         $(\"#panel\").html(data);\n"
            <> "       });\n"
            <> "     });\n"
            <> "   </script>\n"
            <> "</div></body></html>\n"
    in T.pack html

