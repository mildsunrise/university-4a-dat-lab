
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Develop.DatFw.Auth2
where
import Develop.DatFw
import Develop.DatFw.Dispatch
import Develop.DatFw.Handler
import Develop.DatFw.Handler_ext
import Develop.DatFw.Widget
import Develop.DatFw.Template
import Develop.DatFw.Content

import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.ByteString.Builder
import           Text.Blaze.Html
import           Data.List
import           Control.Monad            -- imports when, ...


-- ****************************************************************
-- Auth: Subsistema d'autenticacio.
-- Aquest subsitema ja es dona fet i es susceptible de ser modificat pel professor en versions futures.

-- Definicio del tipus Auth (estat del subsistema) i de la funcio que obte el corresponent valor a partir d'un site.
--      En aquest cas, no hi ha informacio global de subsistema i la funcio getAuth sempre obte un valor constant.
data Auth = Auth

getAuth :: site -> Auth
getAuth _ = Auth

-- Rutes del subsitema Auth.
data instance Route Auth =
        LoginR | LogoutR | DialogR Text [Text]

instance RenderRoute Auth where
    renderRoute LoginR  = (["login"], [])
    renderRoute LogoutR = (["logout"], [])
    renderRoute (DialogR name path) = ("dialog":name:path, [])


-- Classe que defineix els aspectes de la configuracio del subsitema Auth per als diferents llocs.
class WebApp site => WebAuth site where
    authLayout :: (MonadHandler m, site ~ HandlerSite m) => Widget site -> m Html
    loginDest  :: Route site
    logoutDest :: Route site
    authPlugins :: site -> [AuthPlugin site]
    -- Default definitions:
    authLayout widget =
        liftHandler $ defaultLayout widget
    -- | After login and logout, redirect to the referring page, instead of
    -- 'loginDest' and 'logoutDest'. Default is 'False'.
    redirectToReferer :: site -> Bool
    redirectToReferer _ = False



-- Utilitats a ser usades pels llocs
type AuthId = Text

authId_SESSION_KEY :: Text
authId_SESSION_KEY = "__AUTHID"

maybeAuthId :: MonadHandler m => m (Maybe AuthId)
maybeAuthId =
        lookupSession authId_SESSION_KEY

requireAuthId :: MonadHandler m => m AuthId
requireAuthId = do
    mbaid <- maybeAuthId
    maybe notAuthenticated pure mbaid


-- ****************************************************************
-- Plugins.

data AuthPlugin site = AuthPlugin
        { pluginName :: Text
        , pluginLoginW :: (Route Auth -> Route site) -> Widget site
        , pluginDispatch :: [Text] -> SubHandlerFor Auth site TypedContent
        }

-- ****************************************************************
-- Dispatch de les rutes del subsitema Auth.

instance WebAuth site => SubDispatch Auth site where
    subDispatch = subrouting
            $ route ( onStatic ["login"] ) LoginR
                [ onMethod "GET" getLoginR
                ]
            <||> route ( onStatic ["logout"] ) LogoutR
                (onAnyMethod handleLogoutR)
            <||> route ( onStatic ["dialog"] <&&> onDynamic <&&> onDynamicMulti ) DialogR
                (onAnyMethod2 handleDialogR)

-- ****************************************************************
-- Handlers del subsitema Auth.

getLoginR :: WebAuth site => SubHandlerFor Auth site Html
getLoginR = do
    setUltDestReferer'
    -- Return HTML page
    plugins <- getsSite authPlugins
    toMaster <- getRouteToMaster
    authLayout $(widgetTemplFile "src/dat-fw-ext/templates/auth/login.html")

setUltDestReferer' :: WebAuth site => SubHandlerFor Auth site ()
setUltDestReferer' = do
    master <- getSite
    when (redirectToReferer master) setUltDestReferer

handleLogoutR :: WebAuth site => SubHandlerFor Auth site ()
handleLogoutR = do
    setUltDestReferer'
    deleteSession authId_SESSION_KEY
    redirectUltDestRoute logoutDest []

handleDialogR :: WebAuth site => Text -> [Text] -> SubHandlerFor Auth site TypedContent
handleDialogR pgName pgPath = do
    plugins <- getsSite authPlugins
    case find ((pgName ==) . pluginName) plugins of
        Nothing -> notFound
        Just plugin -> pluginDispatch plugin pgPath


setAuthIdRedirect :: WebAuth site => AuthId -> SubHandlerFor Auth site a
setAuthIdRedirect aid = do
    setSession authId_SESSION_KEY aid
    redirectUltDestRoute loginDest []

