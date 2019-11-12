
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Develop.DatFw.Auth
where
import Develop.DatFw
import Develop.DatFw.Template
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields

import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.ByteString.Builder
import           Text.Blaze.Html
import           Data.Int
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
        LoginR | LogoutR

instance RenderRoute Auth where
    renderRoute LoginR  = (["login"], [])
    renderRoute LogoutR = (["logout"], [])


-- Classe que defineix els aspectes de la configuracio del subsitema Auth per als diferents llocs.
class WebApp site => WebAuth site where
    authLayout :: (MonadHandler m, site ~ HandlerSite m) => Widget site -> m Html
    loginDest  :: Route site
    logoutDest :: Route site
    validatePassword :: (MonadHandler m, site ~ HandlerSite m) => Text -> Text ->  m Bool
    -- Default definitions:
    authLayout widget =
        liftHandler $ defaultLayout widget


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
-- Dispatch de les rutes del subsitema Auth.

instance WebAuth site => SubDispatch Auth site where
    subDispatch = subrouting
            $ route ( onStatic ["login"] ) LoginR
                [ onMethod "GET" getLoginR
                , onMethod "POST" postLoginR
                ]
            <||> route ( onStatic ["logout"] ) LogoutR
                (onAnyMethod handleLogoutR)

-- ****************************************************************
-- Handlers del subsitema Auth.

loginForm :: MonadHandler m => AForm m (Text, Text)
loginForm =
    (,) <$> freq textField "Nom d'usuari" Nothing
        <*> freq passwordField "Clau d'accés" Nothing

getLoginR :: WebAuth site => SubHandlerFor Auth site Html
getLoginR = do
    -- Return HTML content
    let mberr = Nothing :: Maybe Text
    toMaster <- getRouteToMaster
    (_, formw) <- runAFormPost loginForm
    authLayout $(widgetTemplFile "src/dat-fw/templates/auth/login.html")

postLoginR :: WebAuth site => SubHandlerFor Auth site Html
postLoginR = do
    (FormSuccess (name, password), formw) <- runAFormPost loginForm
    ok <- validatePassword name password
    if ok then do
        setSession authId_SESSION_KEY name
        redirectRoute loginDest []
    else do
        let mberr = Just "Error d'autenticaciò" :: Maybe Text
        toMaster <- getRouteToMaster
        authLayout $(widgetTemplFile "src/dat-fw/templates/auth/login.html")

handleLogoutR :: WebAuth site => SubHandlerFor Auth site ()
handleLogoutR = do
    deleteSession authId_SESSION_KEY
    redirectRoute logoutDest []

