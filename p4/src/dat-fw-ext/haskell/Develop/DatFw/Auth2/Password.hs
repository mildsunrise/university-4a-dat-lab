
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Develop.DatFw.Auth2.Password
    ( module Develop.DatFw.Auth2
    , passwordPlugin, WebAuthPassword(..)
    )
where
import Develop.DatFw hiding (dispatch)
import Develop.DatFw.Auth2
import Develop.DatFw.Handler
import Develop.DatFw.Content
import Develop.DatFw.Widget
import Develop.DatFw.Template
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields

import           Network.Wai

import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.ByteString.Builder
import           Text.Blaze.Html
import           Data.Int
import           Control.Monad            -- imports when, ...


-- ****************************************************************
-- Auth.Password: Plugin d'autenticacio.
-- Aquest subsitema ja es dona fet i es susceptible de ser modificat pel professor en versions futures.

passwordPlugin :: WebAuthPassword site => AuthPlugin site
passwordPlugin = AuthPlugin
        { pluginName = "password"
        , pluginLoginW = loginW
        , pluginDispatch = dispatch
        }

-- Classe que defineix els aspectes de la configuracio del subsitema Password d'Auth per als diferents llocs.
class WebAuth site => WebAuthPassword site where
    validatePassword :: (MonadHandler m, HandlerSite m ~ site) => Text -> Text ->  m Bool


-- ****************************************************************
-- Handlers del subsitema Auth.Password.

loginForm :: MonadHandler m => AForm m (Text, Text)
loginForm =
    (,) <$> freq textField "Nom d'usuari" Nothing
        <*> freq passwordField "Clau d'accés" Nothing

loginW :: (Route Auth -> Route site) -> Widget site
loginW toMaster = do
    let mberr = Nothing :: Maybe Text
    (_, formw) <- runAFormPost loginForm
    $(widgetTemplFile "src/dat-fw-ext/templates/auth/password/login.html")

dispatch :: WebAuthPassword site => [Text] -> SubHandlerFor Auth site TypedContent
dispatch ["login"] = do
    req <- getRequest
    if requestMethod req == "POST" then toTypedContent <$> postLoginR
                                   else badMethod
dispatch _ = notFound

postLoginR :: WebAuthPassword site => SubHandlerFor Auth site Html
postLoginR = do
    (FormSuccess (name, password), formw) <- runAFormPost loginForm
    ok <- validatePassword name password
    if ok then do
        setAuthIdRedirect name
    else do
        let mberr = Just "Error d'autenticaciò" :: Maybe Text
        toMaster <- getRouteToMaster
        authLayout $(widgetTemplFile "src/dat-fw-ext/templates/auth/password/login.html")

