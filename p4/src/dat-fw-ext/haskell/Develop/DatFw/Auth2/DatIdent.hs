
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Develop.DatFw.Auth2.DatIdent
    ( module Develop.DatFw.Auth2
    , datIdentPlugin
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
import           Network.HTTP.Types

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Text.Blaze.Html
import           Data.Int
import           Control.Monad            -- imports when, ...


-- ****************************************************************
-- Auth.DatIdent: Plugin d'autenticacio.

datIdentPlugin :: WebAuth site => AuthPlugin site
datIdentPlugin = AuthPlugin
        { pluginName = "datident"
        , pluginLoginW = loginW
        , pluginDispatch = dispatch
        }


-- ****************************************************************
-- Handlers del subsitema Auth.Password.

loginW :: (Route Auth -> Route site) -> Widget site
loginW toMaster = do
    let mberr = Nothing :: Maybe Text
    $(widgetTemplFile "src/dat-fw-ext/templates/auth/datident/login.html")

dispatch :: WebAuth site => [Text] -> SubHandlerFor Auth site TypedContent
dispatch ["forward"] = do
    req <- getRequest
    if requestMethod req == "POST" then postForwardR
                                   else badMethod
dispatch ["return"] = do
    req <- getRequest
    if requestMethod req == "POST" then postReturnR
                                   else badMethod
dispatch _ = notFound

postForwardR :: SubHandlerFor Auth site a
postForwardR = do
    server <- lookupPostParam "server" >>= maybe (invalidArgs ["server"]) pure
    tm <- getRouteToMaster
    returnUrl <- getUrlRender >>= \ render -> pure $ render (tm $ DialogR "datident" ["return"]) []
    let params = [("ident.mode", "authn_req"), ("ident.return_to", returnUrl)]
        qtext = renderQueryText True (fmap Just <$> params)
        toUrl = server <> "/sso/browser" <> T.decodeUtf8 (BL.toStrict $ toLazyByteString qtext)
    redirect toUrl

postReturnR :: WebAuth site => SubHandlerFor Auth site a
postReturnR = do
    mode <- lookupPostParam "ident.mode" >>= maybe (invalidArgs ["ident.mode"]) pure
    server <- lookupPostParam "ident.server" >>= maybe (invalidArgs ["ident.server"]) pure
    identity <- lookupPostParam "ident.identity" >>= maybe (invalidArgs ["ident.identity"]) pure
    return_to <- lookupPostParam "ident.return_to" >>= maybe (invalidArgs ["ident.return_to"]) pure
    tm <- getRouteToMaster
    returnUrl <- getUrlRender >>= \ render -> pure $ render (tm $ DialogR "datident" ["return"]) []
    when (mode /= "id_res" || return_to /= returnUrl) $
        invalidArgs ["ident.mode","ident.return_to"]
    setAuthIdRedirect $ server <> identity

