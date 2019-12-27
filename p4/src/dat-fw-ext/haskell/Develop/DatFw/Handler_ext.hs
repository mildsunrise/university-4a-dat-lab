
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Develop.DatFw.Handler_ext
where
import           Develop.DatFw
import           Develop.DatFw.Handler

import           Network.Wai as W

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import           Control.Monad


ultDest_SESSION_KEY :: Text
ultDest_SESSION_KEY = "__ULT"

-- | Sets the ultimate destination to the referer request header, if present.
--
-- This function will not overwrite an existing ultdest.
setUltDestReferer :: MonadHandler m => m ()
setUltDestReferer = do
    mdest <- lookupSession ultDest_SESSION_KEY
    when (isNothing mdest) $ do
        req <- getRequest
        maybe (pure ()) setUltDestBS $ lookup "referer" $ W.requestHeaders req
  where
    setUltDestBS = setUltDest . T.pack . B8.unpack
{--
setUltDestCurrent = do
    mburl <- getCurrentRoute
    case mburl of
        Nothing -> pure ()
        Just route -> do
            urlRender <- getUrlRender
            query <- queryString <$> getRequest
            let params = map (\(n,Just v) -> (T.decodeUtf8 n, T.decodeUtf8 v)) $ filter (isJust . snd) query
            setUltDest (urlRender route params)
--}

-- | Redirect to the ultimate destination in the user's session. Clear the value from the session.
--
-- The ultimate destination is set with 'setUltDest' or 'setUltDestRoute'.
--
-- This function uses 'redirect', and thus will perform a temporary redirect to a GET request.
redirectUltDestRoute :: MonadHandler m => Route (HandlerSite m) -> [(Text, Text)]  -- ^ default destination if nothing in session
                                     -> m a
redirectUltDestRoute route params = do
    mb <- lookupSession ultDest_SESSION_KEY
    case mb of
        Nothing -> do
            urlRender <- getUrlRender
            redirect (urlRender route params)
        Just u -> do
            deleteSession ultDest_SESSION_KEY
            redirect u

