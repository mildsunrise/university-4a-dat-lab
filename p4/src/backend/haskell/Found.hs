
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Found
where
import Model

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Widget
import Develop.DatFw.Auth2.Password
import Develop.DatFw.Auth2.DatIdent

import Network.Wai.Middleware.Approot(getApproot)

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString.Builder
import Data.Int

-- ---------------------------------------------------------------
-- Definició dels tipus del site Forum i de les corresponents rutes.

data Forum = Forum { forumDb :: ForumDb
                   , forumUsers :: [(Text, Text)]
                   }

data instance Route Forum =
        -- API
          ThemesR
        | ThemeR ThemeId | ThemeQuestionsR ThemeId
        | QuestionR QuestionId | QuestionAnswersR QuestionId
        | AnswerR AnswerId
        | UserR
        -- Authentication UI
        | AuthR (Route Auth)
        -- External resources
        | StaticR [Text] [(Text,Text)]

instance RenderRoute Forum where
    renderRoute ThemesR   = (["themes"], [])
    renderRoute (ThemeR tid) = (["themes",toPathPiece tid], [])
    renderRoute (ThemeQuestionsR tid) = (["themes",toPathPiece tid,"questions"], [])
    renderRoute (QuestionR qid) = (["questions",toPathPiece qid], [])
    renderRoute (QuestionAnswersR qid) = (["questions",toPathPiece qid,"answers"], [])
    renderRoute (AnswerR aid) = (["answers",toPathPiece aid], [])
    renderRoute UserR = (["user"], [])
    renderRoute (StaticR ss ps) = (ss, ps)
    renderRoute (AuthR authr) = let (path,qs) = renderRoute authr in ("auth":path, qs)

-- Nota: Els tipus ThemeId, QuestionId i AnswerId són alias de Int64 (veieu el model)
instance PathPiece Int64 where
    toPathPiece = showToPathPiece
    fromPathPiece = readFromPathPiece

-- ---------------------------------------------------------------
-- Instancia de WebApp (configuracio del lloc) per a Forum.

instance WebApp Forum where
    appRoot _ req = T.decodeUtf8 $ getApproot req
    urlRenderOverride site r@(StaticR _ _) qs =
        let (segs, rqs) = renderRoute r
        in Just $ joinPath site "http://soft0.upc.edu/~USER/practica4" segs (rqs <> qs)
    urlRenderOverride _ _ _ = Nothing
    authRoute _ = Nothing
    -- Altres configuracions: (com en la practica 3)

-- ---------------------------------------------------------------
-- Instancia de WebAuth (configuracio del subsistema d'autenticacio Auth) per a Forum.
-- Instancia de WebAuthPassword (configuracio del plugin d'autenticacio "password") per a Forum.

instance WebAuth Forum where
    loginDest = StaticR ["index.html"] []
    logoutDest = StaticR ["index.html"] []
    authPlugins _ = [ passwordPlugin, datIdentPlugin ]
    redirectToReferer _ = True

instance WebAuthPassword Forum where
    validatePassword name password = do
        users <- getsSite forumUsers
        case lookup name users of
            Nothing    -> pure False
            Just upass -> pure $ upass == password

-- ---------------------------------------------------------------
-- Funcions auxiliars usades en la lògica d'autorització dels handlers.

isAdmin :: UserId -> Bool
isAdmin u = u == "admin"

isLeader :: Theme -> UserId -> Bool
isLeader t u = u == tLeader t


