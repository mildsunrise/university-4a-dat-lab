
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler
where
import Found
import Model
import Json

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Widget
import Develop.DatFw.Auth2

import Network.Wai

import           Control.Monad            -- imports forM, ...
import           Control.Monad.IO.Class   -- imports liftIO
import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe               -- imports isJust, isNothing, catMaybes, ...
import           Data.Monoid
import           Data.Bool                -- imports bool
import           Text.Read (readMaybe)
import           Data.Time

-- ---------------------------------------------------------------
-- Conversion to JSON representation
-- ---------------------------------------------------------------

themeToJSON :: (ThemeId, Theme) -> HandlerFor Forum Value
themeToJSON (tid, Theme{..}) = do
    ur <- getUrlRenderNoParams
    pure $ object [("id", toJSON tid), ("selfLink", toJSON $ ur $ ThemeR tid),
                   ("leader", toJSON tLeader), ("category", toJSON tCategory),
                   ("title", toJSON tTitle), ("description", toJSON tDescription)]

questionToJSON :: (QuestionId, Question) -> HandlerFor Forum Value
questionToJSON (qid, Question{..}) = do
    ur <- getUrlRenderNoParams
    pure $ object [("id", toJSON qid), ("selfLink", toJSON $ ur $ QuestionR qid),
                   ("themeId", toJSON qTheme), ("themeLink", toJSON $ ur $ ThemeR qTheme),
                   ("user", toJSON qUser), ("posted", toJSON qPosted),
                   ("title", toJSON qTitle), ("text", toJSON qText)]


answerToJSON :: (AnswerId, Answer) -> HandlerFor Forum Value
answerToJSON (aid, Answer{..}) = do
    ur <- getUrlRenderNoParams
    pure $ object [("id", toJSON aid), ("selfLink", toJSON $ ur $ AnswerR aid),
                   ("questionId", toJSON aQuestion), ("questionLink", toJSON $ ur $ QuestionR aQuestion),
                   ("user", toJSON aUser), ("posted", toJSON aPosted), ("text", toJSON aText)]

-- ---------------------------------------------------------------
-- Parsing JSON request body
-- ---------------------------------------------------------------

getRequestJSON :: (Object -> Parser a) -> HandlerFor Forum a
getRequestJSON fromObject = do
    jval <- getRequestJSON'
    case parseEither (withObject "Request body as an object" fromObject) jval of
        Left err -> invalidArgs [T.pack err]
        Right v -> pure v

getRequestJSON' :: HandlerFor Forum Value
getRequestJSON' = do
    req <- getRequest
    cType <- maybe (invalidArgs ["Request body with application/json content expected"])
                   (pure . T.decodeUtf8)
                   (lookup "content-type" (requestHeaders req))
    let isJson = "application/json" == fst (T.break (';' ==) cType)
    when (not isJson) $
        invalidArgs ["Request body with application/json content expected"]
    lbytes <- liftIO $ strictRequestBody req
    case eitherDecode lbytes of
        Left err -> invalidArgs [T.pack err]
        Right v -> pure v


-- ---------------------------------------------------------------
-- Utility: Run an IO action with the database
-- ---------------------------------------------------------------

runDbAction :: (ForumDb -> IO a) -> HandlerFor Forum a
runDbAction f = do
    db <- getsSite forumDb
    liftIO $ f db


-- ---------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------

-- ---------------------------------------------------------------
-- User

getUserR :: HandlerFor Forum Value
getUserR = do
    -- Get model info
    mbuser <- maybeAuthId
    pure $ object [("name", toJSON mbuser), ("isAdmin", toJSON $ maybe False isAdmin mbuser)]

-- ---------------------------------------------------------------
-- Themes list

getThemesR :: HandlerFor Forum Value
getThemesR = do
    -- Get model info
    themes <- runDbAction getThemeList
    jthemes <- forM themes themeToJSON
    pure $ object [("items", toJSON jthemes)]

postThemesR :: HandlerFor Forum Value
postThemesR = do
    user <- requireAuthId
    (pLeader, pTitle, pDescription) <- getRequestJSON $ \ obj -> do
        leader <- obj .: "leader"
        title <- obj .: "title"
        description <- obj .: "description"
        pure (leader, title, description)
    when (not $ isAdmin user) (permissionDenied "User is not an admin")
    let newtheme = Theme pLeader "" pTitle pDescription
    tid <- runDbAction $ addTheme newtheme
    getThemeR tid

-- ---------------------------------------------------------------
-- Theme

getThemeR :: ThemeId -> HandlerFor Forum Value
getThemeR tid = do
    theme <- runDbAction (getTheme tid) >>= maybe notFound pure
    themeToJSON (tid, theme)

deleteThemeR :: ThemeId -> HandlerFor Forum Value
deleteThemeR tid = do
    user <- requireAuthId
    theme <- runDbAction (getTheme tid) >>= maybe notFound pure
    when (not $ isAdmin user) (permissionDenied "User is not an admin")
    runDbAction $ deleteFullTheme tid
    pure $ object []

deleteFullTheme :: ThemeId -> ForumDb -> IO ()
deleteFullTheme tid db = do
    fail "A completar per l'estudiant"

-- ---------------------------------------------------------------
-- Questions list

getThemeQuestionsR :: ThemeId -> HandlerFor Forum Value
getThemeQuestionsR tid = do
    questions <- runDbAction $ getQuestionList tid
    jqs <- forM questions questionToJSON
    pure $ object [("items", toJSON jqs)]

postThemeQuestionsR :: ThemeId -> HandlerFor Forum Value
postThemeQuestionsR tid = do
    fail "A completar per l'estudiant"

-- ---------------------------------------------------------------
-- Question

getQuestionR :: QuestionId -> HandlerFor Forum Value
getQuestionR qid = do
    fail "A completar per l'estudiant"

deleteQuestionR :: QuestionId -> HandlerFor Forum Value
deleteQuestionR qid = do
    fail "A completar per l'estudiant"

-- ---------------------------------------------------------------
-- Answers list

getQuestionAnswersR :: QuestionId -> HandlerFor Forum Value
getQuestionAnswersR qid = do
    fail "A completar per l'estudiant"

postQuestionAnswersR :: QuestionId -> HandlerFor Forum Value
postQuestionAnswersR qid = do
    fail "A completar per l'estudiant"

-- ---------------------------------------------------------------
-- Answer

getAnswerR :: AnswerId -> HandlerFor Forum Value
getAnswerR aid = do
    fail "A completar per l'estudiant"

deleteAnswerR :: AnswerId -> HandlerFor Forum Value
deleteAnswerR aid = do
    fail "A completar per l'estudiant"


