
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
where
import Found
import Model

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Template
import Develop.DatFw.Auth
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format
import           Data.Maybe
import           Control.Monad
import           Control.Monad.IO.Class   -- imports liftIO
import           Text.Read (readMaybe)

{---------------------------------------------------------------------
                TODO
---------------------------------------------------------------------}

formatPosted :: UTCTime -> String
formatPosted = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"


themeForm :: Maybe Theme -> AForm (HandlerFor Forum) Theme
themeForm theme =
    Theme <$> maybe
                (freq (checkM checkUserExists textField)
                (withPlaceholder "Introduiu el nom de l'usuari responsable" "Nom del responsable")
                Nothing)
              pure (tLeader <$> theme)
          <*> freq textField (withPlaceholder "Introduiu la categoria" "Categoria") (tCategory <$> theme)
          <*> freq textField (withPlaceholder "Introduiu el títol del tema" "Titol") (tTitle <$> theme)
          <*> freq textareaField (withPlaceholder "Introduiu la descripció del tema" "Descripció") (tDescription <$> theme)

checkUserExists :: Text -> HandlerFor Forum (Either Text Text)
checkUserExists uname = do
    users <- getsSite forumUsers
    case lookup uname users of
        Nothing -> pure $ Left "L'usuari no existeix"
        Just _  -> pure $ Right uname

getHomeR :: HandlerFor Forum Html
getHomeR = do
    -- Get model info
    db <- getsSite forumDb
    themes <- liftIO $ getThemeList db
    mbuser <- maybeAuthId
    tformw <- generateAFormPost (themeForm Nothing)
    -- Return HTML content
    defaultLayout $ $(widgetTemplFile "src/forum/templates/home.html")

postHomeR :: HandlerFor Forum Html
postHomeR = do
    user <- requireAuthId
    requireAdmin user
    db <- getsSite forumDb
    (tformr, tformw) <- runAFormPost (themeForm Nothing)
    case tformr of
        FormSuccess newtheme -> do
            liftIO $ addTheme newtheme db
            redirectRoute HomeR []
        _ -> do
            themes <- liftIO $ getThemeList db
            let mbuser = Just user
            defaultLayout $(widgetTemplFile "src/forum/templates/home.html")


questionForm :: AForm (HandlerFor Forum) (Text, Text)
questionForm =
    (,) <$> freq textField (withPlaceholder "Introduiu l'assumpte de la pregunta" "Assumpte") Nothing
        <*> freq textareaField (withPlaceholder "Introduiu la text de la pregunta" "Text") Nothing

getThemeR :: ThemeId -> HandlerFor Forum Html
getThemeR tid = do
    -- Get model info
    db <- getsSite forumDb
    maybeTheme <- liftIO $ getTheme tid db
    theme <- maybe notFound pure maybeTheme
    questions <- liftIO $ getQuestionList tid db
    mbuser <- maybeAuthId
    tformw <- generateAFormPost (themeForm $ Just theme)
    qformw <- generateAFormPost questionForm
    -- Return HTML content
    defaultLayout $ $(widgetTemplFile "src/forum/templates/theme.html")

postThemeR :: ThemeId -> HandlerFor Forum Html
postThemeR tid = do
    user <- requireAuthId
    db <- getsSite forumDb
    maybeTheme <- liftIO $ getTheme tid db
    theme <- maybe notFound pure maybeTheme
    modifyForm <- isJust <$> lookupPostParam "modify"
    deleteForm <- isJust <$> lookupPostParam "delete"
    addForm <- isJust <$> lookupPostParam "add"
    if modifyForm
      then do
        requireLeader theme user
        (tformr, tformw) <- runAFormPost (themeForm $ Just theme)
        case tformr of
            FormSuccess newtheme -> do
                liftIO $ updateTheme tid newtheme db
                redirectRoute (ThemeR tid) []
            _ -> do
                questions <- liftIO $ getQuestionList tid db
                qformw <- generateAFormPost questionForm
                let mbuser = Just user
                defaultLayout $(widgetTemplFile "src/forum/templates/theme.html")
      else if deleteForm then do
        checkBoxes <- lookupPostParams "qid"
        let qids = catMaybes ((readMaybe . T.unpack) <$> checkBoxes)
        forM_ qids $ \ qid ->
            liftIO $ deleteFullQuestion qid db
        redirectRoute (ThemeR tid) []
      else if addForm then do
        (qformr, qformw) <- runAFormPost questionForm
        case qformr of
            FormSuccess (title, text) -> do
                time <- liftIO $ getCurrentTime
                let question = Question tid user time title text
                qid <- liftIO $ addQuestion question db
                redirectRoute (QuestionR tid qid) []
            _ -> do
                questions <- liftIO $ getQuestionList tid db
                tformw <- generateAFormPost (themeForm $ Just theme)
                let mbuser = Just user
                defaultLayout $(widgetTemplFile "src/forum/templates/theme.html")
      else
        invalidArgs ["modify","delete","add"]


answerForm :: AForm (HandlerFor Forum) Text
answerForm =
    freq textareaField (withPlaceholder "Introduiu la text de la resposta" "Text") Nothing

getQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
getQuestionR tid qid = do
    -- Get model info
    db <- getsSite forumDb
    maybeTheme <- liftIO $ getTheme tid db
    theme <- maybe notFound pure maybeTheme
    maybeQuestion <- liftIO $ getQuestion qid db
    question <- maybe notFound pure maybeQuestion
    answers <- liftIO $ getAnswerList qid db
    mbuser <- maybeAuthId
    aformw <- generateAFormPost answerForm
    -- Return HTML content
    defaultLayout $ $(widgetTemplFile "src/forum/templates/question.html")

postQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
postQuestionR tid qid = do
    user <- requireAuthId
    db <- getsSite forumDb
    maybeTheme <- liftIO $ getTheme tid db
    theme <- maybe notFound pure maybeTheme
    deleteForm <- isJust <$> lookupPostParam "delete"
    addForm <- isJust <$> lookupPostParam "add"
    if deleteForm
      then do
        checkBoxes <- lookupPostParams "aid"
        let aids = catMaybes ((readMaybe . T.unpack) <$> checkBoxes)
        forM_ aids $ \ aid ->
            liftIO $ deleteAnswer aid db
        redirectRoute (QuestionR tid qid) []
      else if addForm then do
        (aformr, aformw) <- runAFormPost answerForm
        case aformr of
            FormSuccess text -> do
                time <- liftIO $ getCurrentTime
                let answer = Answer qid user time text
                liftIO $ addAnswer answer db
                redirectRoute (QuestionR tid qid) []
            _ -> do
                maybeQuestion <- liftIO $ getQuestion qid db
                question <- maybe notFound pure maybeQuestion
                answers <- liftIO $ getAnswerList qid db
                let mbuser = Just user
                defaultLayout $(widgetTemplFile "src/forum/templates/question.html")
      else
        invalidArgs ["delete","add"]

