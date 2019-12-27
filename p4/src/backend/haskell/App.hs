
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Found
import Model
import Handler

import           Develop.DatFw
import           Develop.DatFw.Dispatch
import           Develop.DatFw.Auth2

import           Network.Wai

import           Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------
-- Application initialization

-- NOTA: Canvieu al vostre fitxer de la base de dades
forumDbName :: Text
forumDbName = "/home/pract/LabWEB/WEBprofe/private/sqlite3-dbs/forum.db"

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openExistingDb forumDbName
    toApp Forum{ forumDb = db
               , forumUsers = [("admin", "201811"), ("user1", "1234"), ("user2", "1234")]
               }


-- ---------------------------------------------------------------
-- Main controller

instance Dispatch Forum where
    dispatch = routing $
            -- RESTful API:
            --  URI: /themes
            route ( onStatic ["themes"] ) ThemesR
                [ onMethod "GET" getThemesR             -- get the theme list
                , onMethod "POST" postThemesR           -- create a new theme
                ] <||>
            --  URI: /themes/TID
            route ( onStatic ["themes"] <&&> onDynamic ) ThemeR
                [ onMethod1 "GET" getThemeR             -- get a theme
                , onMethod1 "DELETE" deleteThemeR       -- delete a theme
                ] <||>
            --  URI: /themes/TID/questions
            route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["questions"] ) ThemeQuestionsR
                [ onMethod1 "GET" getThemeQuestionsR    -- get a theme's question list
                , onMethod1 "POST" postThemeQuestionsR  -- create a new question
                ] <||>
            --  URI: /questions/QID
            route ( onStatic ["questions"] <&&> onDynamic ) QuestionR
                [ onMethod1 "GET" getQuestionR          -- get a question
                , onMethod1 "DELETE" deleteQuestionR    -- delete a question
                ] <||>
            --  URI: /questions/QID/answers
            route ( onStatic ["questions"] <&&> onDynamic <&&> onStatic ["answers"] ) QuestionAnswersR
                [ onMethod1 "GET" getQuestionAnswersR   -- get a question's answer list
                , onMethod1 "POST" postQuestionAnswersR -- create a new answer
                ] <||>
            --  URI: /answers/AID
            route ( onStatic ["answers"] <&&> onDynamic ) AnswerR
                [ onMethod1 "GET" getAnswerR            -- get an answer
                , onMethod1 "DELETE" deleteAnswerR      -- delete an answer
                ] <||>
            --  URI: /user
            route ( onStatic ["user"] ) UserR
                [ onMethod "GET" getUserR               -- get the current user
                ] <||>
            -- Authentication interface:
            routeSub (onStatic ["auth"]) AuthR getAuth

