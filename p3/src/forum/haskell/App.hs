
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Found
import Model
import Handler

import Develop.DatFw
import Develop.DatFw.Dispatch
import Develop.DatFw.Auth

import Network.Wai

-- ---------------------------------------------------------------
-- Application initialization

-- NOTA: Canvieu al vostre fitxer de la base de dades
forumDbName = "/home/alba/Documents/uni/dat/p3/forum.db"

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openExistingDb forumDbName
    toApp Forum{ forumDb = db
               , forumUsers = [("admin", "1234"), ("user1", "1234"), ("user2", "1234")]
               }

-- ---------------------------------------------------------------
-- Main controller

instance Dispatch Forum where
    dispatch = routing
            $ route ( onStatic [] ) HomeR
                [ onMethod "GET" getHomeR
                , onMethod "POST" postHomeR
                ]
            <||> route ( onStatic ["themes"] <&&> onDynamic ) ThemeR
                [ onMethod1 "GET" getThemeR
                , onMethod1 "POST" postThemeR
                ]
            <||> route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["qs"] <&&> onDynamic ) QuestionR
                [ onMethod2 "GET" getQuestionR
                , onMethod2 "POST" postQuestionR
                ]
            <||> routeSub (onStatic ["auth"]) AuthR getAuth

