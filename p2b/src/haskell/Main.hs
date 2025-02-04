
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import App

import Network.Wai
import Network.Wai.Handler.CGI (run)

import Control.Exception

-- ****************************************************************

main :: IO ()
main = do
    -- try :: Exception e => IO a -> IO (Either e a)
    r <- try $
        -- CGI adapter: run :: Application -> IO ()
        run calcApp
    case r of
        Right _ -> pure ()
        Left exc -> do
            -- Exception on initialization
            putStrLn "Status: 500 Internal Server Error"
            putStrLn "Content-Type: text/plain"
            putStrLn ""
            putStrLn "Exception on initialization (while excution of 'calcApp'): "
            putStrLn $ "    " ++ show (exc :: SomeException)

