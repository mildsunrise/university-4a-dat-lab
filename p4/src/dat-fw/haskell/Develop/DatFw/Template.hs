
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Develop.DatFw.Template
    ( Html
    , htmlTempl, htmlTemplFromString
    , htmlTemplFile
    , widgetTempl, widgetTemplFromString
    , widgetTemplFile
    )
where
import Develop.DatFw.Template.DocParsers
import Develop.DatFw.Template.Conversions

import Develop.DatFw.Internal.Types
import Develop.DatFw.Widget
import Develop.DatFw.Handler

import System.IO as IO
import Data.Text as T
import Data.Text.IO as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Text.Blaze.Html (Html, toHtml)


htmlTempl :: QuasiQuoter
htmlTempl = do
    QuasiQuoter { quoteDec = fail "Invalid quasi-quoter to generate declarations"
                , quoteType = fail "Invalid quasi-quoter to generate types"
                , quotePat = fail "Invalid quasi-quoter to generate patterns"
                , quoteExp = htmlTemplFromString }
    
htmlTemplFromString :: String -> Q Exp
htmlTemplFromString text =
    case parseTemplate text of
        Right docs -> do
            render <- newName "_render"
            let env = Env { envFromHtml = id
                          , envFromRoute = \ r qs -> [| toHtml $ $(varE render) $r $qs |]
                          , envEmbed = \ e -> [| $e $(varE render) |]
                          , envMempty = [|memptyHtml|], envMappend = [|mappendHtml|] }
            e <- tplDocsToExp env docs
            pure $ LamE [VarP render] e
        Left err -> -- Fail Q monad (with error and position indication)
            fail $ "Template syntax error: " ++ err

memptyHtml :: Html
memptyHtml = mempty

mappendHtml :: Html -> Html -> Html
mappendHtml = mappend

htmlTemplFile :: FilePath -> Q Exp
htmlTemplFile fp = do
    addDependentFile fp
    s <- runIO (readUtf8File fp)
    htmlTemplFromString $ T.unpack s

readUtf8File :: FilePath -> IO Text
readUtf8File fp = do
    IO.withFile fp IO.ReadMode $ \h -> do
        IO.hSetEncoding h utf8
#ifdef WINDOWS
        filter ('\r'/=)
#else
        id
#endif
            <$> T.hGetContents h


widgetTempl :: QuasiQuoter
widgetTempl = do
    QuasiQuoter { quoteDec = fail "Invalid quasi-quoter to generate declarations"
                , quoteType = fail "Invalid quasi-quoter to generate types"
                , quotePat = fail "Invalid quasi-quoter to generate patterns"
                , quoteExp = widgetTemplFromString }
    
widgetTemplFromString :: String -> Q Exp
widgetTemplFromString text =
    case parseTemplate text of
        Right docs -> do
            let env = Env { envFromHtml = \ e -> [| toWidget $e |]
                          , envFromRoute = \ r qs -> [| getUrlRender >>= \ render -> toWidget $ toHtml $ render $r $qs |]
                          , envEmbed = id
                          , envMempty = [|memptyWidget|], envMappend = [|mappendWidget|] }
            tplDocsToExp env docs
        Left err -> -- Fail Q monad (with error and position indication)
            fail $ "Template syntax error: " ++ err

memptyWidget :: Widget site
memptyWidget = mempty

mappendWidget :: Widget site -> Widget site -> Widget site
mappendWidget = mappend

widgetTemplFile :: FilePath -> Q Exp
widgetTemplFile fp = do
    addDependentFile fp
    s <- runIO (readUtf8File fp)
    widgetTemplFromString $ T.unpack s

