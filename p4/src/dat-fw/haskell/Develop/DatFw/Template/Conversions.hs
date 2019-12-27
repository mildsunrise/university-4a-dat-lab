
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Develop.DatFw.Template.Conversions
where
import Develop.DatFw.Template.Types

import           Data.Text(Text)
import qualified Data.Text as T
import           Text.Read
import           Text.Parsec
import           Text.Parsec.Text
import           Text.Blaze.Html
import           Data.Char

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

{--
        urender :: (url -> [(String,String)] -> String)
        urender -> id(HTML) <> id(Text(urender ROUTE [])) <> EMBED urender

        getUrlRender >>= \ urender' -> toWidget HTML <> toWidget (Text (urender' ROUTE [])) <> EMBED
        (wd) -> {
                urender = getUrlRender(wd.hdata);
                toWidgetHtml HTML wd *> toWidgetHtml (Text (urender ROUTE [])) wd *> EMBED.run wd
                return null;
        }
--}
data Env = Env
        { envFromHtml :: Q Exp -> Q Exp         -- HTML -> MONOID
        , envFromRoute :: Q Exp -> Q Exp -> Q Exp -- Route a -> [(Text,Text)] -> MONOID
        , envEmbed :: Q Exp -> Q Exp            -- WIDGET -> MONOID
        , envMempty :: Q Exp                    -- MONOID dictionary
        , envMappend :: Q Exp
        }

tplDocsToExp :: Env -> [TplDoc] -> Q Exp
tplDocsToExp env [] =
    envMempty env
tplDocsToExp env [d] =
    tplDocToExp env d
tplDocsToExp env (d:ds) =
    [| $(envMappend env) $(tplDocToExp env d) $(tplDocsToExp env ds) |]

-- Convert to HTML build code
tplDocToExp :: Env -> TplDoc -> Q Exp
tplDocToExp env (DocForall le i ds) = do
    inm <- newName i
    let env2 = env
    [| foldMap (\ $(varP inm) -> $(tplDocsToExp env2 ds)) $(tplExprToExp env le) |]

tplDocToExp env (DocCond e yes no) =
    [| if $(tplExprToExp env e) then $(tplDocsToExp env yes) else $(tplDocsToExp env no) |]

tplDocToExp env (DocMaybe e i yes no) = do
    inm <- newName i
    let env2 = env
    [| maybe $(tplDocsToExp env no) (\ $(varP inm) -> $(tplDocsToExp env2 yes)) $(tplExprToExp env e) |]

tplDocToExp env (DocRaw s) =
    envFromHtml env [| preEscapedText (T.pack s) |]

tplDocToExp env (DocHtmlE e) =
    envFromHtml env [| toHtml $(tplExprToExp env e) |]

tplDocToExp env (DocRouteE e) =
    envFromRoute env (tplExprToExp env e) [| [] |]

tplDocToExp env (DocEmbedE e) =
    envEmbed env (tplExprToExp env e)

tplDocToExp env d = do
    litE $ stringL "TODO: tplDocToExp"


-- Lift template expressions
tplExprToExp :: Env -> TplExpr -> Q Exp
tplExprToExp env (ExprIdent s) =
    pure $ identType s (mkName s)

tplExprToExp env (ExprString s) =
    litE $ stringL s

tplExprToExp env (ExprInt i) =
    litE $ integerL i

tplExprToExp env (ExprApp e1 e2) =
    [| $(tplExprToExp env e1) $(tplExprToExp env e2) |]

tplExprToExp env (ExprList es) = do
    ListE <$> mapM (tplExprToExp env) es

tplExprToExp env (ExprTuple es) = do
    TupE <$> mapM (tplExprToExp env) es

identType :: String -> Name -> Exp
identType (c : _) = if isUpper c || c == ':' then ConE else VarE
identType ""      = error "Bad Ident"

