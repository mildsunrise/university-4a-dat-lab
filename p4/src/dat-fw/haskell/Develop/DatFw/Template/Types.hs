
{-# LANGUAGE OverloadedStrings #-}

module Develop.DatFw.Template.Types
where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data TplDoc = DocLet [(TplExpr, TplBinding)] [TplDoc]
            | DocForall TplExpr TplBinding [TplDoc]
            | DocCond TplExpr [TplDoc] [TplDoc]
            | DocMaybe TplExpr TplBinding [TplDoc] [TplDoc]
            | DocRaw String
            | DocHtmlE TplExpr
            | DocRouteE TplExpr
            | DocEmbedE TplExpr

type TplBinding = String        -- TplBinding { ident :: String, typ :: String }

data TplExpr = ExprIdent String
             | ExprInt Integer
             | ExprString String
             | ExprApp TplExpr TplExpr
             | ExprTuple [TplExpr]
             | ExprList [TplExpr]
        deriving (Eq, Show)

{---
data Binding = BindVar Ident
             | BindAs Ident Binding
             | BindConstr DataConstr [Binding]
             | BindTuple [Binding]
             | BindList [Binding]
             | BindRecord DataConstr [(Ident, Binding)] Bool
data Content = ContentRaw String
             | ContentVar TplExpr
             | ContentUrl Bool TplExpr -- ^ bool: does it include params?
             | ContentEmbed TplExpr
             | ContentMsg TplExpr
             | ContentAttrs TplExpr
---}

