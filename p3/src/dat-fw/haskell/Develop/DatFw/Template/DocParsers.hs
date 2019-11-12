
{-# LANGUAGE OverloadedStrings #-}

module Develop.DatFw.Template.DocParsers
where
import Develop.DatFw.Template.Types
import Develop.DatFw.Template.ExprParsers

import Data.Text as T
import Text.Parsec
import Text.Parsec.String

parseTemplate :: String -> Either String [TplDoc]
parseTemplate s =
    case parse templateP "" s of
        Right docs -> Right docs
        Left err -> Left $ show err

templateP :: Parser [TplDoc]
templateP = docsP <* eof

docsP = many docP

docP :: Parser TplDoc
docP =
    ifP <|> maybeP <|> forallP <|> interpolP <|> rawP

ifP :: Parser TplDoc
ifP = do
    cond <- tagP "if" *> char '{' *> spaces *> exprP <* char '}'
    yes <- docsP
    no <- ifNextP
    pure (DocCond cond yes no)

ifNextP :: Parser [TplDoc]
ifNextP =
    do
        tagP "end"
        pure []
    <|> do
        docs <- tagP "else" *> docsP <* tagP "end"
        pure docs
    <|> do
        cond <- tagP "elseif" *> char '{' *> spaces *> exprP <* char '}'
        yes <- docsP
        no <- ifNextP
        pure [DocCond cond yes no]

maybeP :: Parser TplDoc
maybeP = do
    tagP "maybe"
    char '{'
    spaces
    (binding, expr) <- generatorP
    char '}'
    yes <- docsP
    no <- maybeNextP
    pure $ DocMaybe expr binding yes no

maybeNextP :: Parser [TplDoc]
maybeNextP =
    do
        tagP "end"
        pure []
    <|> do
        tagP "nothing"
        no <- docsP
        tagP "end"
        pure no

forallP :: Parser TplDoc
forallP = do
    tagP "forall"
    char '{'
    spaces
    (binding, expr) <- generatorP
    char '}'
    body <- docsP
    tagP "end"
    pure $ DocForall expr binding body

tagP :: String -> Parser ()
tagP i = try
    (do
        string ('$' : i)
        notFollowedBy alphaNum
      ) <?> ("\"$"++i++"\"")

generatorP :: Parser (TplBinding,TplExpr)
generatorP = do
    bind <- bindingP
    spaces
    string "<-"
    spaces
    value <- exprP
    pure (bind, value)

bindingP :: Parser TplBinding
bindingP = do
    identifier

interpolP :: Parser TplDoc
interpolP =
    let idOnlySupport = False
    in do
         (c, e) <- if idOnlySupport
                then
                    (try $ (,) <$> satisfy interpolChar <*> identP) <|> withBrackets
                else withBrackets
         case c of
                '#' -> pure $ DocHtmlE e
                '@' -> pure $ DocRouteE e
                _   -> pure $ DocEmbedE e
       <?> "interpolation"
  where
    withBrackets =
        (,) <$> try (satisfy interpolChar <* char '{') <*> (exprP <* char '}')

rawP :: Parser TplDoc
rawP =
    DocRaw <$> ((:) <$> (satisfy interpolChar <|> satisfy notSpecialChar) <*> many (satisfy notSpecialChar))
    <?> "raw HTML"

interpolChar :: Char -> Bool
interpolChar c = c == '#' || c == '@' || c == '^'

notSpecialChar :: Char -> Bool
notSpecialChar c = c /= '$' && not (interpolChar c)

