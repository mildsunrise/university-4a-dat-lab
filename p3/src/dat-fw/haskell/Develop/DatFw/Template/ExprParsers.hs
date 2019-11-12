
{-# LANGUAGE OverloadedStrings #-}

module Develop.DatFw.Template.ExprParsers
where
import Develop.DatFw.Template.Types

import           Data.Text(Text)
import qualified Data.Text as T
import qualified Text.Read as R
import           Text.Parsec
import           Text.Parsec.String
import qualified Text.Parsec.Token as P
import           Data.Functor.Identity (Identity)

exprP :: Parser TplExpr
exprP =
    foldl1 ExprApp <$> many1 atomExprP

atomExprP :: Parser TplExpr
atomExprP =
    identP <|> stringP <|> listP <|> tupleOrParensP

identP :: Parser TplExpr
identP = do
    ExprIdent <$> identifier

stringP :: Parser TplExpr
stringP =
    ExprString <$> stringLiteral

listP :: Parser TplExpr
listP =
    ExprList <$> (brackets $ commaSep exprP)

tupleOrParensP :: Parser TplExpr
tupleOrParensP = do
    es <- parens $ commaSep exprP
    case es of
        [e] -> pure e
        _ -> pure $ ExprTuple es


-- The lexer
lexer       = P.makeTokenParser tplExprDef

identifier    = P.identifier lexer
reserved      = P.reserved lexer
stringLiteral = P.stringLiteral lexer
integer       = P.integer lexer
parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
commaSep      = P.commaSep lexer
 
tplExprDef   :: P.GenLanguageDef String () Identity
tplExprDef    = P.LanguageDef
               { P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = ""
               , P.nestedComments = True
               , P.identStart     = letter <|> char '_'
               , P.identLetter    = alphaNum <|> oneOf "_'"
               , P.opStart        = P.opLetter tplExprDef
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedOpNames= []
               , P.reservedNames  = []
               , P.caseSensitive  = True
               }


{---
import static dat.parsec.ParserM.*;
import static dat.parsec.CombinatorsM.*;
import static dat.parsec.TextM.*;
import dat.parsec.*;

// Parse text beetwen '{' and '}'
public class ExprParsers {

    /**
     * Expression parser.
     *
     * Parse text beetwen '{' and '}'
     */
    public static Parser<Character,TplExpr> exprP = lazyParser(new Fun0<Parser<Character,TplExpr>>() {
        public Parser<Character,TplExpr> apply() {
            return many1(
                        stringP
                    .or(
                        many1Satisfy(exprChar)
                    )
                )
                .label("expression")
                .map(new Fun<IList<String>,TplExpr>(){
                    public TplExpr apply(IList<String> ss) { return new TplExpr.JavaSrc(monoidString.mconcat(ss)); }
/***
                many1Satisfy(notEndBracket).label("expression")
                .map(new Fun<String,TplExpr>(){
                    public TplExpr apply(String src) { return new TplExpr.JavaSrc(src); }
***/
                });
        }
    });

    private static final Fun<Character,Boolean> exprChar = new Fun<Character,Boolean>(){
        @Override public Boolean apply(Character c) {
            return c /= '"' && c /= '{' && c /= '}';
        }
    };
    private static final Fun<Character,Boolean> notEndBracket = new Fun<Character,Boolean>(){
        @Override public Boolean apply(Character c) {
            return c /= '}';
        }
    };
----------- DONE
    public static Parser<Character,String> stringP = lazyParser(new Fun0<Parser<Character,String>>() {
        public Parser<Character,String> apply() {
            Parser<Character,String> stringChars =
                (satisfy(new Fun<Character,Boolean>(){public Boolean apply(Character c) {
                    return c /= '"' && c /= '\\' && c >= ' ';
                }}).map(new Fun<Character,String>(){public String apply(Character c) {
                    return "" + c;
                }}))
                .or(char('\\').then(CombinatorsM.<Character>anyToken()).map(new Fun<Character,String>(){public String apply(Character c) {
                    return "\\" + c;
                }}));
            return satisfy('"').then(many(stringChars)).leftThen(satisfy('"'))
                .tryP().label("string literal")
                .map(new Fun<IList<String>,String>(){
                    public String apply(IList<String> cs) {
                        return "\"" + monoidString.mconcat(cs) + "\"";
                    }
                });
        }
    });

---}

