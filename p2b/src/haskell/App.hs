
{-# LANGUAGE OverloadedStrings #-}

module App
where
import           Calc
import           Handler
import           Html
import           TextUtils

import           Network.Wai
import           Network.HTTP.Types

import           Data.Complex
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad
import           Control.Applicative

-- ****************************************************************
-- WAI application

calcApp :: Application
calcApp =
    dispatchHandler calcApp'

-- ****************************************************************
-- Handlers

calcApp' :: Handler HandlerResponse
calcApp' = do
    meth <- getMethod
    if meth == methodGet then doGet
    else if meth == methodPost then doPost
    else pure $ HRError methodNotAllowed405 $ "Invalid request method " <> showt meth

-- Executed when the HTTP method is GET
doGet :: Handler HandlerResponse
doGet = do
    calc <- maybe calcInit id <$> getSession "calcState"
    let (_, hpanel) = runForms Nothing
        html = pageHtml hpanel calc Nothing
    pure $ HRHtml html

-- Executed when the HTTP method is POST
doPost :: Handler HandlerResponse
doPost = do
    calc <- maybe calcInit id <$> getSession "calcState"
    q <- getPostQuery
    let (mbev, hpanel) = runForms (Just q)
    case mbev of
        Just ev ->
            case calcSolve1 ev calc of
                Right calc2 -> do
                    setSession "calcState" calc2
                    pure $ HRRedirect "#"
                Left err ->
                    pure $ HRHtml $ pageHtml hpanel calc (Just err)
        Nothing ->
            pure $ HRHtml $ pageHtml hpanel calc Nothing

-- ****************************************************************
-- Tractament dels formularis corresponents a les diferents instruccions
-- de la calculadora.

type CalcNumber = Complex Double

-- panell de 4 x 4 butons
buttons :: [[(CalcInstr CalcNumber, Text)]]
buttons = [ [ (CalcBin (+), "x1 + x0"), (CalcBin (-), "x1 - x0"), (CalcBin (*), "x1 * x0"), (CalcBin (/), "x1 / x0") ]
          , [ (CalcUn ((0:+1)*), "j* x0"), (CalcUn negate, "-x0"), (CalcUn (1/), "1/x0"), (CalcUn conjugate, "conj x0") ]
          , [ (CalcUn ((:+0) . realPart), "real x0"), (CalcUn ((:+0) . imagPart), "imag x0")
            , (CalcUn ((:+0) . magnitude), "mod x0"), (CalcUn ((:+0) . phase), "arg x0") ]
          , [ (CalcDup, "x0, x0, .. <- x0, .."), (CalcPop, ".. <- x0, .."), (CalcFlip, "x1, x0, .. <- x0, x1, ..") ]
          ]

runForms :: Maybe Query -> (Maybe (CalcInstr CalcNumber), Html)
runForms mbq =
    let (mbnum, html1) = runEnterForm mbq
        (mbbut, html2) = runButtonPanel buttons mbq
    in ( (CalcEnter <$> mbnum) <|> mbbut, html1 <> html2 )

runEnterForm :: Maybe Query -> (Maybe CalcNumber, Html)
runEnterForm mbq =
    let mbq' = do
                q <- mbq
                const q <$> lookupParam "enter" q
        (mbr, htmlr) = runDoubleField "real" "Part real" mbq'
        (mbi, htmli) = runDoubleField "imag" "Part imaginària" mbq'
    in ( (:+) <$> mbr <*> mbi
       , hElem "form" [ ("method", "POST"), ("action", "#") ] $
             hElem "div" [ ("class", "form-row") ] $ do
                hElem "div" [ ("class", "col-5") ] htmlr
                hElem "div" [ ("class", "col") ] $
                    hElem "span" [ ("class", "form-control") ] $ hText " + j * "
                hElem "div" [ ("class", "col-5") ] htmli
                hElem "div" [ ("class", "col-1") ] $
                    hElem "button" [ ("type", "submit"), ("class", "btn btn-info"), ("name", "enter") ] $ hText "Enter"
       )

runDoubleField :: Text -> Text -> Maybe Query -> (Maybe Double, Html)
runDoubleField name ph mbq =
    let (res, val, mberr) = case mbq of
            Nothing -> (Nothing, "0.0", Nothing)
            Just q ->
                let t = maybe "" id $ lookupParam name q
                in if T.null t
                    then (Nothing, t, Just "Valor requerit")
                    else case readtEither t of
                        Left err -> (Nothing, t, Just err)
                        Right d -> (Just d, t, Nothing)
        ident = "entry." <> name
        hgroup = case mberr of
            Just err -> hElem "div" [ ("class", "form-group"), ("optional",""), ("has-error","") ] $ do
                            hElemEmpty "input" [ ("type", "text"), ("class", "form-control is-invalid")
                                               , ("id", ident), ("name", name), ("value", val), ("placeholder", ph) ]
                            hElem "div" [ ("class", "invalid-feedback") ] $ do
                                hElem "span" [ ("class", "form-text text-muted") ] $
                                    hText err
            Nothing  -> hElem "div" [ ("class", "form-group"), ("optional","") ] $ do
                            hElemEmpty "input" [ ("type", "text"), ("class", "form-control")
                                               , ("id", ident), ("name", name), ("value", val), ("placeholder", ph) ]
    in ( res, hElem "label" [ ("class","sr-only"), ("for", ident) ] (hText name)
               <> hgroup )

runButtonPanel :: [[(CalcInstr CalcNumber, Text)]] -> Maybe Query -> (Maybe (CalcInstr CalcNumber), Html)
runButtonPanel buttonss mbq =
    let (res, htmls) = unzip $ zipWith go [0 ..] buttonss
    in ( msum res , mconcat htmls )
    where
        go rownum butts =
            let (res, html) = runButtonRow rownum butts mbq
            in (res, hElem "div" [ ("class", "row") ] html)

runButtonRow :: Int -> [(CalcInstr CalcNumber, Text)] -> Maybe Query -> (Maybe (CalcInstr CalcNumber), Html)
runButtonRow rownum buttons mbq =
    let (res, htmls) = unzip $ zipWith go [0 ..] buttons
    in ( msum res, mconcat htmls )
    where
        go colnum butt =
            let (res, html) = runButtonForm rownum colnum butt mbq
            in (res, hElem "div" [ ("class", "col") ] html)

runButtonForm :: Int -> Int -> (CalcInstr CalcNumber, Text) -> Maybe Query -> (Maybe (CalcInstr CalcNumber), Html)
runButtonForm rownum colnum (instr, label) mbq =
    let name = "butt-" <> showt rownum <> "-" <> showt colnum
        res = do
                q <- mbq
                const instr <$> lookupParam name q
        hbutt = hElem "button" [ ("type", "submit"), ("class", "btn btn-outline-info btn-block"), ("name", name) ] $ hText label
    in ( res, hElem "form" [ ("method", "POST"), ("action", "#") ] hbutt )

-- ****************************************************************
-- View

pageHtml :: Html -> CalcStack CalcNumber -> Maybe Text -> Html
pageHtml hpanel calc mberror = do
    hDOCTYPE
    hElem "html" [] $ do
        hElem "head" [] $ do
            hElemEmpty "meta" [ ("charset", "UTF-8") ]
            hElem "title" [] $ hText "Calculadora"
            hElemEmpty "link" [ ("rel", "stylesheet")
                              , ("href", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
                              , ("integrity", "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T")
                              , ("crossorigin", "anonymous") ]
        hElem "body" [] $ do
            hElem "div" [ ("class", "container-fluid") ] $ do
                hElem "h1" [] $ hText "Calculadora"
                hElemEmpty "hr" []
                hpanel
                case mberror of
                    Just err -> hElem "div" [ ("class", "alert alert-danger") ] $ hText err
                    Nothing -> mempty
                hElem "h3" [] $ hText "Estat de la pila:"
                hElem "div" [ ("class", "panel scrollable") ] $
                    hElem "ul" [ ("class", "list-group list-group-flush") ] $
                        mconcat $ zipWith calcElem [0..] calc
    where
        calcElem i num = hElem "li" [ ("class", "list-group-item") ] $ do
                             hElem "span" [ ("class", "badge badge-pill badge-secondary") ] $
                                 hText ("x" <> showt i)
                             hText (showNum num)
        showNum (re :+ im) = (if re /= 0.0 || im == 0.0 then showt re else "")
                              <> (if im < 0.0 then " - j * " <> showt (-im)
                                  else if im > 0.0 then " + j * " <> showt im
                                  else "")

