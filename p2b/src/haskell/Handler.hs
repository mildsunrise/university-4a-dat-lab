
{-# LANGUAGE OverloadedStrings #-}

module Handler
    -- Exporta les seguents declaracions d'aquest modul
    ( Handler, dispatchHandler, HandlerResponse(..)
    , getMethod, getSession, setSession, getPostQuery
    -- Export tambe les funcions 'lookupParam' i 'lookupParams' de 'WaiUtils'
    , U.lookupParams, U.lookupParam
    )
where
import           Network.Wai
import           Network.HTTP.Types
import           Web.Cookie

import qualified WaiUtils as U
import           TextUtils
import           Html

import           Data.Text (Text)
import           Control.Monad
import           Control.Applicative

-- ****************************************************************

-- Tipus correponent al monad 'Handler'.
-- El context d'un Handler compren:
--      L'argument Request que permet obtenir informacio sobre la peticio.
--      L'estat del Handler (argument i resultat de les operacions).
newtype Handler a = HandlerC (Request -> HandlerState -> IO (a, HandlerState))

runHandler :: Handler a -> Request -> HandlerState -> IO (a, HandlerState)
runHandler (HandlerC h) = h

-- L'estat del Handler compren:
--      'Cache' dels parametres de la peticio.
--      L'estat de la sessio que s'obte de les corresponents 'cookies'.
--        Aquest estat de sessio es una llista de parelles nom-valor.
data HandlerState = HandlerStateC (Maybe Query) [(Text, Text)]

-- Funcions auxiliars per obtenir informació de l'estat del handler
hsQuery :: HandlerState -> Maybe Query
hsQuery (HandlerStateC q _) = q
hsSession :: HandlerState -> [(Text, Text)]
hsSession (HandlerStateC _ s) = s

-- Funcions auxiliars per modificar l'estat del handler
hsSetQuery :: Maybe Query -> HandlerState -> HandlerState
hsSetQuery q (HandlerStateC _ s) = HandlerStateC q s
hsSetSession :: [(Text, Text)] -> HandlerState -> HandlerState
hsSetSession s (HandlerStateC q _) = HandlerStateC q s

instance Functor Handler where
    -- tipus dels metodes en aquesta instancia:
    --          fmap :: (a -> b) -> Handler a -> Handler b
    fmap f (HandlerC h) = HandlerC $ \ req st0 -> do
        ( x, st1 ) <- h req st0
        pure ( f x, st1 )

instance Applicative Handler where
    -- tipus dels metodes en aquesta instancia:
    --          pure  :: a -> Handler a
    --          (<*>) :: Handler (a -> b) -> Handler a -> Handler b
    pure x = HandlerC $ \ _ st0 -> pure ( x, st0 )
    HandlerC hf <*> HandlerC hx = HandlerC $ \ req st0 -> do
        ( f, st1 ) <- hf req st0
        ( x, st2 ) <- hx req st1
        pure ( f x, st2 )

instance Monad Handler where
    -- tipus dels metodes en aquesta instancia:
    --          (>>=) :: Handler a -> (a -> Handler b) -> Handler b
    return = pure
    (>>) = (*>)
    HandlerC hx >>= f = HandlerC $ \ req st0 -> do
        ( x, st1 ) <- hx req st0
        (runHandler $ f x) req st1

-- ****************************************************************

-- Tipus que ha de tenir el resultat del handler que se li passa a 'dispatchHandler'.
data HandlerResponse =
        HRHtml Html             -- Resposta normal. Parametre: Contingut HTML.
      | HRRedirect Text         -- Redireccio. Parametre: URL.
      | HRError Status Text     -- Resposta anormal. Parametres: Codi d'estat HTTP i missatge.

-- 'dispatchHandler' converteix (adapta) un 'Handler' a una aplicacio WAI,
-- realitzant els passos seguents:
--      Obte l'estat inicial (st0) del handler amb una sessio inicial a partir
--        de les cookies rebudes en la peticio WAI.
--      Executa el handler passant-li la peticio i l'estat inicial.
--      Amb l'execucio del handler s'obte el parell format
--        pel resultat del handler (res) i l'estat final (st1).
--      Construeix la corresponent resposta WAI i l'envia.
--        La resposta WAI depen del nou estat de sessio en st1.
-- El tipus 'Application' esta definit en el modul 'Network.Wai' com:
--      type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
dispatchHandler :: Handler HandlerResponse -> Application
dispatchHandler handler req respond = do
    let st0 = HandlerStateC Nothing (U.requestSession req)
    ( res, st1 ) <- runHandler handler req st0
    let newsession = hsSession st1
        wairesp = case res of
            HRHtml html -> U.htmlResponse (runHtmlToText html) newsession
            HRRedirect url -> U.redirectResponse url newsession
            HRError status msg -> U.errorResponse status msg
    respond wairesp

-- ****************************************************************

-- Obte el metode HTTP de la peticio
getMethod :: Handler Method
getMethod = HandlerC $ \ req st0 ->
    pure ( requestMethod req, st0 )

-- Obte el valor de l'atribut de sessio amb el nom indicat.
-- Retorna Nothing si l'atribut indicat no existeix o no te la sintaxis adequada.
getSession :: Read a => Text -> Handler (Maybe a)
getSession name = (>>= readt) <$> getSession_ name

-- Obte el valor de l'atribut de sessio amb el nom indicat.
-- Retorna Nothing si l'atribut indicat no existeix.
getSession_ :: Text -> Handler (Maybe Text)
getSession_ name = HandlerC $ \ req st0 ->
    pure ( lookup name $ hsSession st0, st0 )

-- Fixa l'atribut de sessio amb el nom i valor indicats.
setSession :: Show a => Text -> a -> Handler ()
setSession name value = setSession_ name (showt value)

-- Fixa l'atribut de sessio amb el nom i valor indicats.
setSession_ :: Text -> Text -> Handler ()
setSession_ name value = HandlerC $ \ req st0 -> do
    let newsession = (name, value) : filter ((name /=) . fst) (hsSession st0)
    pure ( (), hsSetSession newsession st0 )

-- Obte els parametres del contingut de la peticio.
getPostQuery :: Handler Query
getPostQuery = HandlerC $ \ req st0 -> do
    -- Si previament ja s'havien obtingut els parametres (i guardats en l'estat del handler)
    -- aleshores es retornen aquests, evitant tornar a llegir el contingut de la peticio
    -- (veieu el comentari de 'requestGetPostQuery' en el modul 'WaiUtils').
    case hsQuery st0 of
        Just query ->
            pure ( query, st0 )
        Nothing -> do
            query <- U.requestGetPostQuery req
            pure ( query, hsSetQuery (Just query) st0 )

