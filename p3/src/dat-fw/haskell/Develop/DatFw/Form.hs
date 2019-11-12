
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Develop.DatFw.Form
    (-- * Form construction
      AForm
    , FieldSettings(..), withPlaceholder
    , freq, fopt, liftToAForm
    -- * Form processing
    , generateAFormPost, runAFormPost
    , FormResult(..)
    )
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Internal.Handler
import Develop.DatFw.Widget
import Develop.DatFw.Handler
import Develop.DatFw.Template
import Develop.DatFw.Form.Fields

import           Network.Wai

import           Data.Monoid
import           Data.Maybe
import           Data.String
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import           Text.Blaze.Html

--------------------------------------------------------

type FEnv = Maybe (M.Map Text [Text])

data AForm m a = AForm
        { unAForm :: FEnv -> Int -> m (FormResult a, [FormView (HandlerSite m)], Int) }

instance Monad m => Functor (AForm m) where
    fmap f (AForm x) = AForm $ \ env s -> do
        (r', v', s') <- x env s
        pure (fmap f r', v', s')

instance Monad m => Applicative (AForm m) where
    pure x = AForm $ \ env s ->
        pure (FormSuccess x, [], s)
    AForm f <*> AForm x = AForm $ \ env s -> do
        (r', v', s') <- f env s
        (r'', v'', s'') <- x env s'
        pure (r' <*> r'', v' <> v'', s'')

--------------------------------------------------------

data FieldSettings = FieldSettings
        { fsLabel :: Text
        , fsTooltip :: Maybe Text
        , fsId :: Maybe Text
        , fsName :: Maybe Text
        , fsAttrs :: [(Text, Text)]
        }

instance IsString FieldSettings where
    fromString label = FieldSettings (T.pack label) Nothing Nothing Nothing []

-- | Add a placeholder attribute to a field.
withPlaceholder :: Text -> FieldSettings -> FieldSettings
withPlaceholder ph fs =
    fs{ fsAttrs = ("placeholder", ph) : fsAttrs fs }

--------------------------------------------------------

liftToAForm :: Monad m => m a -> AForm m a
liftToAForm handler = AForm $ \ mbenv s -> do
    r <- maybe (pure FormMissing) (const (FormSuccess <$> handler)) mbenv
    pure (r, [], s)

freq :: MonadHandler m => Field m a -> FieldSettings -> Maybe a -> AForm m a
freq field fs def =
    fhelper field fs def (FormFailure ["Value is required"]) FormSuccess True

fopt :: MonadHandler m => Field m a -> FieldSettings -> Maybe a -> AForm m (Maybe a)
fopt field fs def =
    fhelper field fs def (FormSuccess Nothing) (FormSuccess . Just) False

newFormIdent :: Int -> (Text, Int)
newFormIdent s =
    let s' = s + 1
    in (T.pack ('f' : show s'), s')

fhelper :: MonadHandler m => Field m a -> FieldSettings -> Maybe a -> FormResult b -> (a -> FormResult b) -> Bool -> AForm m b
fhelper field fs def onMissing onFound isReq = AForm $ \ mbenv s -> do
    i <- maybe newIdent pure $ fsId fs
    let (name, s') = maybe (newFormIdent s) (\ n -> (n, s)) $ fsName fs
    (res, val) <- case mbenv of
        Nothing -> pure (FormMissing, maybe (Left "") Right def)
        Just env -> do
            let mvals = maybe [] id $ M.lookup name env
            emx <- fldParse field mvals
            case emx of
                Left inv -> pure ( FormFailure [inv], Left $ if null mvals then "" else head mvals )
                Right Nothing -> pure (onMissing, Left "")
                Right (Just x) -> pure (onFound x, Right x)
    let verror = case res of
            FormFailure (msg : _) -> Just (text msg)
            _ -> Nothing
    pure ( res
         , [FormView{ fvLabel = text $ fsLabel fs, fvTooltip = text <$> fsTooltip fs
                    , fvId = i, fvInput = fldView field i name (fsAttrs fs) val isReq
                    , fvErrors = verror, fvRequired = isReq }]
         , s'
         )

--------------------------------------------------------

runAFormPost :: MonadHandler m => AForm m a -> m (FormResult a, Widget (HandlerSite m))
runAFormPost form =
    runFormPost $ renderForm form

-- | Similar to 'runAFormPost', except it always ignores the currently available
-- environment. This is necessary in cases like a wizard UI, where a single
-- page will both receive and incoming form and produce a new, blank form. For
-- general usage, you can stick with 'runAFormPost'.
generateAFormPost :: MonadHandler m => AForm m a -> m (Widget (HandlerSite m))
generateAFormPost form =
    generateFormPost $ renderForm form

type Form m a = FEnv -> Int -> m (a, Int)

-- Render a Bootstrap v3 Basic Form
renderForm :: Monad m => AForm m a -> Html -> Form m (FormResult a, Widget (HandlerSite m))
renderForm (AForm form) fragment env s = do
    (formr, formvs, s') <- form env s
    let hasErrors view = maybe False (const True) $ fvErrors view
        w = [widgetTempl|
#{fragment}
$forall{ view <- formvs }
  <div class="form-group $if{fvRequired view} required $else optional$end $if{hasErrors view}has-error$end">
    <label for="#{fvId view}">#{fvLabel view}:</label>
    ^{fvInput view}
    $maybe{ tt <- fvTooltip view }<span class="help-block">#{tt}</span>$end
    $maybe{ err <- fvErrors view }<span class="help-block error-block">#{err}</span>$end
  </div>
$end
            |]
    pure ((formr, w), s')

runFormPost :: MonadHandler m => (Html -> Form m (FormResult a, markup)) -> m (FormResult a, markup)
runFormPost form = do
    env <- postEnv
    postHelper form env

generateFormPost :: MonadHandler m => (Html -> Form m (FormResult a, markup)) -> m markup
generateFormPost form =
    snd <$> postHelper form Nothing

postHelper :: Monad m => (Html -> Form m (FormResult a, markup)) -> FEnv -> m (FormResult a, markup)
postHelper form env = do
    --TODO: Insert CSRF protection code
    let extra = ""
    ((r, w), _) <- form extra env 0
    pure (r, w)

postEnv :: MonadHandler m => m FEnv
postEnv = do
    req <- getRequest
    if requestMethod req == "GET" then pure Nothing
    else do
        let f (n, Nothing) = id
            f (n, Just v)  = M.insertWith' (++) (T.decodeUtf8 n) [T.decodeUtf8 v]
        q <- lookupPostQuery
        pure $ Just $ foldr f M.empty q


--------------------------------------------------------

-- | A form can produce three different results: there was no data available,
-- the data was invalid, or there was a successful parse.
--
-- The 'Applicative' instance will concatenate the failure messages in two
-- 'FormResult's.
-- The 'Alternative' instance will choose 'FormFailure' before 'FormSuccess',
-- and 'FormMissing' last of all.
data FormResult a = FormMissing | FormFailure [Text] | FormSuccess a

instance Functor FormResult where
    fmap f (FormSuccess x) = FormSuccess $ f x
    fmap f (FormFailure e) = FormFailure e
    fmap f FormMissing     = FormMissing

instance Applicative FormResult where
    pure x = FormSuccess x
    FormSuccess f  <*> FormSuccess x  = FormSuccess $ f x
    FormFailure e1 <*> FormFailure e2 = FormFailure $ e1 <> e2
    FormFailure e  <*> _              = FormFailure e
    _              <*> FormFailure e  = FormFailure e
    _              <*> _              = FormMissing


--------------------------------------------------------

data FormView site = FormView
        { fvLabel :: Html
        , fvTooltip :: Maybe Html
        , fvId :: Text
        , fvInput :: Widget site
        , fvErrors :: Maybe Html
        , fvRequired :: Bool
        }

