
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Develop.DatFw.Form.Fields
    (-- * Field controls
      Field(..)
    , textField, passwordField
    , textareaField
    -- * Validation
    , check, checkMap, checkM, checkMMap
    )
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Widget
import Develop.DatFw.Handler
import Develop.DatFw.Template

import           Data.Monoid
import           Data.Text(Text)
import qualified Text.Blaze.Html as H

data Field m a = Field
        { fldParse :: [Text] -> m (Either Text (Maybe a))
        , fldView :: FieldViewFunc (HandlerSite m) a
        }

type FieldViewFunc site a = Text ->             -- id
                            Text ->             -- name
                            [(Text,Text)] ->    -- attributes
                            Either Text a ->    -- Either (invalid text) or (legitimate result)
                            Bool ->             -- required?
                            Widget site

textField :: Monad m => Field m Text
textField = Field
    { fldParse = parseHelper Right
    , fldView = inputFieldFunc "text" True
    }

passwordField :: Monad m => Field m Text
passwordField = Field
    { fldParse = parseHelper Right
    , fldView = inputFieldFunc "password" False
    }

inputFieldFunc :: Text -> Bool -> FieldViewFunc site Text
inputFieldFunc typ showval i name attrs evalue required = do
        let fattr (n, v) = H.preEscapedText " " <> H.text n <> H.preEscapedText "=\"" <> H.text v <> H.preEscapedText "\""
            htyp = H.preEscapedText typ
            hattrs = mconcat (fattr <$> attrs)
            sval = if showval then either id id evalue else ""
            hreq = if required then H.preEscapedText "required" else mempty
        -- NOTE: Use class 'form-control' for Bootstrap v3
        toWidget [htmlTempl| <input type="#{htyp}" class="form-control" id=#{i} name=#{name} #{hattrs} #{hreq} value="#{sval}"> |]

-- | Creates a @\<textarea>@ tag whose returned value is wrapped in a 'Textarea'; see 'Textarea' for details.
textareaField :: Monad m => Field m Text
textareaField = Field
    { fldParse = parseHelper Right
    , fldView = \ i name attrs evalue required -> do
        let fattr (n, v) = H.preEscapedText " " <> H.text n <> H.preEscapedText "=\"" <> H.text v <> H.preEscapedText "\""
            hattrs = mconcat (fattr <$> attrs)
            sval = either id id evalue
            hreq = if required then H.preEscapedText "required" else mempty
        -- NOTE: Use class 'form-control' for Bootstrap v3
        toWidget [htmlTempl| <textarea class="form-control" id=#{i} name=#{name} #{hattrs} #{hreq}>#{sval}</textarea> |]
    }

parseHelper :: Monad m => (Text -> Either Text a) -> [Text] -> m (Either Text (Maybe a))
parseHelper f values = do
    case values of
        []     -> pure $ Right Nothing
        "" : _ -> pure $ Right Nothing
        v  : _ -> pure $ Just <$> f v


--------------------------------------------------------
-- Validation

check :: Monad m => (a -> Either Text a) -> Field m a -> Field m a
check checkf field =
    checkM (pure . checkf) field

-- | Same as check, but modifies the datatype.
--
-- In order to make this work, you must provide a function to convert back from
-- the new datatype to the old one (the second argument to this function).
checkMap :: Monad m => (a -> Either Text b) -> (b -> a) -> Field m a -> Field m b
checkMap checkf backf field =
    checkMMap (pure . checkf) backf field

checkM :: Monad m => (a -> m (Either Text a)) -> Field m a -> Field m a
checkM checkf field =
    checkMMap checkf id field

-- | Same as checkM, but modifies the datatype.
--
-- In order to make this work, you must provide a function to convert back from
-- the new datatype to the old one (the second argument to this function).
checkMMap :: Monad m => (a -> m (Either Text b)) -> (b -> a) -> Field m a -> Field m b
checkMMap checkf backf field =
    let parse values = do
            e1 <- fldParse field values
            case e1 of
                Left t -> pure (Left t)
                Right Nothing -> pure (Right Nothing)
                Right (Just x) -> fmap Just <$> checkf x
        view i n as evalue req = fldView field i n as (backf <$> evalue) req
    in Field parse view

