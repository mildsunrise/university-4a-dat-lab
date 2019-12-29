
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- @author Jordi Forga
--
module Develop.DatFw.Content
    ( module Develop.DatFw.Content
    , Html
    )
where
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

import           Data.Text(Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.ByteString as B
import           Data.ByteString.Builder

--------------------- Types ------------------------

data Content =
      ContentBuilder
          Builder       -- byte-string builder
          (Maybe Int)   -- content length (possibly unknown)
    --- | ContentSource (OutputStream -> IO ()) -- writeTo

-- | Zero-length content.
emptyContent :: Content
emptyContent = ContentBuilder mempty (Just 0)


data TypedContent = TypedContent ContentType Content

type ContentType = B.ByteString

typePlain, typeHtml, typeJson :: ContentType
typePlain = "text/plain; charset=utf-8"
typeHtml = "text/html; charset=utf-8"
typeJson = "application/json"


--------------------- Type classes ------------------------

class ToContent a where
    toContent :: a -> Content

class ToContent a => ToTypedContent a where
    toTypedContent :: a -> TypedContent


--------------------- ToContent instances ------------------------

instance ToContent Content where
    toContent c = c

instance ToContent TypedContent where
    toContent (TypedContent t c) = c

instance ToContent () where
    toContent () = ContentBuilder mempty (Just 0)

instance ToContent [Char] where
    toContent text =  ContentBuilder (stringUtf8 text) Nothing

instance ToContent Text where
    toContent text =  ContentBuilder (encodeUtf8Builder text) Nothing

instance ToContent Html where
    toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing

{---
public static ToContent<Html> toContentHtml = new ToContent<Html>() {
    @Override public Content apply(final Html html) {
        if (html instanceof Html.PreEscaped) {
            String preesc = ((Html.PreEscaped) html).preEscaped;
            return toContentString.apply(preesc);
        } else if (html instanceof Html.Text) {
            return toContentString.apply(render(html));
        } else {
            return Content.Source(new Comm1<OutputStream,Void>(){
                public Void run(OutputStream out) throws IOException { htmlWriteTo(html, out); return null; }
            });
        }
    }
};
    -- where
    private static void htmlWriteTo(Html html, OutputStream out) throws IOException {
        if (html instanceof Html.PreEscaped) {
            out.write(((Html.PreEscaped) html).preEscaped.getBytes("utf-8"));
        } else if (html instanceof Html.Text) {
            out.write(render(html).getBytes("utf-8"));
        } else {
            htmlWriteTo(((Html.Append) html).left, out);
            htmlWriteTo(((Html.Append) html).right, out);
        }
    }

-- instance ToContent<Json>
public static ToContent<Json> toContentJson = new ToContent<Json>() {
    @Override public Content apply(final Json json) {
        return Content.Source(new Comm1<OutputStream,Void>(){
            public Void run(OutputStream out) throws IOException {
                OutputStreamWriter writer = new OutputStreamWriter(out, "UTF-8");
                writeTo(json, writer);
                writer.flush();
                return null;
            }
        });
    }
};
---}

--------------------- ToTypedContent instances ------------------------

instance ToTypedContent TypedContent where
    toTypedContent tc = tc

instance ToTypedContent () where
    toTypedContent () = TypedContent typePlain (toContent ())

instance ToTypedContent [Char] where
    toTypedContent text = TypedContent typePlain (toContent text)

instance ToTypedContent Text where
    toTypedContent text = TypedContent typePlain (toContent text)

instance ToTypedContent Html where
    toTypedContent html = TypedContent typeHtml (toContent html)

{---
-- instance ToTypedContent<Json>
public static ToTypedContent<Json> toTypedContentJson = new ToTypedContent<Json>() {
    @Override public TypedContent apply(Json json) { return TypedContent(typeJson, toContentJson.apply(json)); }
};
---}

