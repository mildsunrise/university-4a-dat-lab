
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Develop.DatFw.Widget
    (-- * Widget types
      WidgetFor, Widget
    -- * Conversion
    , ToWidget(..), setTitle
    -- * Page construction from Widget
    , PageContent(..), widgetToPageContent
    )
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Handler

import           Data.Monoid
import           Data.IORef
import           Data.Text as T
import           Text.Blaze.Html

------------------- Widget conversion ------------------------

class ToWidget site a where
    toWidget :: a -> Widget site

{---
public static interface ToWidgetHead<site,a> {
    public abstract Widget<site> toWidgetHead(a x);
}

public static interface ToWidgetBody<site,a> {
    public abstract Widget<site> toWidgetBody(a x);
}
---}


setTitle :: Html -> Widget site
setTitle html = WidgetFor $ \ WidgetData{ wdStateRef = ref } -> do
    modifyIORef ref $ \ ws -> ws{ wsTitle = Just html }


------------------- ToWidget instances ----------------------------

-- instances of '(Route site -> [(Text, Text)] -> Text) -> Html'

---instance ToWidget site (HtmlUrl (Route site)) where
---instance url ~ Route site => ToWidget site (HtmlUrl url) where
instance urender ~ (Route site -> [(Text, Text)] -> Text) => ToWidget site (urender -> Html) where
    toWidget htmlf = WidgetFor $ \ WidgetData{ wdStateRef = ref } -> do
        modifyIORef ref $ \ ws -> ws{ wsBody = wsBody ws <> htmlf }

{---
--  instance ToWidgetHead<site, (Route site -> [(Text, Text)] -> Text) -> Html>
public static <site> ToWidgetHead<site, Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iToWidgetHeadHtmlUrl() {
    return (htmlf) -> (wdata) -> { iToWidgetHeadHtmlUrl(htmlf, wdata); return null; };
}
public static <site> void iToWidgetHeadHtmlUrl(Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html> htmlf, WidgetData<site> wdata) {
    Monoid<Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iMonoid = iMonoidFun(iMonoidHtml);
    WidgetState<Route<site>> olds = wdata.getState();
    wdata.putState(WidgetState(olds.title, iMonoid.mappend(olds.head, htmlf), olds.body));
}

--  instance ToWidgetBody<site, (Route site -> [(Text, Text)] -> Text) -> Html>
public static <site> ToWidgetBody<site, Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iToWidgetBodyHtmlUrl() {
    return (htmlf) -> (wdata) -> { iToWidgetBodyHtmlUrl(htmlf, wdata); return null; };
}
public static <site> void iToWidgetBodyHtmlUrl(Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html> htmlf, WidgetData<site> wdata) {
    iToWidgetHtmlUrl(htmlf, wdata);
}
---}

-- instances of 'Html'

instance ToWidget site Html where
    toWidget html = toWidget (const html :: UrlRender (Route site) -> Html)

{---
--  instance ToWidgetHead<site, Html>
public static  <site> ToWidgetHead<site,Html> iToWidgetHeadHtml() {
    ToWidgetHead<site,Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iTWHHtmlUrl = iToWidgetHeadHtmlUrl();
    return (html) -> iTWHHtmlUrl.toWidgetHead(_f -> html);
}

--  instance ToWidgetBody<site, Html>
public static <site> ToWidgetBody<site,Html> iToWidgetBodyHtml() {
    ToWidgetBody<site,Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iTWBHtmlUrl = iToWidgetBodyHtmlUrl();
    return (html) -> iTWBHtmlUrl.toWidgetBody(_f -> html);
}
---}


-------------------------------------------------------------------------------
-- Page construction from Widget

data PageContent route = PageContent
        { pcTitle :: Html
        , pcHead :: UrlRender route -> Html
        , pcBody :: UrlRender route -> Html
        }

widgetToPageContent :: Widget site -> HandlerFor site (PageContent (Route site))
widgetToPageContent widget = HandlerFor $ \ hdata -> do
    ref <- newIORef mempty
    unWidgetFor widget WidgetData{ wdStateRef = ref, wdHData = hdata}
    ws <- readIORef ref
    pure $ PageContent (maybe mempty id $ wsTitle ws) (wsHead ws) (wsBody ws)

