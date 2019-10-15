{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

#if defined(WAI_CGI) || defined(WAI_WARP)
#define WAI
#endif

{- |
TODO: Document the API.
-}
module Drawing
    (
    -- * Tipus
      Drawing, Point, Color
    , module Drawing
    -- * Composició
    , (<>)
    -- * Sortida
    , svgOf
#ifdef WAI
    , drawingOf, animationOf, simulationOf, activityOf
#endif
    )
where
import Drawing.Internal
import Drawing.Render
#ifdef WAI
import Drawing.Handler
#endif

import Data.Functor
import Data.Foldable
import Data.Semigroup

-- * Figures bàsiques

blank :: Drawing
blank = mempty

polyline :: forall f. Foldable f => f Point -> Drawing
polyline ps = Outline (Path False $ toList ps) "black"

polygon :: forall f. Foldable f => f Point -> Drawing
polygon ps = Outline (Path True $ toList ps) "black"

solidPolygon :: forall f. Foldable f => f Point -> Drawing
solidPolygon ps = Fill (Path True $ toList ps) "black"

circle :: Double -> Drawing
circle r = Outline (Ellipse r r) "black"

solidCircle :: Double -> Drawing
solidCircle r = Fill (Ellipse r r) "black"

rectangle :: Double -> Double -> Drawing
rectangle w h = Outline (Rect {width = w, height = h }) "black"

solidRectangle :: Double -> Double -> Drawing
solidRectangle w h = Fill (Rect {width = w, height = h }) "black"


text :: String -> Drawing
text s = Fill (Text s) "black"


coordinatePlane :: Drawing
coordinatePlane = CoordinatePlane


-- * Modificació de figures

colored :: Color -> Drawing -> Drawing
colored c (Fill sh _) = Fill sh c
colored c (Outline sh _) = Outline sh c
colored c (Transformed tr drw) = Transformed tr (colored c drw)
colored c (Many drws) = Many (colored c <$> drws)


translated :: Double -> Double -> Drawing -> Drawing
translated dx dy = transform (translation dx dy)

scaled :: Double -> Double -> Drawing -> Drawing
scaled sx sy = transform (scaling sx sy)

dilated :: Double -> Drawing -> Drawing
dilated s = transform (scaling s s)

rotated :: Double -> Drawing -> Drawing
rotated a = transform (rotation a)

xSkewed :: Double -> Drawing -> Drawing
xSkewed a = transform (skewX a)

ySkewed :: Double -> Drawing -> Drawing
ySkewed a = transform (skewY a)


-- * Colors

white, black, gray, red, green, blue, yellow, brown :: Color
white = "white"
black = "black"
gray = "gray"
red = "red"
green = "green"
blue = "blue"
yellow = "yellow"
brown = "brown"


