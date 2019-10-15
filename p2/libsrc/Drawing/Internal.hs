
module Drawing.Internal where
import Data.Functor
import Data.Foldable
import Data.Semigroup


data Drawing =
      Fill Shape FillStyle
    | Outline Shape OutlineStyle
    | Transformed Transform Drawing
    | Many [Drawing]
    | CoordinatePlane

instance Semigroup Drawing where
    (Many ds1) <> (Many ds2) =
        Many (ds1 <> ds2)
    (Many ds1) <> d2 =
        Many (ds1 <> [d2])
    d1 <> (Many ds2) =
        Many (d1 : ds2)
    d1 <> d2 =
        Many [d1, d2]

instance Monoid Drawing where
    mempty = Many []
    mappend = (<>)


type Point = (Double, Double)

data Shape =
      Rect {width :: Double, height :: Double}
    | Path Bool [Point]
    | Ellipse Double Double
    | Text String

type Color = String

type FillStyle = Color

type OutlineStyle = Color


data Transform = Transform { m11 :: Double, m12 :: Double, m21 :: Double, m22 :: Double, m31 :: Double, m32 :: Double}

instance Semigroup Transform where
    Transform a11 a12 a21 a22 a31 a32 <> Transform b11 b12 b21 b22 b31 b32 =
        {- [a11 a21 a31]   [b11 b21 b31]   [a11*b11+a21*b12 a11*b21+a21*b22 a11*b31+a21*b32+a31]
         - [a12 a22 a32] x [b12 b22 b32] = [a12*b11+a22*b12 a12*b21+a22*b22 a12*b31+a22*b32+a32]
         - [0   0   1  ]   [0   0   1  ]   [0               0               1                  ]
         -}
        Transform { m11 = a11*b11+a21*b12, m21 = a11*b21+a21*b22, m31 = a11*b31+a21*b32+a31,
                    m12 = a12*b11+a22*b12, m22 = a12*b21+a22*b22, m32 = a12*b31+a22*b32+a32 }

instance Monoid Transform where
    mempty =
        Transform { m11 = 1.0, m12 = 0.0, m21 = 0.0, m22 = 1.0, m31 = 0.0, m32 = 0.0 }
    mappend = (<>)

instance Show Transform where
    show (Transform m11 m12 m21 m22 m31 m32) =
        show "Transform{ m11:" <> show m11 <> ", m12:" <> show m12 <> ", m21:" <> show m21 <> ", m22:" <> show m22 <> ", m31:" <> show m31 <> ", m32:" <> show m32 <> " }"

translation dx dy =
    Transform { m11 = 1.0, m12 = 0.0, m21 = 0.0, m22 = 1.0, m31 = dx, m32 = dy }

scaling sx sy =
    Transform { m11 = sx, m12 = 0.0, m21 = 0.0, m22 = sy, m31 = 0.0, m32 = 0.0 }

rotation a =
    Transform { m11 = cos a, m12 = sin a, m21 = -sin a, m22 = cos a, m31 = 0.0, m32 = 0.0 }

{-
[a b c]   [1 t 0]   [a t*a+b c]
[d e f] x [0 1 0] = [d t*d+e f]
[0 0 1]   [0 0 1]   [0 0     1]
-}
skewX a =
    Transform { m11 = 1.0, m12 = tan a, m21 = 0.0, m22 = 1.0, m31 = 0.0, m32 = 0.0 }

{-
[a b c]   [1 0 0]   [a+t*b b c]
[d e f] x [t 1 0] = [d+t*e e f]
[0 0 1]   [0 0 1]   [0     0 1]
-}
skewY a =
    Transform { m11 = 1.0, m12 = 0.0, m21 = tan a, m22 = 1.0, m31 = 0.0, m32 = 0.0 }


transform :: Transform -> Drawing -> Drawing
transform tr (Transformed tr2 dr) = Transformed (tr <> tr2) dr
transform tr dr = Transformed tr dr

transformPoint :: Transform -> Point -> Point
transformPoint (Transform a11 a12 a21 a22 a31 a32) (x, y) =
    (a11*x + a21*y + a31, a12*x + a22*y + a32)

transformDistance :: Transform -> Double -> Double -> (Double, Double)
transformDistance (Transform a11 a12 a21 a22 a31 a32) dx dy =
    (a11*dx + a21*dy, a12*dx + a22*dy)


