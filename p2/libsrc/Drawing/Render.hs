
module Drawing.Render where
import Drawing.Internal
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder as Bd
import Data.Functor
import Data.Semigroup
import Data.Int

svgOf :: Drawing -> IO ()
svgOf =
    renderWith 100 200 (\s m -> putStr s >> m) (putStrLn "") . pprDrawingXml


data Content = Content { cntLength :: !Int64, cntBuilder :: !Bd.Builder }

instance Semigroup Content where
    (Content l1 b1) <> (Content l2 b2) = Content (l1 + l2) (b1 <> b2)

instance Monoid Content where
    mempty = Content 0 mempty
    mappend = (<>)

stringContent :: String -> Content
stringContent s =
    Content (fromIntegral $ length s) (Bd.stringUtf8 s)

lazyByteStringContent :: BL.ByteString -> Content
lazyByteStringContent s =
    Content (BL.length s) (Bd.lazyByteString s)


renderSvg :: Drawing -> Content
renderSvg =
    let addContent s (Content l bd) =
            Content (l + (fromIntegral $ length s)) (Bd.stringUtf8 s <> bd)
    in renderWith 100 200 addContent mempty . pprDrawingHtml

renderWith ::
          Int                  -- Line length
       -> Int                  -- Ribbon width
       -> (String -> a -> a)   -- What to do with text
       -> a                    -- What to do at the end
       -> Doc
       -> a                   -- Result
renderWith w r out end =
    let ribbonsPerLine = fromIntegral r / fromIntegral w
        txtPrinter (Chr c) = out [c]
        txtPrinter (Str s1) = out s1
        txtPrinter (PStr s1) = out s1
    in fullRender PageMode w ribbonsPerLine txtPrinter end

viewMax :: Int
viewMax = 300 -- in pixels
unitSize :: Double
unitSize = 20.0 -- in pixels

pprDrawingXml :: Drawing -> Doc
pprDrawingXml draw = vcat
    [ text "<?xml version='1.0' encoding='UTF-8' standalone='no'?>"
    , text "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
    , text ("<svg width='" <> show (viewMax*2) <> "' height='" <> show (viewMax*2) <> "' viewBox='" <> show (-viewMax) <> " " <> show (-viewMax) <> " " <> show (viewMax*2) <> " " <> show (viewMax*2) <> "'")
    ---, text ("<svg viewBox='" <> show (-viewMax) <> " " <> show (-viewMax) <> " " <> show (viewMax*2) <> " " <> show (viewMax*2) <> "'")
    , text " xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>"
    , nest 2 (pprDrawingCommon draw)
    , text "</svg>"
    ]

pprDrawingHtml :: Drawing -> Doc
pprDrawingHtml draw = vcat
    [ text ("<svg width='" <> show (viewMax*2) <> "' height='" <> show (viewMax*2) <> "' viewBox='" <> show (-viewMax) <> " " <> show (-viewMax) <> " " <> show (viewMax*2) <> " " <> show (viewMax*2) <>
                       "' onclick='clickEvent("<>show viewMax<>","<>show unitSize<>",evt);'>")
    ---[ text ("<svg viewBox='" <> show (-viewMax) <> " " <> show (-viewMax) <> " " <> show (viewMax*2) <> " " <> show (viewMax*2) <> "'>")
    , nest 2 (pprDrawingCommon draw)
    , text "</svg>"
    ]

pprDrawingCommon draw = vcat
    [ text ("<rect vector-effect='non-scaling-stroke' fill='none' stroke='gray' stroke-width='2' x='" <> show (-viewMax) <> "' y='" <> show (-viewMax) <>
                                                           "' width='" <> show (viewMax*2) <> "' height='" <> show (viewMax*2) <> "'/>")
    , pprDrawing draw
    ]

pprDrawing (Fill shape style) =
        pprShape (pprFill style) shape
pprDrawing (Outline shape style) =
        pprShape (pprOutline style) shape
pprDrawing (Many draws) = vcat (reverse (pprDrawing <$> draws))
pprDrawing (Transformed (Transform m11 m12 m21 m22 m31 m32) draw) =
        vcat [ text ("<g transform='matrix(" <> show m11 <> ", " <> show (-m12) <> ", "
                                             <> show (-m21) <> ", " <> show m22 <> ", "
                                             <> show (m31*unitSize) <> ", " <> show (-m32*unitSize) <> ")'>")
             , nest 2 (pprDrawing draw)
             , text "</g>"
             ]
             {--     [ u  0 0 ]      H x Tapp = Tsvg x H
                 H = [ 0 -u 0 ]      Tsvg = H x Tapp x H^(-1)
                     [ 0  0 1 ]
                            [ a11 a21 a31 ]            [ a11*u  a21*u  a31*u  ]            [ a11  -a21 a31*u  ]
                 Tsvg = H x [ a12 a22 a32 ] x H^(-1) = [ -a12*u -a22*u -a32*u ] x H^(-1) = [ -a12 a22  -a32*u ]
                            [ 0   0   1   ]            [ 0      0      1      ]            [ 0    0    1      ]
             --}
pprDrawing CoordinatePlane =
        vcat $
            [ text ("<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='2' x1='-" <> show viewMax <> "' y1='0' x2='" <> show viewMax <> "' y2='0'/>")
            , text ("<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='2' x1='0' y1='-" <> show viewMax <> "' x2='0' y2='" <> show viewMax <> "'/>")
            ] ++ concatMap lines [1 .. round (fromIntegral viewMax / unitSize)]
      where
        lines n =
          let c = fromIntegral n * unitSize in
            [ line (-viewMax) c    viewMax c        -- horitzontal line at y = -n
            , line (-viewMax) (-c) viewMax (-c)     -- horitzontal line at y = n
            , line c (-viewMax)    c viewMax        -- vertical line at x = n
            , line (-c) (-viewMax) (-c) viewMax     -- vertical line at x = -n
            , number "end" 0 c            (-n)
            , number "end" 0 (-c)         n
            , number "middle" c (unitSize*0.5)    n
            , number "middle" (-c) (unitSize*0.5) (-n)
            ]
        line x1 y1 x2 y2 =
            text ("<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='1' x1='" <> show x1 <> "'  y1='" <> show y1 <> "' x2='" <> show x2 <> "' y2='" <> show y2 <> "'/>")
        number anchor x y n =
            text ("<text fill='gray' text-anchor='" <> anchor <> "' transform='translate(" <> show x <> "," <> show y <> ") scale(0.7,0.7)' x='0' y='0'>" <> show n <> "</text>")

pprFill :: Color -> ShowS
pprFill c s =
    " fill='" <> c <> "'" <> s

pprOutline :: Color -> ShowS
pprOutline c s =
    " fill='none' vector-effect='non-scaling-stroke' stroke='" <> c <> "' stroke-width='1'" <> s

pprShape :: ShowS -> Shape -> Doc
pprShape attrs (Rect w h) =
    let x' = -w*unitSize/2 ; y' = -h*unitSize/2; w' = w*unitSize; h' = h*unitSize
    in text $ showString "<rect" $ attrs $ showString " x='" $ shows x' $ showString "' y='" $ shows y' $ showString "' width='" $ shows w' $ showString "' height='" $ shows h' "'/>"
pprShape attrs (Ellipse rx ry) =
    let rx' = rx*unitSize; ry' = ry*unitSize
    in text $ showString "<ellipse" $ attrs $ showString " cx='0' cy='0' rx='" $ shows rx' $ showString "' ry='" $ shows ry' "'/>"
pprShape attrs (Path closed []    ) = mempty
pprShape attrs (Path closed (p:ps)) =
    text $ showString "<path" $ attrs $ showString " d='M" $ point p $ go ps
    where
        point (x, y) s =
            let x' = x*unitSize; y' = -y*unitSize
            in shows x' $ showChar ' ' $ shows y' s
        go (p:ps) = showString " L" $ point p $ go ps
        go []     = if closed then " Z'/>" else "'/>"
pprShape attrs (Text s) =
    text $ showString "<text" $ attrs $ showString " text-anchor='middle' x='0' y='0'>" $ showString (escapeHtml s) "</text>"

escapeHtml :: String -> String
escapeHtml =
    let convert '<'  = "&lt;"
        convert '&'  = "&amp;"
        convert '>'  = "&gt;"
        convert '\"' = "&quot;"
        convert '\'' = "&apos;"
        convert c    = [c]
    in concatMap convert

