import Drawing

lights = [ (-2.5, green), (0, yellow), (2.5, red) ]
lightBulb (y, c) = colored c $ translated 0 y $ solidCircle 1

frame = rectangle 2.5 7.5 <> (colored gray $ solidRectangle 2.5 7.5)
trafficLight = dmap lightBulb lights <> frame

-- Array drawing

light :: Int -> Int -> Drawing
light r c = translated (3 * fromIntegral c - 6) (8 * fromIntegral r - 16) trafficLight

lightRow :: Int -> Drawing
lightRow r = repeatDraw (light r) 3

myDrawing :: Drawing
myDrawing = repeatDraw lightRow 3

main :: IO ()
main = svgOf myDrawing

-- Utils

stack :: Foldable f => f Drawing -> Drawing
stack = foldr (<>) blank

dmap :: (x -> Drawing) -> [x] -> Drawing
dmap f l = stack $ f <$> l

repeatDraw :: (Int -> Drawing) -> Int -> Drawing
repeatDraw thing n = dmap thing [1..n]
