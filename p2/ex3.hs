import Drawing

lights = [ (-2.5, green), (0, yellow), (2.5, red) ]
lightBulb (y, c) = colored c $ translated 0 y $ solidCircle 1

frame = rectangle 2.5 7.5 <> (colored gray $ solidRectangle 2.5 7.5)
trafficLight = dmap lightBulb lights <> frame

myDrawing :: Drawing
myDrawing = col [-8, 0, 8] $ row [-3, 0, 3] $ trafficLight

main :: IO ()
main = svgOf myDrawing

-- Utils

stack :: Foldable f => f Drawing -> Drawing
stack = foldr (<>) blank

dmap :: (x -> Drawing) -> [x] -> Drawing
dmap f l = stack $ f <$> l

row xs fig = dmap (\x -> translated x 0 fig) xs
col ys fig = dmap (\y -> translated 0 y fig) ys
