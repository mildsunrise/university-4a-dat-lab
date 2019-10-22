import Drawing

lights = [ (-2.5, green), (0, yellow), (2.5, red) ]
lightBulb (y, c) = colored c $ translated 0 y $ solidCircle 1

frame = rectangle 2.5 7.5 <> (colored gray $ solidRectangle 2.5 7.5)
trafficLight = foldMap lightBulb lights <> frame

-- Array drawing

trafficLights :: [(Double, Double)] -> Drawing
trafficLights = foldMap (\(x,y) -> translated x y trafficLight)

points = concat $ for [-8, 0, 8] $ \r -> for [-3, 0, 3] $ \c -> (c, r)

myDrawing :: Drawing
myDrawing = trafficLights points

main :: IO ()
main = svgOf myDrawing

-- Utils

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap
