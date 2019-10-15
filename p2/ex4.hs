import Drawing

tree 0 = colored yellow $ solidCircle 0.2
tree n = let
    subtree = dmap (\s -> rotated (s*pi/2/5) $ tree (n-1)) [1, -1]
  in polyline [(0,0), (0,1)] <> translated 0 1 subtree

myDrawing :: Drawing
myDrawing = tree 8

main :: IO ()
main = svgOf myDrawing

-- Utils

stack :: Foldable f => f Drawing -> Drawing
stack = foldr (<>) blank

dmap :: (x -> Drawing) -> [x] -> Drawing
dmap f l = stack $ f <$> l
