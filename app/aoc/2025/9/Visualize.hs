module Visualize where

import Data.Function ((&))
import Data.List (intercalate)
import Data.List.Split (splitOn)

data Point = Point { x :: Int, y :: Int }

-- Rendering config
scale, padding :: Int
scale   = 40
padding = 20

-- Map grid coords to SVG pixel coords
toSvg :: Int -> Int -> (Int, Int)
toSvg px py = (px * scale + padding, py * scale + padding)

-- "x,y" string for a single point
svgCoord :: Point -> String
svgCoord (Point px py) =
    let (sx, sy) = toSvg px py
    in show sx ++ "," ++ show sy

-- Overall canvas size
canvasWidth, canvasHeight :: [Point] -> Int
canvasWidth  pts = maximum (map x pts) * scale + 2 * padding
canvasHeight pts = maximum (map y pts) * scale + 2 * padding

-- A dot for each vertex
dot :: Point -> String
dot (Point px py) =
    let (sx, sy) = toSvg px py
    in "  <circle cx=\"" ++ show sx ++ "\" cy=\"" ++ show sy
       ++ "\" r=\"4\" fill=\"red\"/>"

-- Label for each vertex
label :: Point -> String
label (Point px py) =
    let (sx, sy) = toSvg px py
    in "  <text x=\"" ++ show (sx + 5) ++ "\" y=\"" ++ show (sy - 5)
       ++ "\" font-size=\"10\" fill=\"black\">"
       ++ "(" ++ show px ++ "," ++ show py ++ ")</text>"

renderSvg :: [Point] -> String
renderSvg pts = unlines $
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<svg xmlns=\"http://www.w3.org/2000/svg\""
    , "     width=\""  ++ show (canvasWidth  pts) ++ "\""
    , "     height=\"" ++ show (canvasHeight pts) ++ "\">"
    -- Filled polygon (pts ++ [head pts] closes the shape)
    , "  <polygon points=\""
      ++ intercalate " " (map svgCoord (pts ++ [head pts]))
      ++ "\" fill=\"lightblue\" stroke=\"navy\" stroke-width=\"2\"/>"
    ]
    ++ map dot   pts
    ++ map label pts
    ++ ["</svg>"]

parse :: String -> [Point]
parse input =
    lines input
    & map (splitOn ",")
    & map (map read)
    & map (\[px, py] -> Point px py)

main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/9/input-mini.txt"
    let pts = parse contents
    writeFile "app/aoc/2025/9/output.svg" (renderSvg pts)
    putStrLn "Written to app/aoc/2025/9/output.svg"
