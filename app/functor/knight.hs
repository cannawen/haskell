import Data.Function
import Control.Applicative
import Control.Monad

type Knight = (Int, Int)
type Direction = (Int, Int)
boardSize = 8

knightDirections :: [Direction]
knightDirections = [ (x, y) | x <- posOrNeg <*> [2,5], y <- posOrNeg <*> [2,5], abs x /= abs y]
    where posOrNeg = [id, negate]

move :: Knight -> Direction -> Maybe Knight
move (kx, ky) (dx, dy) =
    if  kx + dx >= boardSize || kx + dx < 0 || ky + dy >= boardSize || ky + dy < 0 
        then Nothing
        else Just (kx + dx, ky + dy)

moveKnight = map (\d -> move (6,1) d) knightDirections 

main = do
    print knightDirections
    print $ moveKnight
