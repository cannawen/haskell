import Data.Function
import Control.Applicative
import Control.Monad

type Knight = (Int, Int)
type Direction = (Int, Int)
boardSize = 8

knightDirections :: [Direction]
knightDirections = [ (x, y) | x <- posOrNeg <*> [1,2], y <- posOrNeg <*> [1,2], abs x /= abs y]
    where posOrNeg = [id, negate]

move :: Knight -> [Knight]
move (kx, ky) = do 
    (dx, dy) <- knightDirections
    guard (kx + dx < boardSize && kx + dx >= 0 && ky + dy < boardSize && ky + dy >= 0 )
    return (kx + dx, ky + dy)

main = do
    print knightDirections
    print $ move (5,1)
    print $ move (7,0)
