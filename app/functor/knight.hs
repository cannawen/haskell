import Data.Function
import Control.Applicative
import Control.Monad

type Knight = (Int, Int)
type Direction = (Int, Int)
boardSize = 8

knightDirections :: [Direction]
knightDirections = [ (x, y) | 
    x <- [id, negate] <*> [1,2], 
    y <- [id, negate] <*> [1,2], 
    abs x /= abs y ]

moveA (kx, ky) = do 
    (dx, dy) <- knightDirections
    guard (kx + dx < boardSize && kx + dx >= 0 && ky + dy < boardSize && ky + dy >= 0 )
    return (kx + dx, ky + dy)
moveB (kx, ky) = [ (kx + dx, ky + dy) | 
    (dx, dy) <- knightDirections, 
    kx + dx < boardSize, 
    kx + dx >= 0, 
    ky + dy < boardSize, 
    ky + dy >= 0]

move :: Knight -> [Knight]
move = moveB

inThreeMovesA start = do 
    oneMove <- move start
    twoMove <- move oneMove
    move twoMove
inThreeMovesB start = return start >>= move >>= move >>= move
inThreeMovesC start = move start >>= move >>= move

inThreeMoves :: Knight -> [Knight]
inThreeMoves = inThreeMovesC

canReachIn3 :: Knight -> Knight -> Bool
canReachIn3 start end = end `elem` inThreeMoves start

main = do
    print $ canReachIn3 (5,1) (5,0)
    print $ canReachIn3 (5,1) (6,2)
