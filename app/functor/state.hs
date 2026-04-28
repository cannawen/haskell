import Control.Monad.State
import System.Random
import Control.Monad.Writer
import Control.Monad (filterM)


type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)

push' :: Int -> Stack -> ((), Stack)
push' a xs = ((), a:xs)

stackManip' stack = let
    ((), newStack1) = push' 3 stack
    (a, newStack2) = pop' newStack1
    in pop' newStack2

-- stackManip' [5,8,2,1]

pop :: State Stack Int
pop = state (\(x:xs) -> (x, xs))

push :: Int -> State Stack ()
push a = state (\xs -> ((), a:xs))

stackManip = do
    push 3
    pop
    pop

-- runState stackManip [5,8,2,1]

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

pop2 :: State Stack Int
pop2 = do
    xs <- get
    put (tail xs)
    return (head xs)

push2 :: Int -> State Stack ()
push2 x = do 
    xs <- get
    put (x:xs)

-- runState stackyStack [1,2,3]   -- => ((), [8,3,1])
-- runState stackyStack [5,8,2]   -- => ((), [9,2,1])
-- runState (do { push2 7; pop2 }) [5,8,2]
-- runState (push2 7) [5,8,2]   -- => ((), [7,5,8,2])
-- runState pop2 [5,8,2]   -- => (5, [8,2])

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

x = filter (< 4) [9, 1, 5, 2, 10, 3]

keepSmall :: Int -> Writer [String] Bool
keepSmall y
    | y < 4 = do 
        tell ["Keeping " ++ show y]
        return True
    | otherwise = do 
        tell [show y ++ " is too large"]
        return False

z = runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3]

powerset xs = filterM (\x -> [True, False]) xs
