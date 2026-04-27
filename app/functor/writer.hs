import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (a, log) f = (b, log `mappend` newLog)
    where (b, newLog) = f a

-- (3, "smallish gang.") `applyLog` isBigGang
-- (False,"smallish gang.Compared gang size to 9")

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whisky", Sum 99)
addDrink _ = ("beer", Sum 30)

-- ("beans", Sum 10) `applyLog` addDrink
-- ("milk",Sum {getSum = 35})
-- ("jerky", Sum 25) `applyLog` addDrink
-- ("whisky",Sum {getSum = 124})

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number : " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

-- multWithLog 
-- WriterT (Identity (15,["Got number : 3","Got number : 5","Gonna multiply these two"]))

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with  " ++ show a]
        return a
    -- | b == 0 = writer (a, ["Finished with  " ++ show a])
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

addStuff' = (+) <$> (*2) <*> (+10)

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

addStuff'' :: Int -> Int
addStuff'' x = let
    a = (*2) x
    b = (+10) x
    in a+b
