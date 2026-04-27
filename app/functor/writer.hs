import Data.Monoid
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
