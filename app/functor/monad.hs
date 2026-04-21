import Data.Function
import Control.Applicative
type Bird = Int
type Pole = (Int, Int)

landLeft :: Bird -> Pole -> Maybe Pole
landLeft n (l, r) = createMaybePole (n + l, r)

landRight :: Bird -> Pole -> Maybe Pole
landRight n (l, r) = createMaybePole (l, n + r)

createMaybePole :: Pole -> Maybe Pole
createMaybePole (l, r) = if abs (l - r) < 4 then Just (l, r) else Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

landBirds0 =
    return (0,0)
    >>= landLeft 1 
    >>= landRight 1
    >>= banana
    >>= landRight 1

landBirds1 = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    Nothing -- whoa, this is chained
    landLeft 1 second

maybeAdd0 = (+) <$> Just 3 <*> Just 4

maybeAdd1 = (pure (+)) <*> Just 3 <*> Just 4

maybeAdd2 = liftA2 (+) (Just 3) (Just 4)

maybeAdd3 = Just 3 >>= (\x -> Just 4 >>= (\y -> Just (x + y)))

maybeAdd4 = 
    Just 3 >>= (\x -> 
    Just 4 >>= (\y -> 
    Just (x + y)))

maybeAdd5 = do
    x <- Just 3
    y <- Just 4
    Just (x + y)

maybeAdd = do
    mapM_ print [maybeAdd0, maybeAdd1, maybeAdd2, maybeAdd3, maybeAdd4, maybeAdd5]

zip0 = (+) <$> [1, 2] <*> [10, 20]

zip1 = [(+)] <*> [1, 2] <*> [10, 20]

zip2 = liftA2 (+)  [1, 2]  [10, 20]

zipThree = [1, 2] >>= (\x -> [10, 20] >>= (\y -> [x + y]))

zip4 = 
    [1, 2] >>= (\x -> 
    [10, 20] >>= (\y -> 
    [x + y]))

zip5 = do 
    x <- [1, 2]
    y <- [10, 20]
    [x + y]

zipL = do 
    mapM_ print [zip0, zip1, zip2, zipThree, zip4, zip5]

main = zipL
