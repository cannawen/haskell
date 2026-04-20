import Data.Function
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

landBirds =
    return (0,0)
    >>= landLeft 1 
    >>= landRight 1
    >>= banana
    >>= landRight 1

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
    print maybeAdd0
    print maybeAdd1
    print maybeAdd2
    print maybeAdd3
    print maybeAdd4
    print maybeAdd5

main = maybeAdd