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

main = do 
    return (0,0)
    >>= landLeft 1 
    >>= landRight 1
    >>= banana
    >>= landRight 1
    & print
