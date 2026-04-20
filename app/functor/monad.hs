import Data.Function
type Bird = Int
type Pole = (Int, Int)

landLeft :: Bird -> Pole -> Maybe Pole
landLeft n (l, r) = createMaybePole (n + l, r)

landRight :: Bird -> Pole -> Maybe Pole
landRight n (l, r) = createMaybePole (l, n + r)

createMaybePole :: Pole -> Maybe Pole
createMaybePole (l, r) = if abs (l - r) < 4 then Just (l, r) else Nothing


main = do 
    landLeft 1 (0,0)
    >>= landRight 1
    >>= landRight 5
    & print
