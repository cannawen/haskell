import Data.Function
type Bird = Int
type Pole = (Int, Int)

landLeft :: Bird -> Pole -> Pole
landLeft n (l, r) = (n + l, r)

landRight :: Bird -> Pole -> Pole
landRight n (l, r) = (l, n + r)
