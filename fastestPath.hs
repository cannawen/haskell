import Data.Function ((&))
import System.IO
import System.Environment
import Text.Read
import Text.Read.Lex (Number)

data Road = A Int | B Int | C Int | AC Int | BC Int | Invalid deriving (Show)

roadNum (A n)  = Just n
roadNum (B n)  = Just n
roadNum (C n)  = Just n
roadNum (AC n) = Just n
roadNum (BC n) = Just n
roadNum Invalid = Nothing

instance Num Road where
    (+) (A a) (C c) = AC (a+c)
    (+) (B b) (C c) = BC (b+c)
    (+) _ _ = Invalid
    (-) _ _ = Invalid
    (*) _ _ = Invalid
    negate _ = Invalid
    abs _ = Invalid
    signum _ = Invalid
    fromInteger _ = Invalid

instance Eq Road where
    (==) r1 r2 = roadNum r1 == roadNum r2


instance Ord Road where
    compare r1 r2 = compare (roadNum r1) (roadNum r2)

type Segment = (Road, Road, Road)

a = map A [50, 5, 40, 10]
b = map B [10, 90, 2, 8]
c = map C [30, 20, 25, 0]

segments :: [Road] -> [Road] -> [Road] -> [Segment]
segments = zip3

fastestPathToA :: Segment -> Road
fastestPathToA (a, b, c) = min a (b + c)

fastestPathToB :: Segment -> Road
fastestPathToB (a, b, c) = min b (a + c)

segmentTuples = foldl (\memo segment ->  (fastestPathToA segment, fastestPathToB segment) : memo) [] (segments a b c)

main = do
    print $ segmentTuples

