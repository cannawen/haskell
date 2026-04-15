import Data.Function ((&))
import System.IO
import System.Environment
import Text.Read
import Text.Read.Lex (Number)

data Road = A Int | B Int | C Int deriving (Show)

roadNum (A n)  = n
roadNum (B n)  = n
roadNum (C n)  = n

instance Eq Road where
    (==) r1 r2 = roadNum r1 == roadNum r2

instance Ord Road where
    compare r1 r2 = compare (roadNum r1) (roadNum r2)

type Segment = (Road, Road, Road)

type ShortestPathToA = [Road]
type ShortestPathToB = [Road]

a = map A [50, 5, 40, 10]
b = map B [10, 90, 2, 8]
c = map C [30, 20, 25, 0]

segments :: [Road] -> [Road] -> [Road] -> [Segment]
segments = zip3

pathLength :: [Road] -> Int
pathLength roads = foldl (\m r -> m + roadNum r) 0 roads

fastestPathToA :: (ShortestPathToA, ShortestPathToB) -> Segment -> ShortestPathToA
fastestPathToA path (a, b, c) = 
    if roadNum a + pathLength (fst path) < roadNum b + roadNum c + pathLength (snd path)
        then a : fst path
        else c : b : snd path

fastestPathToB :: (ShortestPathToA, ShortestPathToB) -> Segment -> ShortestPathToB
fastestPathToB path (a, b, c) = 
    if roadNum b + pathLength (snd path) < roadNum a + roadNum c + pathLength (fst path)
        then b : snd path
        else c : a : fst path

--  B 10,  C 30,  A 5,  C 20,  B2,  B 8
segmentTuples = foldl 
    (\memo segment -> (fastestPathToA memo segment, fastestPathToB memo segment))
    ([A 0], [B 0]) (segments a b c)

main = do
    print $ reverse (fst segmentTuples) 
    print $ reverse (snd segmentTuples) 

