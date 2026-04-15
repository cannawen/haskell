data Road a = A a | B a | C a deriving (Show)

instance Functor Road where 
    fmap :: (a -> b) -> Road a -> Road b
    fmap f (A n) = A (f n)
    fmap f (B n) = B (f n)
    fmap f (C n) = C (f n)

roadNum (A n)  = n
roadNum (B n)  = n
roadNum (C n)  = n

type Segment = (Road Int, Road Int, Road Int)

type ShortestPathToA = [Road Int]
type ShortestPathToB = [Road Int]

a = map A [50, 5, 40, 10]
b = map B [10, 90, 2, 8]
c = map C [30, 20, 25, 0]

pathLength :: [Road Int] -> Int
pathLength = foldl (\m r -> m + roadNum r) 0

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

segmentTuples :: (ShortestPathToA, ShortestPathToB)
segmentTuples = foldl 
    (\memo segment -> (fastestPathToA memo segment, fastestPathToB memo segment))
    ([], []) (zip3 a b c)

-- 75
-- [B 10,C 30,A 5,C 20,B 2,B 8]
main = do
    print $ pathLength (fst segmentTuples) 
    print $ reverse (snd segmentTuples) 

