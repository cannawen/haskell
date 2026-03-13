import Data.List
import Data.Function ((&))
import Data.Char

w x = x & words & sort & group & map (\arr -> (head arr, length arr))
w' = map (\arr -> (head arr, length arr)) . group . sort . words

find' :: Eq a => [a] -> [a] -> Bool
find' needle haystack = tails haystack
    & foldr (\partialHaystack memo -> memo || length (filter (\(n, h) -> n==h) (zip needle partialHaystack)) == length needle) False

encode :: Int -> String -> String
encode i = map (\input -> ord input + i & chr )
