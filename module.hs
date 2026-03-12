import Data.List (words, sort, group)
import Data.Function ((&))

w x = x & words & sort & group & map (\arr -> (head arr, length arr))
w' = map (\arr -> (head arr, length arr)) . group . sort . words

