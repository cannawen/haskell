import Data.Monoid
import qualified Data.Foldable as F  

lengthCompareDumb x y = 
    let a = length x `compare` length y
        b = x `compare` y
    in if a == EQ then b else a

lengthCompareMonoid x y =
    (length x `compare` length y) `mappend` (x `compare` y)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = 
        foldMap f l `mappend`
        f x `mappend`
        foldMap f r

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )  

main :: IO ()
main = print $ "hello"
