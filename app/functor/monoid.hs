import qualified Data.Foldable as F  
import Data.Monoid

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

testTree :: Tree String 
testTree = Node "12345"
            (Node "123" 
                (Node "1" Empty Empty)  
                (Node "123456" Empty Empty)  
            )  
            (Node "123456789"  
                (Node "12345678" Empty Empty)  
                (Node "1234567890" Empty Empty)  
            )

main :: IO ()
main = do 
    print $ F.foldl (++) "" testTree
    print $ F.foldl (\memo _ -> memo + 1) 0 (F.foldl (++) "" testTree)
    print $ F.foldl (\memo str -> memo + length str) 0 testTree
