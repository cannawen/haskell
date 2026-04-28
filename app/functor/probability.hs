import Data.Ratio
import Control.Monad
import Data.List (all)

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving Show

x = [(3, 1%2), (5, 1%4), (9, 1%4)]

instance Functor Prob where
    fmap f (Prob xs) = Prob [(f x, p) | (x, p) <- xs]
    
instance Applicative Prob where
  pure x = Prob [(x, 1)]
  (<*>) = ap

instance Monad Prob where
    return = pure
    (Prob xs) >>= f = 
        flatten $ Prob [(f x, p) | (x, p) <- xs]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob [(x, p*p') | (Prob inner, p) <- xs, (x, p') <- inner]

coin = Prob [('H', 1 % 2), ('T', 1 % 2)]

twoCoins = do
    c1 <- coin
    c2 <- coin
    return (c1, c2)

data Coin = Heads | Tails deriving (Show, Eq)

coin' :: Prob Coin
coin' = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin'
    b <- coin'
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

main = do 
    print flipThree
