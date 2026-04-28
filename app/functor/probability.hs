{-# LANGUAGE InstanceSigs #-}
import Data.Ratio
import Control.Monad

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving Show

x = [(3, 1%2), (5, 1%4), (9, 1%4)]

instance Functor Prob where
    fmap f (Prob xs) = Prob [(f x, p) | (x, p) <- xs]
    
instance Applicative Prob where
  pure x = Prob [(x, 1)]
  (<*>) = ap

instance Monad Prob where
    return = pure
    (>>=) :: Prob a -> (a -> Prob b) -> Prob b
    (Prob xs) >>= f = 
        flatten $ Prob [(f x, p) | (x, p) <- xs]


flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob [(x, p*p') | (Prob inner, p) <- xs, (x, p') <- inner]

coin = Prob [('H', 1 % 2), ('T', 1 % 2)]

twoCoins = do
    c1 <- coin
    c2 <- coin
    return (c1, c2)

main = do 
    print twoCoins
