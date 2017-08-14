{-# LANGUAGE DeriveFoldable #-} 
module PQ(
    PQ,
    emptyPQ,
    singletonPQ,
    insertPQ,
    popPQ,
    sizePQ
    )where

import Data.Foldable    
    
--the wrapper datatype    
newtype PQ a = PQ  (Skew a) deriving Show   

--implementation
data Skew a = SLeaf | SNode a (Skew a) (Skew a) deriving (Show,Eq,Foldable)

emptyPQ :: PQ a
emptyPQ = PQ SLeaf

singletonPQ :: a -> PQ a
singletonPQ = PQ . singleton

insertPQ :: (Ord a)=> a -> PQ a -> PQ a
insertPQ x (PQ h) = PQ $ insert h x

popPQ :: (Ord a)=>PQ a -> (Maybe a, PQ a)
popPQ (PQ h) = (ans,PQ h') 
                        where 
                                (ans,h') = pop h

sizePQ :: PQ a -> Int
sizePQ (PQ h) = length (toList h)


--implementation
singleton :: a -> Skew a
singleton x = SNode x SLeaf SLeaf

insert h x = merge h (singleton x)

merge :: (Ord a)=> Skew a -> Skew a -> Skew a
merge SLeaf h = h
merge h SLeaf = h
merge hA@(SNode xA lA rA) hB@(SNode xB lB rB)
    | xA < xB    = SNode xA (merge rA hB) lA
    | otherwise = SNode xB (merge rB hA) lB
    
pop SLeaf = (Nothing,SLeaf)
pop (SNode x l r) = (Just x, merge l r)    