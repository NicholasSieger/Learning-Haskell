{-# LANGUAGE DeriveFunctor,DeriveGeneric #-} 
module PrefixTree(
    Weighted(..),
    PTree,
    WPTree,
    Encoding(..),
    Direction(..),
    singletonPT,
    mergePT,
    singletonWPT,
    mergeWPT,
    encodeT,
    decodeT
    )where
    
import Control.Applicative ((<$>),(<*>),(<|>))
import Data.Binary
import Data.List (unfoldr)
import Data.Monoid ((<>))
import GHC.Generics
import qualified Data.Map.Strict as M


--for weighted data
data Weighted a = W { _weight :: Int, _value :: a } deriving (Show,Functor)

instance Ord (Weighted a) where
    compare (W w1 _) (W w2 _) = compare w1 w2
instance Eq (Weighted a) where
        (W w1 _) == (W w2 _) = w1 == w2

        
--prefix tree datatype
data PTree a = PLeaf a | PNode (PTree a) (PTree a) deriving (Show,Eq,Generic)

singletonPT :: a -> PTree a
singletonPT = PLeaf

mergePT :: PTree a -> PTree a -> PTree a
mergePT = PNode

type WPTree a = Weighted (PTree a)

singletonWPT :: Int -> a -> WPTree a
singletonWPT w = W w . PLeaf

mergeWPT :: WPTree a -> WPTree a -> WPTree a
mergeWPT (W w1 t1) (W w2 t2) = W (w1 + w2) (mergePT t1 t2)


--for traversing the PrefixTree
data Direction = L | R deriving (Show,Eq,Generic)

type Encoding = [Direction]
instance Binary Direction


instance (Binary a)=> Binary (PTree a) where 
    put (PLeaf x) = do{
                 put True;
                 put x}
    put (PNode l r) = do{
                 put False;
                 put l;
                 put r;
                 }
    
    get = do{
                 isLeaf <- get;
                 if isLeaf then PLeaf <$> get
                              else PNode <$> get <*> get
                }

makeTable :: (Ord a)=> PTree a -> M.Map a Encoding
makeTable t = go t []
                       where 
                            go (PLeaf x) enc = x `M.singleton` reverse enc
                            go (PNode l r) enc = go l (L:enc) <> go r (R:enc)
                            
lookupTable :: Ord a => M.Map a Encoding -> a -> Maybe Encoding
lookupTable = flip M.lookup   

encodeT :: (Ord a)=> PTree a -> [a] -> Maybe Encoding
encodeT t xs = concat <$> sequence (map (lookupTable tb) xs)
    where
        tb = makeTable t
        
decodeOne :: PTree a -> Encoding -> Maybe (a, Encoding)
decodeOne (PLeaf x) ds          = Just (x,ds)
decodeOne (PNode l r) (L:ds) = decodeOne l ds
decodeOne (PNode l r) (R:ds) = decodeOne r ds
decodeOne (PNode _ _) []       = Nothing

decodeT :: PTree a -> Encoding -> Maybe [a]
decodeT (PLeaf _) _ = Nothing
decodeT t  enc          = Just $ unfoldr (decodeOne t) enc 