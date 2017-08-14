module Huffmann(
    buildTree,
    encode,
    decode
    )where
    
import qualified Data.Map.Strict as M
import PQ
import PrefixTree

type FreqTable a = M.Map a Int

listFreq :: (Ord a)=> [a] -> FreqTable a
listFreq = foldr (\x m->M.insertWith (+) x 1 m) M.empty

listQueue :: (Ord a)=> FreqTable a -> PQ (WPTree a)
listQueue = M.foldrWithKey (\ k v q->insertPQ (singletonWPT v k) q) emptyPQ 

fillTree :: State (PQ (WPTree a)) (Maybe (PTree a))
fillTree = do{
                    root <- State popPQ;
                    case root of 
                        Nothing -> return Nothing;
                        Just t1 -> do{
                                            t2' <- State popPQ;
                                            case t2' of 
                                                    Nothing -> return (Just (_value t1));
                                                    Just t2 -> do{
                                                                modify (insertPQ (mergeWPT t1 t2));
                                                                fillTree;
                                                                        }
                                            }
                    }                                            
                    
buildTree :: (Ord a)=>[a] -> Maybe (PTree a)
buildTree xs = fst  $ (evalState fillTree) ((listQueue . listFreq) xs)

encode :: (Ord a)=>[a] -> Maybe Encoding
encode xs = (buildTree xs) >>= (\ t-> encodeT t xs) 

decode :: PTree a -> Encoding -> Maybe [a]
decode = decodeT
                       
--state monad implementation 
newtype State s a = State { evalState :: s -> (a,s)}

instance Functor (State s) where
    fmap f v = State $ \s -> let (a,s2) = (evalState v) s in (f a,s2)

instance Applicative (State s) where
    pure x = State $ \s -> (x,s)
    f <*> x = State $ \s -> let (a,s1) = (evalState x) s ;
                                              (g,s2) = (evalState f) s1 ;
                                        in (g a, s2)

instance Monad (State s) where
    return = pure
    m >>= f = State $ \s -> let (a,s1) = (evalState m) s;
                                          in (evalState (f a)) s1

get :: State s s
get  = State $ \s -> (s,s)

put :: s -> State s ()
put v = State $ \s -> ((),v) 

modify :: (s -> s) -> State s () 
modify f = State $ \s -> ((),f s)
