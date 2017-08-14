{-# LANGUAGE MultiParamTypeClasses,GADTs,KindSignatures, 
TypeOperators,TypeFamilies #-}
import Data.Either 
import Modules
import NumberTypes
import MemoTrie

data ExtInts = NegInf | Number Int | PosInf deriving (Show,Ord,Eq)


    

class Game s where
    type Move s :: *
    type Player s :: *
    moves :: s -> [Move s]
    players :: s -> [Player s]
    currentPlayer :: s -> Player s
    makeMove :: s -> Move s -> s
    isOver :: s -> Maybe (Player s)
    start :: s

class (Game s)=> EstGame s where
    estimate :: s -> Player s :->: ExtInts
    gameState :: s -> Either (Player s) (Player s :->: ExtInts)
    gameState s = case isOver s of
                                Just p -> Left p
                                Nothing -> Right $ estimate s

    predictor :: (Game s,Integral n)=> n -> s -> Player s :->: ExtInts
    predictor 0 s = estimate s
    predictor n s = foldr  (const NegInf) $
                            map (predictor (n - 1) . makeMove s) (moves s)

maxP :: (Bounded a,Enum a,Ord b)=> (a -> b) -> (a -> b) -> (a -> b)
maxP f g = 
                                
                                
                                
{- Yuccy Chocky example                           
data Player = Maxie | Minnie deriving Show
newtype State =  State (Player,Int,Int) deriving Show
newtype Position = Position Int

other Maxie = Minnie
other Minnie = Maxie

instance 	Game State Int Player where
    moves (State (Maxie,w,h)) = [2,3 .. w] 
    moves (State (Minnie,w,h)) = [2,3 .. h]
    
    player (State (p,_,_)) = p

    makeMove (State (Maxie,w,h)) m = State (Minnie,m - 1,h)
    makeMove (State (Minnie,w,h)) m = State (Maxie,w,m - 1)


    isOver (State (Maxie,1,h))  = Just Minnie
    isOver (State (Minnie,w,1)) = Just Maxie
    isOver _                              = Nothing
    
    start = State (Maxie,10,2)

instance EstGame State Int Player where
    estimate (State (_,w,h)) Maxie= if w == 1 then -1 else 1
    estimate (State (_,w,h)) Minnie= if h == 1 then 1 else -1
    
maxValue :: (Integral n,Ord v,Num v,EstGame s m p)=>n -> s -> Either p v
maxValue 0 s = gameState s
maxValue n s = foldr (\ a b->pure max <*> a <*> b) (gameState s)
                                $ map (minValue (n - 1) . makeMove s) (moves s)
                                
minValue :: (Integral n,Ord v,Num v,EstGame s m p)=>n -> s -> Either p v
minValue 0 s = gameState s
minValue n s = foldr (\ a b->pure min <*> a <*> b) (gameState s)
                                $ map (maxValue (n - 1) . makeMove s) (moves s)
                 -}               
