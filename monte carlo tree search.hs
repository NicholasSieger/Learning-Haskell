{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-
implementation of a multi-armed bandit approach for searching game trees
author: Nicholas Sieger
status: very much under construction
-}

import Game


class (Eq p)=>Game s m p | s -> m, s -> p where 
    moves :: s -> [m]
    player :: s -> p
    makeMove :: s -> m -> s
    isOver :: s -> Maybe p --gives the winner if there is one
    start :: Int -> s    --takes a seed and produces a starting state
    
    
{-- -> find best leaf 
      -> pick move/moves from leaf 
      -> simulate entire game from new children
      -> backpropagate winner
      
      Tree needs:
      current game state
      mapping from moves to trees
      wins
      passes
      best move
 --}  
data GameTree s m p = 

update :: (Game s m p)=> Int -> GameTree s m p 
                                               -> (Map p Int, GameTree s m p)
update n GameTree                                               
expand :: (Game s m p)=>Int -> GameTree s m p -> [(Rand Int, s)]
simulate :: (Game s m p)=> (Rand Int,s) -> p
simulate g s = case isOver s of
                        Just p -> p
                        Nothing ->  do {
                                                
                                                
                                                    }
