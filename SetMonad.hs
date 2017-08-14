{-# LANGUAGE GADTs, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- Various implementations of the Set monad

module SetMonad where

import Control.Monad
import Control.Monad.Cont
import Control.Applicative
import Data.Set as S

-- ========================================================================
-- The efficient Set monad (non-CPS)

data SetM a where
    SMOrd :: Ord a => S.Set a -> SetM a
    SMAny :: [a] -> SetM a 

instance Functor SetM where
        fmap f m = SMAny $ Prelude.map f $ setMtoList m
    
instance Applicative SetM where
        pure = return
        f <*> x = f >>= (\g -> fmap g x)

instance Alternative SetM where
    empty = SMAny []
    (SMAny x) <|> (SMAny y) = SMAny (x ++ y)
    (SMAny x) <|> (SMOrd y) = SMOrd (union y (fromList x))
    (SMOrd x) <|> (SMAny y) = SMOrd (union x (fromList y))
    (SMOrd x) <|> (SMOrd y) = SMOrd (union x y)
        
instance Monad SetM where
    return x = SMAny [x]
    m >>= f = collect . Prelude.map f $ setMtoList m
    

instance MonadPlus SetM where
    mzero = Control.Applicative.empty
    mplus x y = x <|> y


    
setMtoList :: SetM a -> [a]
setMtoList (SMOrd x) = S.toList x
setMtoList (SMAny x) = x

collect :: [SetM a] -> SetM a
collect []  = SMAny []
collect [x] = x
collect ((SMOrd x):t) = case collect t of
                         SMOrd y -> SMOrd (S.union x y)
                         SMAny y -> SMOrd (union x (fromList y))
collect ((SMAny x):t) = case collect t of
                         SMOrd y -> SMOrd (union y (fromList x))
                         SMAny y -> SMAny (x ++ y)

runSetM :: Ord a => SetM a -> Set a
runSetM (SMOrd x) = x
runSetM (SMAny x) = S.fromList x



-- choose is now taken as primitive
-- We could've defined MonadPlus with an additional constraint.
-- It would have been unrestricted monad (only non-deterministic
-- choice is restricted)
chooseOrd :: Ord a => [a] -> SetM a
chooseOrd x = SMOrd (fromList x)

ptriang_setm :: Integer -> Set Integer
ptriang_setm m= runSetM $ do
  let triang n = n * (n+1) `div` 2
  k <- chooseOrd [1..m]
  i <- chooseOrd [1..k]
  j <- chooseOrd [1..i]
  if triang i + triang j == triang k then return k else mzero
-- fromList [3,6,8,10,11,13,15,16,18,20,21,23,26,27,28]




