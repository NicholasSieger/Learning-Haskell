{-#LANGUAGE GADTs,KindSignatures, TypeOperators,TypeFamilies#-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Modules(
    Vec(..),
    headV,
    tailV,
    Module(..)
    )where 

import NumberTypes
    
infixr 5 :<

data Vec :: * -> * -> * where
    ZVec :: Vec Z a
    (:<) :: (IsNat n)=>a -> Vec n a -> Vec (S n) a

instance (Show a)=> Show (Vec n a) where
        show ZVec = "=||"
        show (x :< xs) = (show x) ++ " :< " ++ (show xs)
    
instance Functor (Vec n) where
    fmap f ZVec = ZVec
    fmap f (x :< xs) = f x :< fmap f xs
    
instance Foldable (Vec n) where
    foldr _ i ZVec = i
    foldr f i (x :< xs) = x `f` foldr f i xs
    
instance (IsNat n)=>Applicative (Vec n) where
    pure a = units a
    ZVec <*> ZVec = ZVec
    (f :< fs) <*> (x :< xs) = f x :< (fs <*> xs)
    
instance (IsNat n)=>Monad (Vec n) where
    return = pure
    m >>= f = join (fmap f m)
    
headV :: Vec (S n) a -> a
headV (x :< _) = x    

tailV :: Vec (S n) a -> Vec n a
tailV (_ :< xs) = xs
  
join :: Vec n (Vec n a) -> Vec n a
join ZVec = ZVec
join (v :< vs) = headV v :< join (fmap tailV vs)

units :: IsNat n => a -> Vec n a
units x = unitsN nat x

unitsN :: Nat n -> a -> Vec n a
unitsN Zero _    = ZVec
unitsN (Succ n) x = x :< unitsN n x   


class Module v where
    type Scalar v :: *
    identity :: v
    (|+|) :: v-> v ->v 
    (*|) :: Scalar v -> v -> v
    
    
instance (Num a,IsNat n)=>Module (Vec n a) where
    type Scalar (Vec n a) = a
    identity = pure $ fromInteger 0
    u |+| v = pure (+) <*> u <*> v
    c *| v = fmap (*c) v
