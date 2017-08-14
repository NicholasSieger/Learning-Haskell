{-#LANGUAGE GADTs,KindSignatures, TypeOperators,TypeFamilies#-}
{-#LANGUAGE UndecidableInstances#-}
module NumberTypes(
    Z,
    S,
    (:+:),
    (:*:),
    IsNat(..),
    Nat(..)
    )where
    
data Z 
data S n 

data Nat :: * -> * where
    Zero :: Nat Z
    Succ :: IsNat n => Nat n -> Nat (S n)

class IsNat n where nat :: Nat n
instance IsNat Z where nat = Zero
instance (IsNat n)=>IsNat (S n) where nat = Succ nat

type family n :+: m
type family n :*: m

type instance Z :+: n = n
type instance (S n) :+: m = S (n :+: m)

type instance Z :*: n = Z
type instance (S n) :*: m = (S n) :+: (n :*: m)
    
    