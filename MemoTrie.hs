{-#LANGUAGE GADTs,KindSignatures, TypeOperators,TypeFamilies #-}
module MemoTrie(
    HasTrie,
    memo,
    memofix
)where


import Data.Function (fix)

class HasTrie a where
    data (:->:) a :: * -> *
    trie :: (a -> b) -> (a :->: b)
    untrie :: (a :->: b) -> (a -> b)
    enumerate :: (a :->: b) -> [(a,b)]
    
memo :: HasTrie a => (a -> b) -> (a -> b)
memo = untrie . trie    
    
memofix :: (HasTrie a)=> ((a -> b) -> (a -> b)) -> (a -> b)
memofix f = fix (memo . f)    
{-
merge :: (b -> b -> b) -> a :->: b -> a :->: b -> a :->: b
merge f t1 t2 
-}
    
instance (HasTrie a, Show a, Show b)=> Show (a :->: b) where
    show t = "Trie: " ++ show (enumerate t)

instance (HasTrie a) => Functor ((:->:) a) where
    fmap f t = trie (fmap f (untrie t))
    
instance HasTrie a => Applicative ((:->:) a) where
  pure b        = trie (pure b)
  tf <*> tx     = trie (untrie tf <*> untrie tx)

instance HasTrie a => Monad ((:->:) a) where
  return a      = trie (return a)
  u >>= k       = trie (untrie u >>= untrie . k)
    
    
    
    
    
instance HasTrie () where
    data () :->: a = UnitTrie a
    trie f = UnitTrie (f ())
    untrie (UnitTrie t) = const t
    enumerate (UnitTrie t) = [((),t)]

    
instance (HasTrie a,HasTrie b)=>  HasTrie (a,b) where
    data (a,b) :->: c = PairTrie (a :->: (b :->: c))
    trie f = PairTrie (trie (trie . curry f))
    untrie (PairTrie t) = uncurry (untrie . untrie t)
    enumerate (PairTrie t) = concatMap pairup (enumerate t)
                                where
                                   pairup (a,b) = [((a,c),x)| (c,x)<- enumerate b]

instance (HasTrie a,HasTrie b)=> HasTrie (Either a b) where
    data Either a b :->: c = EitherTrie (a :->: c) (b :->: c)
    trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
    untrie (EitherTrie l r) = either (untrie l) (untrie r)
    enumerate (EitherTrie l r) = enum' Left l `weave` enum' Right r
    
enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f t =  map (\(a,b)->(f a,b)) $ enumerate t

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)

instance HasTrie Bool where
    data Bool :->: a = BoolTrie a a
    trie f = BoolTrie (f False) (f True)
    untrie (BoolTrie f t) = \b -> if b then t else f
    enumerate (BoolTrie f t)= [(False,f),(True,t)]

instance (HasTrie a)=>HasTrie [a] where
    newtype [a] :->: b = ListTrie (Either () (a,[a]) :->: b)
    trie f = ListTrie (trie (f . listify))
    untrie (ListTrie t) = untrie t . unlistify
    enumerate (ListTrie t) = enum' listify t
    
listify :: Either () (a,[a]) -> [a]
listify = either (const []) (uncurry (:))

unlistify :: [a] -> Either () (a,[a])
unlistify [] = Left ()
unlistify (x:xs) = Right (x,xs)
    
    
instance HasTrie Integer where
    newtype Integer :->: a = IntTrie ((Bool,[Bool]) :->: a)
    trie f = IntTrie (trie (f . fromBinary))
    untrie (IntTrie t) = untrie t . toBinary
    enumerate (IntTrie t) = enum' fromBinary t
 
 
fromBinary :: (Bool,[Bool])-> Integer
fromBinary (s,xs) = sig $ convert 0 xs
                where
                    sig = if s then id else negate
                    convert a [] = a + 0
                    convert a (True:xs) = convert (2 * a + 1) xs
                    convert a (False:xs) = convert (2 * a) xs

toBinary :: Integer -> (Bool,[Bool])
toBinary z = (z >= 0 , (reverse . toBits . abs ) z)

toBits :: Integer -> [Bool]
toBits 0 = []
toBits n = ((mod n 2) > 0) : toBits (div n 2)
                
   {- 
data (:^) :: (* -> *) -> * -> (* -> *) where
    ZeroC :: a -> (f :^ Z) a
    SuccC :: IsNat n => f ((f :^ n) a) -> (f :^ (S n )) a
    
    
instance Functor f => Functor (f :^ n) where
    fmap f (ZeroC x) = ZeroC (f x)
    fmap f (SuccC fs) = SuccC ((fmap . fmap) f fs)
    
instance (IsNat n, Applicative f) => Applicative (f :^ n) where
    pure = pureN nat
    ZeroC f <*> ZeroC x = ZeroC (f x)
    (SuccC fs) <*> (SuccC xs) = SuccC ((<*>) <$> fs <*> xs)
    
pureN :: (Applicative f)=> Nat n -> a -> (f :^ n) a
pureN Zero a = ZeroC a
pureN (Succ n) a = SuccC ((pure . pureN n) a)
-}


fibF :: (Integer -> Integer) -> (Integer -> Integer)
fibF _ 0 = 1
fibF _ 1 = 1
fibF f n = f (n - 1) + f (n - 2)

collatz :: (Integer -> Integer) -> (Integer -> Integer)
collatz _ 1 = 0
collatz f n = if (mod n 2) == 0 then 1 + f (div n 2) else 1 + f (3 *n + 1) 

recur _ 0 = 1
recur f n = f (div n 2) + f (div n 3) + f (div n 4) + n



{-instance (IsNat n, HasTrie a) => HasTrie (Vec n) where
    data (Vec n a) :->: a = Trie a :^ n
    trie = trieN
    Trie (ZeroC v) `untrie` ZVec = v
    Trie( SuccC t) `untrie` (x :< xs) = (Trie t `untrie` xs) `untrie` x-}