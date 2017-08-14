module RBTree (
    RBTree,
    empty,
    find,
    insert,
    fmap,
    reduce
)where
    
data Color = R | B  
    
data RBTree k v = Empty | Node k v Color (RBTree k v) (RBTree k v)

instance (Show k, Show v)=> Show (RBTree k v) where
    show Empty = ""
    show (Node k v c l r) = "(" ++ show l ++ "{" ++ show k ++ 
                                        "," ++ show v ++ "}" ++ show r ++ ")"

empty = Empty
                              
find :: (Ord k)=>RBTree k v-> k -> Maybe v
find Empty x = Nothing
find (Node y v c l r) x 
                              | x > y   = find l x
							  | x == y = Just v
							  | x < y   = find r x      

balance k0 v0 B (Node k1 v1 R a (Node k2 v2 R b c)) d = Node k0 v0 R (Node k1 v1 B a b) (Node k2 v2 B c d)
balance k0 v0 B (Node k1 v1 R (Node k2 v2 R a b) c) d = Node k0 v0 R (Node k1 v1 B a b) (Node k2 v2 B c d)
balance k0 v0 B a (Node k1 v1 R b (Node k2 v2 R c d)) = Node k0 v0 R (Node k1 v1 B a b) (Node k2 v2 B c d)
balance k0 v0 B a (Node k1 v1 R (Node k2 v2 R b c) d) = Node k0 v0 R (Node k1 v1 B a b) (Node k2 v2 B c d)
balance k v c l r = Node k v c l r
                              
                              
insert :: (Ord k)=> RBTree k v -> (k,v) -> RBTree k v 
insert t (k,v) = case insertH t (k,v) of
                                Node k v R l r -> Node k v B l r
                                t2 -> t2
                              
insertH :: (Ord k)=> RBTree k v -> (k,v) -> RBTree k v
insertH Empty (k,v) = Node k v R Empty Empty
insertH (Node y w c l r) (k,v) 
                              | k < y   = balance y w c (insertH l (k,v)) r
							  | k == y = Node y w c l r
							  | k > y   = balance y w c l (insertH r (k,v)) 

instance Functor (RBTree k) where
    fmap f Empty = Empty
    fmap f (Node k v c l r) = Node k (f v) c (fmap f l) (fmap f r)
    
reduce :: ((k,v)->(k,v)->(k,v)->(k,v))->(k,v) -> RBTree k v -> (k,v)
reduce f (k,v) Empty = (k,v)
reduce f (k,v) (Node k2 v2 c l r) = f (reduce f (k,v) l) (k2,v2) 
                                                        (reduce f (k,v) r)