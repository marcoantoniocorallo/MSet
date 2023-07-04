module MultiSet
( MSet
, empty
, add
, occs
, elems
, subeq
, union
, fromList
, toString
) where

data MSet a = MS [(a, Int)] deriving (Show)

-- empty returns an empty multiset
empty :: MSet a
empty = MS []

-- add mset v returns a multiset obtained by adding the element v to mset.
-- if v is already present in mset, its multiplicity is increased by one,
-- otherwise it is inserted with multilplicity 1.
add :: Eq a => MSet a -> a -> MSet a
add mset v = add' mset v []
  where add' (MS []) v acc = MS $ (v,1):acc
        add' (MS ((x, occ):xs)) v acc
          | x == v    = MS $ (x, occ+1):acc ++xs
          | otherwise = add' (MS xs) v ((x, occ):acc)

-- occs mset v returns the number of occurrences of v in mset.
occs :: Eq a => MSet a -> a -> Int
occs (MS []) _ = 0
occs (MS ((x, occ):xs)) v
  | x == v    = occ
  | otherwise = occs (MS xs) v

-- elems mset returns a list containing all the elements of mset.
elems :: MSet a -> [a]
elems (MS xs) = map fst xs

-- subeq mset1 mset2 returns True if each element of mset1 is also an element of mset2 
-- with the same multiplicity at least.
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS []) _ = True
subeq (MS ((x,occ):xs)) (MS ys) = case lookup x ys >>= (\v -> return (v >= occ)) of
  Just True -> subeq (MS xs) (MS ys)
  Just False -> False
  Nothing -> False

-- union mset1 mset2 returns an MSet having all the elements of mset1 and of mset2
-- each with the sum of the corresponding multiplicites.
union :: Eq a => MSet a -> MSet a -> MSet a
union (MS xs) (MS ys) = 
  MS(
    [(x,n) | (x,m) <- xs, (x',k) <- ys, x==x', let n = m + k]++
    [(x,n) | (x,n) <- xs, x `notElem` (elems (MS ys))]++
    [(y,n) | (y,n) <- ys, y `notElem` (elems (MS xs))]
  )

-- fromList l parses a list l and returns the MSet containing the element of the list, 
-- each with the corresponding multiplicity
fromList :: Eq a => [a] -> MSet a
fromList l = MS (reduce [ (x,1) | x<-l ] sum)

-- toString mset returns a string containing, one per line, each element of mset 
-- in the format "<elem> - <multiplicity>"
toString :: Show a => MSet a -> String
toString (MS mset) = unlines $ map (\(x,n) -> "<"++ show x ++"> - <" ++ show n ++">") mset

-- mapMSet f mset returns the MSet obtained by applying f to all the elements of mset
-- mapMSet cannot be an implementation of fmap because they have not the same signature.
-- In order to define an istance of Functor for MSets, the implementation of fmap must have the signature
-- fmap :: (a -> b) -> f a -> f b, that is: mapMSet :: (a -> b) -> MSet a -> MSet b
-- However, the signature of mapMSet must considr the type class Eq to returns a well-defined MSet. 
-- Thus the signature of mapMSet is the following, that doesn't coincide with the fmap's one.
mapMSet :: Eq b => (a -> b) -> MSet a -> MSet b
mapMSet f mset = wdefine (mapMSet' f mset [])
  where mapMSet' f (MS []) acc = MS acc
        mapMSet' f (MS ((x,n):xs)) acc = mapMSet' f (MS xs) (((f x),n):acc)
        wdefine (MS l) = MS (reduce l sum )

-------------------------------------------------------------------------------------------------------------
-------------------------------------------- Type Class Instances -------------------------------------------
-------------------------------------------------------------------------------------------------------------

-- two multisets are equal if they contain the same elements with the same multiplicity
instance Eq a => Eq (MSet a) where
  (MS xs) == (MS ys) = all (`elem` ys) xs && all (`elem` xs) ys

-- folding a multiset with a binary function 
-- should apply the function to the elements of the multiset, ignoring the multiplicities.
-- note: I choose to implement foldr as Minimal complete definition 
instance Foldable MSet where
  foldr f acc (MS []) = acc
  foldr f acc (MS ((x,_):xs)) = f x (foldr f acc (MS xs))

-------------------------------------------------------------------------------------------------------------
-------------------------------------------- Utilities functions --------------------------------------------
-------------------------------------------------------------------------------------------------------------

-- rmdup l returns the list l without duplicate elements
rmdup :: Eq a => [a] -> [a]
rmdup l = rmdup' l []
  where rmdup' [] acc = acc
        rmdup' (x:xs) ls
          | x `elem` ls   = rmdup' xs ls
          | otherwise     = rmdup' xs (x:ls)

-- reduce l unify reduce the associative list l using a given unify function.
-- for example:
-- reduce [ (1,1), (2,1), (1,5), (3,1), (2,3) ] sum -> [ (1,6), (2,4), (3,1) ]
-- reduce [ ("k1","v1"), ("k2","v2"), ("k1", "v3") ] unwords -> [("k2","v2"),("k1","v1 v3")]
reduce l unify = rmdup $ map (\(x,_) -> (x, (unify $ map snd $ filter (\(y,_) -> x==y) l) ) ) l