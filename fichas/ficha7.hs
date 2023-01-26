import GHC.CmmToAsm.AArch64.Instr (x0)
import Data.Time.Format.ISO8601 (yearFormat)
import System.Posix.Internals (lstat)
data ExpInt = Const Int| Simetrico ExpInt|  Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt
-------------1
-----------------a
calcula::ExpInt-> Int
calcula e =
    case e of 
        Const x     -> x
        Simetrico x -> - (calcula x)
        Mais x y    -> calcula x + calcula y
        Menos x y   -> calcula x - calcula y 
        Mult x y    -> calcula x + calcula y 

---------------b)
infixa :: ExpInt -> String
infixa (Const x)     = show x
infixa (Simetrico x) = "-"++ infixa x
infixa (Mais x y )   = "(" ++ infixa x ++ "+" ++ infixa y ++ ")"
infixa (Menos x y )  = "(" ++ infixa x ++ "-" ++ infixa y ++ ")"
infixa (Mult x y )   = "(" ++ infixa x ++ "*" ++ infixa y ++ ")"

---------------c
posfixa :: ExpInt -> String
posfixa (Const x)     = show x
posfixa (Simetrico x) = posfixa x ++" (-)"
posfixa (Mais x y )   = posfixa x ++posfixa y ++ " (+)"
posfixa (Menos x y )  = posfixa x ++ posfixa y ++ " (-)"
posfixa (Mult x  y)   = posfixa x ++ posfixa y ++ " (*)"

----------------------2
data RTree a = R a [RTree a]

arvore = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]

--arvore = R 6 [R 4 [R 7 [R 1 [],R 3 []],R 9 []],R 3 [R 12 []],R 6 [],R 11 []]
----------------a)
soma :: Num a => RTree a -> a
soma (R a []) = a 
--soma (R a (h:t)) = a + sum( map soma (h:t))
soma (R a (h:t)) = a + soma h+ soma (R a t)


---------------b)
altura :: RTree a -> Int
altura (R a [])   = 1 
--altura (R a l)    = 1+ maximum(map altura l)
altura (R a (h:t))= 1 + altura h + altura (R a t)

---------------c
prune :: Int -> RTree a -> RTree a
prune 0 (R a[])     = R a []
prune x (R a l)     = R  a  (map(prune (x-1) )l)

---------------d

mirror :: RTree a -> RTree a
mirror (R a []) = R a []
--mirror (R a l ) = R a (map mirror( reverse l))
mirror (R a (h:t) ) = R a  (reverse (h:t))


---------------e
postorder :: RTree a -> [a]
postorder (R a []) = [a] 
postorder (R a (h:t) ) = postorder h  ++ postorder (R a t)

--------------------------------4
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)
--------------------------
leef = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))
---------------------a)
ltSum:: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork a b ) = ltSum a +ltSum b 

-------------------b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b ) = listaLT a ++ listaLT b 

-------------------c)
{-}
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork a b) = ltHeight a + ltHeight b 
-}


ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)


-------------------4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
{-
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)
-}

ftree = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))
--------------------a
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b)=(Empty,Tip b)
splitFTree (No a b c) = (Node a ls lx, Fork ly lz)
  where 
    (ls,ly) = splitFTree b 
    (lx,lz) = splitFTree c

-------------------------b
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty  (Tip a)           = Just a
joinTrees (Node r e d ) (Fork a b) = case (joinTreese e a, joinTrees d b ) of (Just x, Just y) -> Just (Node r x y)
                                                                             _ -> Nothing
joinTrees _ _ = Nothing 