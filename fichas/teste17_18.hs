{-
Teste 2017/18 -janeiro -10
-}

--------1
------------a

insert:: Ord a=> a -> [a]->[a]
insert x [] = [x]
insert x (h:t)
  |x<=h = x:(h:t)
  |otherwise = h: insert x t 

------------2
catMaybes:: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) 
  |h == Nothing = catMaybes t
  |otherwise=  a: catMaybes t

-------------3

data Exp a = Const a | Var String | Mais (Exp a) (Exp a) | Mult (Exp a) (Exp a)

----------a
instance Show a => Show ( Exp a) where
    show :: Exp a -> String
    show Const x = show x 
    show Var s =  s 
    show Mais e1 e2 = "(" ++ show e1 ++ " + " ++ show e2 ")"
    show Mult e1 e2 = "(" ++ show e1 ++ " * " ++ show e2 ")"


-------------4
sort0n :: Ord b => (a->b) -> [a] ->[a]
sort0n  _ []    = [] 
sort0n  f (h:t) = sortOn f s ++ [h]++ sortOn f b 
    where 
        (s,b)= foldr (\x (small,big) -> if f x > f h then (small , x :big else (x: small , big)) ) ([],[]) (h:t)


------------5
---a
amplitude ::  [Int] -> Int 
amplitude [] = 0
amplitude l  = m -n
    where
      (m,n) = foldr (\(small,big) x -> if x <small then (x, big) else (small,big) ) (head l,head l) l 
amplitude l = maximun l - minimun l 

---------b
parte ::[Int] ->([Int],[Int])
parte [] = ([],[])
parte 



----------6

data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]
-----a
conta:: Imagem ->Int
conta Quadrado x = 1 
conta Mover (_,_) i = conta i 
conta Juntar [] = 0 
conta Juntar l =  sum map conta l



\\:: Eq a => [a]->[a]->[a]
\\ [] [] = [] 
\\ (h:t) (x:xs) 
  |h==x = \\ t xs 
  |otherwise = h : \\ t (x:xs) 