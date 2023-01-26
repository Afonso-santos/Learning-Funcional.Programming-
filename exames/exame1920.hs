
{-
teste 2 janeiro 2020
-}

-------1
---------a)
inits:: [a] -> [[a]]
inits [] = []
inits l  = inits (init l) ++ [l]

-------b)
isPrefixOf:: Eq a => [a]->[a]->Bool
isPrefixOf [] [] = True
isPrefixOf [] l2 = True
isPrefixOf l1 [] = False  
isPrefixOf (h:t) (x:xs)
  |h== x = isPrefixOf t xs 
  |otherwise = False 

--------------2
data BTree a = Empty 
               |Node a (BTree a ) (BTree a ) deriving Show

---------------a
folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node r Empty Empty) = 1 
folhas (Node r e d) = folhas e + folhas d 

-------------b
path :: [Bool] -> BTree a-> [a] 
path [] (Node r _  _ )= [r] 
path _ Empty = []
path (h:t) (Node r e  d) 
  |h = r: path t d
  |otherwise = r : path  t e 

------------3
type Polinomio = [Coeficinte] 
type Coeficinte = Float

--------------------------a
valor :: Polinomio -> Float -> Float
valor l x = foldr (\(g,q) acc -> acc +q*x^g ) 0 (zip [0..] l)  

--------------------------b

deriv ::Polinomio -> Polinomio
deriv l = tail $ foldr(\(g,c) acc -> (g*c):acc ) [] (zip [0..] l )

-----------------------------c

soma :: Polinomio->Polinomio->Polinomio 
soma [] [] = []
soma p1 [] = p1 
soma [] p2 = p2 
soma (h:t) (x:xs)= (h+x) : soma t xs 

----4
type Mat a = [[a]]
-------a
quebraLinha :: [Int] -> [a] -> [[a]] 
quebraLinha [] _ =[]
quebraLinha _ [] = [] 
quebraLinha (h:t) l = (take h l ):quebraLinha t (drop h l)
--------b











