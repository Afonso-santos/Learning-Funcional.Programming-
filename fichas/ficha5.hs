import Data.List
import GHC.Base (VecElem(Int16ElemRep, FloatElemRep))

-----1
-------a)
anie :: (a -> Bool) -> [a] -> Bool
anie _ []= False 
anie f (h:t)=  f h == True ||  anie f t
-------b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] l = []
zipWith' _ l [] = []
zipWith'  f l1 l2 =  f (head l1) (head l2) : zipWith'  f (tail l1) (tail l2)

--------c)
tateWhile :: (a->Bool) -> [a] -> [a]
tateWhile  _ [] =  []
tateWhile f (h:t)
  |f h == True = h : tateWhile f t
  |otherwise = tateWhile f t

--------d)
drophile :: (a->Bool) -> [a] -> [a]
drophile _ [] = []
drophile f (h:t)
  | f h == True =h :drophile f t
  | otherwise = drophile f t 
-------e)
spwa :: (a-> Bool) -> [a] -> ([a],[a])
spwa _ [] =([],[])
spwa f l = ( tateWhile f l ,drophile f l  )
-------f) 
deleteBye :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBye _ _ [] = []
deleteBye f x (h:t)
  |f x h == False = t
  |otherwise = deleteBye f x t 
------g)
s0rtOn :: Ord b => (a -> b) -> [a] -> [a]
s0rtOn _ [] = []
s0rtOn f [x] = [x]
s0rtOn f (h:s:t) 
  |f h <= f s = h : s0rtOn f t
  |otherwise = s : s0rtOn f (h:t) 
{-}
mysortOn :: Ord b => (a->b) -> [a] -> [a]
mysortOn p [] = []
mysortOn p (h:t) = myinsert h (mysortOn p t)
                where myinsert x [] = [x]
                      myinsert x (y:ys) | p x <= p y = x:y:ys
                                        | otherwise = y : myinsert x y
                                        
----2mysortOn :: Ord b => (a->b) -> [a] -> [a]
mysortOn p [] = []
mysortOn p (h:t) = myinsert h (mysortOn p t)
                where myinsert x [] = [x]
                      myinsert x (y:ys) | p x <= p y = x:y:ys
                                        | otherwise = y : myinsert x y -}

---2
------a)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau g  = filter (aux g )
    where 
        aux g (n,gr) = g== gr 

-----b)
conta :: Int -> Polinomio -> Int
conta n l = foldl contaux 0 l
    where
        contaux :: Int -> Monomio -> Int
        contaux acc (num,gr) 
          | n == gr = 1 + acc
          | otherwise = acc
{-
conta = lenght(filter (aux g ))
    where 
        aux g (n,gr) = g== gr 
-}

----c)
grau :: Polinomio -> Int 
grau l = foldl grauMaior 0 l
  where 
    grauMaior:: Int->Monomio -> Int
    grauMaior acc (num,gr)
      |acc < gr =gr 
      |otherwise = acc 
----d) 
deriv :: Polinomio -> Polinomio
deriv l = filter (/= (0,0)) (map(\(num,gr)->if gr>0 then (num*gr,(fromInteger gr )-1) else (0,0)) l)

----e)
calcula :: Float -> Polinomio -> Float
calcula x l = foldl calaux (0.0) l 
  where
    calaux::Int-> Monomio->Float
    calaux acc (num,gr)
      |gr /= 0 = num*(x^gr) + fromInteger acc
      |otherwise = acc + num*x