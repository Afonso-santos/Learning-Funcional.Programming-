{-
exame de 2018-19 / janeiro / 25 
-}
----1
------a
isSorted :: (Ord a)=> [a] -> Bool
isSorted [] = True
isSorted [x] = True   
isSorted (h:x:t)
  |h<=x = isSorted (x:t)
  |otherwise = False 

--------b
inits:: [a]-> [[a]] 
inits [] = [] 
inits l = inits (init l) ++  [l]

------------------2
maxinumMB :: (Ord a) => [Maybe a]-> Maybe a
maxinumMB [] = Nothing 
maxinumMB (h:t)------------------------


-----------3
data LTree a = Tip a | Fork (LTree a ) (LTree a)

ex = Fork (Tip 4) (Fork (Fork (Tip 2) (Tip 3) ) (Tip 6) )
-----------------------a
listaLT:: LTree a -> [a]
listaLT (Tip a) = [a] 
listaLT (Fork e d) = listaLT e ++  listaLT d 

----------------------b
instance Show a => Show (LTree a) where
    show:: LTree a -> String 
    show (Tip a)    = "."++ show a 
    show (Fork e d) =show e + \n ++ show d

-------------------4
