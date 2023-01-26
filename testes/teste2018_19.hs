{-
teste de 2018-29 /janeiro  / 5 
-}
----1
---------a
elemIndices ::Eq a=> a-> [a] -> [Int]
elemIndices _ [] = [] 
elemIndices x (h:t) = elemIndicesAux x (h:t) 0

elemIndicesAux :: Eq a => a-> [a] -> Int ->[Int]
elemIndicesAux _ [] _ = []
elemIndicesAux x (h:t) acc 
  |x== h = acc: elemIndicesAux x t (acc+1)
  |otherwise = elemIndicesAux x t (acc+1) 

-----------b
isSubsequenceOf::Eq a =>[a]-> [a]-> Bool 
isSubsequenceOf [] [] = True
isSubsequenceOf [] l2 = True 
isSubsequenceOf l1 [] = False 
isSubsequenceOf (h:t) (x:xs) 
  |h == x =isSubsequenceOf t xs 
  |otherwise = isSubsequenceOf (h:t) xs 

----------2
data Btree a = Empty| Node a (BTree a) (BTree)
-------------a
 
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAp _ Empty = Nothing
lookupAP x (Node (ra,rb) Empty Empty) = Just rb  
lookupAP x (Node (ra.rb) e d ) 
  |x > ra =  lookupAP d
  |x < ra = lookupAP e 
  |otherwise =Just rb

------------b

zipWithBT :: (a-> b-> c) -> BTree a -> Btree b -> BTree c 
zipWithBT _ Empty Empty = Empty 
zipWithBT f (Node ra e d) (Node rb l r ) = Node (f ra rb) (zipWithBT f e l) (zipWithBT f d r)


------------------3
DigitAlpha:: String -> (String,String) 
DigitAlpha [] = ([],[])
DigitAlpha l = foldr(\x (num,lett)->  if isDigit x then (x:num,lett) else (num, x:lett) ) ([],[])  l 


-----------4

data Seq a = Nil | Cons  a (Seq a) | App (Seq a) (Seq a )
-----------------------------a

firstSeq :: Seq a -> a 
firstSeq (Cons a s) = a
firstSeq (App s1 _) = firstSeq s1 
firstSeq (App Nil s) = firstSeq s 

-----------------------------b
dropSeq :: Int->Seq a -> Seq a 
dropSeq _ Nil         = Nil
dropSeq 0 s           = s  
dropSeq x (Cons a s)  = dropSeq (x-1) s 
dropSeq x (App s1 s2) = App (dropSeq  (x-1) s1) (dropSeq (x-1)s2) 

---------------------------c

instance Show a => Show Seq a where 
  Show:: Seq a -> String 
  Show Nill        = ""
  Show (Cons a s)  = show a++ "," ++Show s
  Show (App s1 s2) = "<<" ++ Show s1 ++ " , "++ Show s2 ++ ">>" 

--------------------5
type Mat a = [[a]]

-----------------------------a
getElem :: Mat a-> IO a 
getElem m = 
  do l<- randomRIO (0, (length m -1) )
     c<- randomRIO (0, (length (head m) -1 ))
     print( ((m !! l)!! c ))

magic :: Mat Int-> Bool 
magic [] = False 
magic [x] = False 
magic mat = linhasI nM mat  && colunasI nM mat  && diagoniasI nM mat 
  where nm= sum (head mat)

linhasI :: Int -> Mat Int -> Bool
linhsI x l =foldr(\s acc -> sum s == x && acc) True l 

colunasI :: Int -> Mat Int -> Bool
colunasI n l = foldr(\x acc ->sum(map(\l -> l!! x)mat )== n && acc ) True ([0..(length mat -1)]) 

diagomais n l = sum (map(\x ->(l!! n ) ))