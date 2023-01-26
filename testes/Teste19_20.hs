{-
Teste 11/janeiro/ 2019-20
-}

----1
------a
intersect::Eq a=> [a]->[a]->[a]
intersect [] [] = []
intersect [] _ = []
intersect _ [] = []
intersect (h:t) l2 
  | elem h l2 {- == True -} =h: intersect t l2
  | otherwise = intersect t l2 

-------b
tails:: [a]-> [[a]] 
tails [] = [[]]
tails (x:t) = (x:t):tails t

----2
-----------a
----------------
type ConjInt =[Intervalo]
type Intervalo = (Int,Int)
-------------------------
elems :: ConjInt -> [Int]
elems [] = []
elems ((x,xs):t)
  |x== xs = x : elems t 
  |otherwise = [x..xs] ++ elems t 
 
elemsAux::(Int,Int)-> [ Int]
elemsAux ( , ) =  []
elemsAux (x,xs) 
  | xs > x =x: elemsAux (x+1,xs)
  | otherwise = [x] 

------------b
{-
geraconj :: [Int] -> ConjInt 
geraconj [] = []
geraconj (h:x:t)
  |h+1== x  = (h,geraconjAux (x:t)):
  |otherwise =(h, h) : geraconj (x:t)

geraconjAux:: [Int]-> Int
geraconjAux [x] = x 
geraconjAux (h:x:t) 
  |h+1==x = geraconjAux (x:t)
  |otherwise = h   -}
--------------------

geraconj::[Int] -> ConjInt
geraconj [] = []
geraconj (h:t)
  case geraconj t of 
    [] -> [(h,h)]
    (a,b):r 
        | h+1 == a -> (h,b):r
        |otherwise -> (h,h): (a,b):r  



-------------3
data Contacto = Casa Interger | Trab Interger |Tlm Interger | Email String deriving (Show) 

type Nome  = String 
type Agenda  = [(Nome ,[Contacto])]
---------------------a


acrescEmail::Nome -> String -> Agenda -> Agenda 
acrescEmail n e [] = [(n, [Email e] )]
acrescEmail n e ((no,l):t)
  | n== no = ((no, l: Email e):t)
  | otherwise =(no,l) : acrescEmail n e t


------------------b

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((no,c):t)
  | n == no  = Just map(\Email e-> e)(filter(isEmails) c)
  |otherwise = verEmails n t 
   where
    isEmails (Email _ ) = True
    isEmails  _         = False

---------------------c

consulta :: [Contacto] -> ([Interger],[String])
consulta [ ] = ( , )
consulta l =
  foldr(\x -> (tlfn, email)-> case x of Casa n  -> (tlfn:n,_)
                                        Trab n  -> (tlfn:n,_)
                                        Tlm n   -> (tlfn:n ,_ )
                                        Email e -> (_, email:e )) ([],[]) l 

---------------------d~~~~~~~~~~~~DUVIDAS 
consultaIO :: Agenda ->IO()
consulta [] = return ()
consultaIO a = do 
  nome <- getLine 
  let contactos = getContactos a nome
  putStrLn(show contactos)

getContactos:: Agenda -> Nome -> [Contacto]
getContactos [] _ = []
getContactos((n,c):t) nome 
  |n == nome = c 
  |otherwise = getContactos t nome 





---------4
data RTree a = R a [RTree a] deriving (Show,Eq)

----------------a
paths :: RTree a -> [[a]] 
paths R a [ ] = [[a]]
paths R a (h:t) = map (a:) (concatMap paths (h:t))

---------------b------------------DUVIDAS
unpaths :: Eq a => [[a]] -> RTree
unpaths [[x]]    = R x [ ]

unpaths l = R raiz(map unpaths (groupBy (\a b -> head a ==head b ) caudas)) 
  where raiz =head(head l )
        caudas = map tal l 




randomSel:: Int -> [a] -> IO[a]
randomSel 0 _ = return []
randomSel n l  = do 
  randomIndex <- randomRio (0 , length l -1)
  let randomElem =l !! randomIndex
  recur <- randomSel (n-1) (take (randomIndex) l ++  drop(randomIndex +1) l ) 
  return (randomElem :recur ) 

func::[[Int]] -> [Int]
func l = concat (filter(\x -> sum x > 10)l)

func'::[[Int]]-> [Int]
func'[]= []
func' (h:t)
  |sum h > 10 = h ++ func' t1
  |otherwise =  func' t 



organiza::Eq a => [a] -> [(a,[Int])]
organiza l = foldr(\e acc ->insere e (map (\x,xs acc)  ) [] l

insere:: a-> [(a,[Int])] ->[(a,[Int])]
insere a [] = [(a,[0])]
insere a ((x,xs):t)
  | a == e = (x,0:is ):t
  | otherwise = (x,xs):insere a t 

