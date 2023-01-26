import Data.Sequence (ViewR(EmptyR), empty)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
----1
-------a
altura :: BTree a -> Int
altura empty = 0
altura (Node r e d) = if altura e > altura d then 1+ altura e else 1+ altura d

--------b
contaNodos :: BTree a -> Int
contaNodos empty = 0 
contaNodos (Node r e d ) = 1+ contaNodos e + contaNodos d

-------c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1 
folhas (Node r e d) =  (folhas e ) + (folhas d) 

-------d
prune :: Int -> BTree a -> BTree a
prune _ empty = Empty 
prune 0 _ = Empty 
prune x (Node r e d ) = Node r (prune (x-1) e) (prune (x-1) d)

-----e)
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path l Empty = []
path (h:t) (Node r e d)= if h== True then r: path t e else r:path t d

------f
mirror :: BTree a -> BTree a
mirror empty = empty
mirror (Node r e d )= Node r (mirror e) (mirror d)

------g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r e d ) (Node a b c )=  Node (f r a) (zipWithBT f e b) (zipWithBT f d c )
zipWithBT _ _ _ = Empty
-----h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d ) = (Node a unzipBTe1  unzipBTd1 , Node b unzipBTe2  unzipBTd2, Node c unzipBTe3 unzipBTd3 )
     where 
        (unzipBTe1,unzipBTe2,unzipBTe3 )=  unzipBT e
        (unzipBTd1,unzipBTd2,unzipBTd3) = unzipBT d

-----------2
---------------a)
minimo :: Ord a => BTree a ->a
minimo (Node x empty _ ) = x 
minimo (Node x e d )= minimo e 
----------------b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r empty d )= d 
semMinimo ( Node r e d)= Node r (semMinimo e) d 

-----------------c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin  (Node r empty d) = (r, d)
minSmin (Node r e d) = (a, Node r b d)
    where (a,b) = minSmin e 

---------------d)
{-
remove :: Ord a => a -> BTree a -> BTree
remove x  (Node r e d)
    | x < r =Node r (remove x e) d
    | x> r = Node r e ( remove x d )
    | otherwise = 

-}
-----
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int |Rep | Faltou deriving Show
type Turma = BTree Aluno
------------
turma :: BTree (Int, String, Regime, Classificacao)
turma = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) (Node (20,"Pedro",TE,Aprov 10) Empty (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty) (Node (28,"Vasco",MEL,Rep) Empty Empty))))
----3
-----------a
inscNum :: Numero -> Turma -> Bool
inscNum n (Node (num,_,_,_) e d) = n == num || if n >num then inscNum n e else inscNum n d  
inscNum n empty= False 
-----------b
inscNome :: Nome -> Turma -> Bool
inscNome m (Node ( _,name,_,__)e d ) = m == name || if m> name  then inscNome m e else  inscNome m d
inscNome n empty = False 

------------c
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty  = []
trabEst (Node (num,nome,TE,_) e d ) = [(num,nome)]++(trabEst e)++ (trabEst d)
trabEst (Node (num,nome,_,_ )e d ) = (trabEst e) ++(trabEst d)

------------d
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,_,_,cla)e d ) 
  |n== num = Just cla 
  |n >num = nota n d 
  |otherwise = nota n e

-------------e
percFaltas :: Turma -> Float
percFaltas Empty = 0.0
percFaltas t=  ( (faltas t) / total t) *100

faltas::Turma -> Float
faltas (Node (_,_,_,Faltou) e d )= 1 + faltas e + faltas d
faltas (Node (_,_,_,_) e d )= faltas e + faltas d 
faltas Empty = 0.0


total::Turma -> Float
total (Node t e d ) =1+ total e + totalP d 
total  Empty = 0.0
-----------f
mediaAprov :: Turma -> Float
mediaAprov Empty = 0.0
mediaAprov t = notas t / totalP t

notas:: Turma -> Float
notas (Node (_,_,_,Aprov x) e d ) = fromIntegral x + notas e + notas d
notas  (Node (_,_,_,_) e d ) = notas e + notas d 
notas Empty = 0.0

totalP:: Turma -> Float
totalP (Node (_,_,_,Aprov _) e d ) = 1 + totalP e + totalP d
totalP (Node (_,_,_,_) e d) = totalP e + totalP d 
totalP Empty = 0.0

-------g
aprovAv :: Turma -> Float
aprovAv Empty = 0.0
aprovAv t = passados t / total t

passados ::Turma-> Float
passados Empty = 0.0
passados (Node (_,_,_,Aprov x )e d)
  | x >= 10 = fromIntegral x + passados e + passados d
  |otherwise = passados e + passados d 
passados (Node (_,_,_,_)e d ) = passados e + passados d