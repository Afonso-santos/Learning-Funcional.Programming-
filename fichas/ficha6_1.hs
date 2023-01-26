import Data.Sequence (ViewR(EmptyR), unzip, empty)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

arvore = (Node 5 (Node 2 (Node 1 Empty
                                 Empty) 
                         (Node 3 Empty 
                                 Empty)) 
                 (Node 9 (Node 7 (Node 6 Empty 
                                         Empty) 
                                 (Node 8 Empty 
                                         Empty)) 
                         Empty))

---------------1
--------------------------------a
altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d )= if altura e > altura d then 1+ altura e else 1+ altura d 

---------------------------b
contaNodos :: BTree a -> Int
contaNodos Empty = 0 
contaNodos (Node r e d ) = 1+ contaNodos e + contaNodos d 

-----------------------c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) =folhas e + folhas d

----------------------d
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune x (Node r e d ) = Node r (prune (x-1) e) (prune (x-1) d)

--------------------e
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path l Empty = []
path (h:t) (Node r e d) 
  | h {-== True-} = r : path t d
  | otherwise = r : path t e 

-----------------f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node  r e d ) =Node r (mirror d) ( mirror e)

------------------g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT  f (Node r e d)  (Node a b c) = Node ( f r a) (zipWithBT f e b) (zipWithBT f d c )
zipWithBT _ _ _ = Empty

--------------------h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a  x  f , Node b y g ,Node c z h)
  where 
    (x,y,z) = unzipBT e
    (f,g,h) = unzipBT d 


-------------------2

----------------------a
minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node r e d ) = minimo e

---------------------b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty d ) = d
semMinimo (Node r e d ) = Node r (semMinimo e)  d

------------------c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node  r Empty  d) = (r , d )
minSmin (Node r e d ) =(minimo e , Node r (semMinimo e) d )

------------------d

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d) 
  | x < r = Node r (remove x e )  d
  | x > r = Node r  e ( remove x d)
  | otherwise = case d of
    Empty -> e 
    _ -> Node g e h where (g,h) = minSmin d             

--------------------------3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep|  Faltou deriving Show
type Turma = BTree Aluno
----------------------------------
turma :: Turma
turma = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) (Node (20,"Pedro",TE,Aprov 10) Empty (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty) (Node (28,"Vasco",MEL,Rep) Empty Empty))))
-----------------------------a
inscNum :: Numero -> Turma -> Bool
inscNum x Empty = False
inscNum x (Node (n,_,_,_) e d ) 
  |x > n = inscNum x d
  |x < n = inscNum x e 
  |otherwise = True 

-----------------------------b
inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (_,p,_,_) e d) 
  |n ==p = True 
  |otherwise = inscNome n e || inscNome n d

------------------------c

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node(n,p,TE,_) e d) = (trabEst e)++[(n,p)] ++(trabEst d)
trabEst (Node(n,_,_,_) e d) = (trabEst e) ++ (trabEst d)

--------------d
nota :: Numero -> Turma -> Maybe Classificacao
note _ Empty = Nothing
nota n (Node (num,_,_,cla)e d)
  |n < num = nota n e
  |n > num = nota n d
  |otherwise = Just cla

-------------------e
percFaltas :: Turma -> Float
percFaltas Empty = 0.0
percFaltas t = (aFaltas t / atotal t )* 100

aFaltas:: Turma -> Float
aFaltas (Node(_,_,_,Faltou) e d) =1+ aFaltas e + aFaltas d
aFaltas (Node (_,_,_,_) e d) = aFaltas e + aFaltas d 
aFaltas Empty= 0.0

atotal::Turma -> Float
atotal (Node i e d )= 1+ atotal e +atotal d
atotal Empty =0.0

-----------------d
mediaAprov :: Turma -> Float
mediaAprov Empty = 0.0
mediaAprov t = aprov t / aPtotal t *100

aprov :: Turma -> Float
aprov Empty = 0.0 
aprov (Node (_,_,_,Aprov x) e d ) =  fromIntegral x + aprov e + aprov d

aPtotal::Turma-> Float 
aPtotal  Empty = 0.0 
aPtotal (Node (_,_,_,Aprov x) e d) = 1+ aPtotal e + aPtotal d 

--------------------------e
aprovAv :: Turma -> Float
aprovAv Empty = 0.0
aprovAv turma = uncurry (/) (sumAprovAv turma)

sumAprovAv ::Turma  -> (Float,Float)

sumAprovAv (Node (_,_,_,cla)e d )= 
    case cla of Aprov nota -> (ap+1,av+1)
                Rep        -> (ap,av+1)
                _          -> (ap,av)
     where 
        (ap,av) = addPairs (sumAprovAv e) (sumAprovAv d)
        addPairs::(Float,Float)->(Float,Float)-> (Float,Float)
        addPairs (a,b) (c,d) = (a+c,b+d)      