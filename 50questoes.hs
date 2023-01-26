import GHC.CmmToAsm.AArch64.Instr (x0, Instr, Target (TReg))
import GHC.Plugins (pprFastFilePath, tYPE_ERROR_ID, DynFlags (includePaths), unSaturatedOk)
import GHC (l2l, HsValBindsLR (ValBinds), convertLit)
import Data.Char 
import System.Info (compilerVersion)
------------------1
enumFromTo1:: Int ->Int -> [Int]
-- 2,4 =[2,3,4]
-- 5 2 = [5,4,3,2]
enumFromTo1 x y
  |x<y = x:enumFromTo1(x+1) y 
  |x>y = x:enumFromTo1(x-1) y
  |otherwise = [x]
------------2
enumFromThenTo' :: Int -> Int-> Int -> [Int]
enumFromThenTo' x y z
  |x> z && y >= x || x < z && y<= x =[]
  |x <= z = x: enumFromThenTo' y (y-x+ y) z
  | otherwise = []
---------------3
pp:: [a] -> [a] -> [a]
pp [] []= []
pp l1 [] = l1
pp [] l2 =l2 
pp (h:t) (a:b) =h:(pp t (a:b))
---------------4
bb::Eq a =>[a] -> Int -> a  

bb  (h:t) x 
  | x==0 = h
  |otherwise = bb t (x-1)
{-bb (h:_) 0 =h
bb (h:t) x =bb(h:t) (x-1)-}

--------------5
re :: [a] -> [a]
re []=[]
re l =last l : re (init l)
--------------6
teka :: Int -> [a] -> [a]
teka  x []= []
teka n (h:t)
  |n >0 = ((h):teka (n-1) (t))
  |otherwise = []
-------------7
drip :: Int -> [a] -> [a]
drip  _ []=[]
drip n (_:t)
  |n>0 = drip (n-1) t
  |otherwise =[]
------------8
zap :: [a] -> [b] -> [(a,b)]
zap _ [] = [] 
zap [] _ =[]
zap (h:t) (a:b) =((h,a)):zap t b
-----------9
repil :: Int -> a ->[a]
repil 0 _ = []
repil n x 
  |n>0 = x: repil (n -1) x
  |otherwise = []

----------10
intel :: a -> [a] ->[a] 
intel _ [] =  []
intel _ [h]= [h]
intel x (h:t)= (h:x:intel x t)
--------------------------------------------------------------------------------11
gruop ::Eq a=> [a] -> [[a]]
gruop []= []
gruop [h]=[[h]]
gruop (h:t)
  |elem h (head r) = ( h :(head r)):tail r 
  |otherwise = [h]:r 
  where r= gruop t 




------------12
condog::[[a]]->[a]
condog []= []
condog (h:t)= h ++condog t 

-----------13
inuts::[a]->[[a]]
inuts []= [[]]
inuts l = inuts(init l) ++ [l]

------------14
sonic :: [a] -> [[a]]
sonic [] = [[]]
sonic l = [l] ++ sonic( tail l)
------------15
heads' :: [[a]] -> [a]
heads' [] =[]
heads' (h:t) = take 1 h ++ heads' t
----------16
tatol :: [[a]] -> Int
tatol [] = 0 
tatol (h:t)= length h + tatol t
----------17
fan :: [(a,b,c)] -> [(a,c)]
fan []= []
fan ((a,b,c):t)= (a,c) : fan t
---------18
coka :: [(String,b,c)] -> String 
coka []=[]
coka ((string,b,c):t)= string ++ coka t
----------19
idade :: Int -> Int -> [(String,Int)] -> [String] 
idade a i [] = []
idade a i ((n,an):t)
  |a-an >= i = n:idade a i t
  |otherwise= idade a i t 
---------20
power :: Int -> Int -> [Int]
power n 1 = [n]
power n m 
  |m>1 = power n (m-1) ++[n^(m-1)]
  |otherwise =[] 
-----------------------------------------------------------------------21
isPrime ::Int ->Bool
isPrime x 
  |x>= 2 = primecheck x 2
  |otherwise = False
primecheck::Int->Int->Bool
primecheck n m
  |m*m>n = True
  |mod n m == 0 =False 
  |otherwise = primecheck n (m+1)
---------------22
isPreOf :: Eq a => [a]-> [a] -> Bool 
isPreOf [] _ = True
isPreOf _ []= False 
isPreOf (h:t) (h1:t1)
  |h==h1= isPreOf t t1
  |otherwise = False 
--------------23
isSu :: Eq a => [a]-> [a] -> Bool
isSu [] [] = False 
isSu [] l2= True 
isSu _ []= False 
isSu l1 l2
  |last l1 ==last l2 = isSu (init l1) (init l2)
  |otherwise = False 
-----------------24
isSub :: Eq a =>[a] -> [a] -> Bool 
isSub [] _ = True
isSub _ [] = False 
isSub (h:t) (h1:t1) 
  | h==h1 = isSub t t1
  |otherwise = isSub (h:t) t1
------------------25
elemIndi :: Eq a => a ->[a] -> [Int]
elemIndi _ [] = []
elemIndi x l =elemaux x l 0

elemaux :: Eq a => a -> [a] -> Int -> [Int]
elemaux _ []_ = []
elemaux x (h:t) i
  |x==h= i: elemaux x t (1+i)
  |otherwise = elemaux x t (1+i) 

------------------26
noob :: Eq a => [a] -> [a]
noob []=[]
noob (h:t)
  |elem h t = noob t
  |otherwise =h:noob t 
-----------------27
gelete :: Eq a => a -> [a]-> [a]
gelete _ []= []
gelete x (h:t)
  |x==h = t
  |otherwise = h: gelete x t

-------------------28 ----- (\\)
mm:: Eq a => [a] -> [a]-> [a] 
mm [] [] = []
mm [] _ =[]
mm l []= l
mm l (h:t) = mm (gelete h l) t
--------------------29
onion :: Eq a => [a] -> [a]-> [a]
onion [] [] = []
onion [] l = l
onion l1 []= l1 
onion l1 (h:t)
  |elem  h l1 ==True = onion l1 t 
  |otherwise = l1 ++[h]
-------------------30
interstar:: Eq a => [a] ->[a] -> [a] 
interstar [] []= []
interstar l []=  l
interstar [] l2 = []
interstar (h:t) l2
 |elem h l2 == True =h: interstar t l2 
 |otherwise = interstar t l2 
--------------------31
incert :: Ord a => a -> [a]-> [a] 
incert x []= [x]
incert x (h:t)
  |x>=h = h:(incert x t)
  |otherwise = x:h:t
-----------------32
unowords :: [String] -> String
unowords []=[]
unowords (h:t)= h ++ " " ++unowords t 
-----------------33
unolines :: [String] -> String
unolines []= []
unolines(h:t)=h ++ "\n" ++ unolines t
----------------------------------------------------------34
pMaior :: Ord a => [a] -> Int
pMaior []= error "lista vazia"
pMaior [x]= 0
pMaior (h:t)
  |h>=maiorE =0
  |otherwise = maiorT +1
  where maiorE =t !! maiorT
        maiorT = pMaior t  
-------------------35
lukup :: Eq a => a -> [(a,b)]-> Maybe b 
lukup _ [] = Nothing
lukup x ((a,b):t)
  |x==a = Just b 
  |otherwise = lukup x t


------------------36
preCrescente :: Ord a => [a] -> [a]
preCrescente []= []
preCrescente [x]=[x]
preCrescente (h:x:t)
 |h<=x = h:preCrescente (x:t) 
 |otherwise = [h]
------------------------------------------37
iSort :: Ord a => [a] -> [a]
iSort []= []
iSort (h:t) = incert h (iSort t) 
------------------38
menor :: String -> String -> Bool
menor [] []= False
menor l1 [] = True
menor [] l2 = True
menor (h:t) (h1:t1)
  |h>h1 = False
  |h<h1 = True
  |otherwise = menor t t1
----------------39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ []= False
elemMSet x ((a,b):t)
  |x==a = True
  |otherwise = elemMSet x t 

-----------------40
converteMSet :: [(a,Int)] -> [a]
converteMSet []=[]
converteMSet ((a,b):t)
  |b>0 = a:converteMSet ((a,(b-1)):t)
  |otherwise = converteMSet t

------------------41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x []= [(x,1)]
insereMSet x ((a,b):t) = if x==a then ((a,b+1):t) else (a,b):insereMSet x t
  
---------------42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ []= []
removeMset x ((a,b):t)
  |x==a = if b>1 then ((a,b-1):t) else t 
  |otherwise= (a,b):removeMSet x t
---------------------------------------43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet []= []
constroiMSet (h:t)=  insereMSet h ( constroiMSet t)
-------------44
partitionE :: [Either a b] -> ([a],[b])
partitionE []=([],[])
partitionE ((Left a):t)= (a:as,bs)
  where (as,bs) = partitionE t 
partitionE ((Right b):t) = (as,b:bs)
  where (as,bs)= partitionE t
------------45
dogMaybes :: [Maybe a] -> [a]
dogMaybes []=[]
dogMaybes (Nothing:t)=dogMaybes t
dogMaybes (Just h:t)= h : dogMaybes t 
------------46
data Movimento = Norte | Sul | Este | Oeste deriving Show
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1)
  |x1>x = Este :caminho (x+1, y) (x1,y1)
  |x1<x = Oeste :caminho (x-1,y) (x1,y1)
  |y1>y = Norte :caminho (x,y+1) (x1,y1)  
  |y1<y = Sul :caminho (x,y-1) (x1,y1)
  |otherwise = []
------------47
hasLoops :: (Int,Int) -> [Movimento] -> Bool 
hasLoops (x,y) []= False
hasLoops (x,y) l 
  |(x,y)== (posicao (x,y) l) =True 
  |otherwise = hasLoops (x,y) (init l)


posicao:: (Int,Int)-> [Movimento]->(Int,Int)
posicao (x,y) []= (x,y)
posicao (x,y) (Norte:t)= posicao (x,y+1) t
posicao (x,y) (Sul:t) =posicao (x,y-1) t
posicao (x,y) (Este:t) = posicao (x+1, y) t
posicao (x,y) (Oeste:t) =posicao (x-1,y) t




-----------49
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto deriving Show 
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0.0
areaTotal ((Rect (x1,y1) (x2,y2)):t)= abs (x1-x2) * abs(y1-y2)+ areaTotal t 
-----------48
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] =0
contaQuadrados ((Rect (x1,y1) (x2,y2):t))
  |abs (x1-x2) == abs(y1-y2) = 1+contaQuadrados t
  |otherwise = contaQuadrados t




---------50
data Equipamento = Bom | Razoavel | Avariado deriving Show
naoReparar :: [Equipamento] -> Int
naoReparar []= 0
naoReparar (Bom:t)= 1+naoReparar t
naoReparar (Razoavel:t) =1+ naoReparar t
naoReparar (Avariado:t) = 0+ naoReparar t


----------------------------testes
eads:: [[a]]->[a]
eads  [[]]=[]
eads ((h:t):ts)=h: eads ts