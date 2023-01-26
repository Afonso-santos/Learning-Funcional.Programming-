{-
Exame 17-18 / janeiro / 30
-}
-----------------------1
--- (!!) [10,20,30] 1 corresponde a 20.
pp:: [a] -> Int -> a 
pp (h:t) x 
    |x> 0 = pp t (x-1)
    | otherwise = h

-----------------2
data Movimento = Norte | Sul | Este | Oeste deriving Show
-----------------a
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) 
  |h== Norte = posicao (x,y+1) t
  |h== Sul   = posicao (x,y-1) t 
  |h== Este  = posicao (x+1,y) t 
  |otherwise = posicao (x-1,y) t 

------------------3
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False 
-----------------
any f l = elem True (map f l )
------------------- 

any f (h:t) 
  |f h == True = True 
  |otherwise = any f t 

-------------4
type Mat a = [[a]]
-----------------
triSup :: Num a => Mat a -> Bool



---------------5
movimenta :: IO (Int,Int)
movimenta =
    do n<- getStrln
       (x,y)<-direcao n (0,0)
        return ((x,y))

direcao:: [Char] -> (Int,Int) -> (Int,Int)
direcao [] (x,y) = (x,y)
direcao (h:t) (x,y) 
  |h== "N" = (x,y+1)
  |h== "S" = (x,y-1)
  |h== "E" = (x+1,y)
  |h== "O" = (x-1,y)
  |otherwise = (x,y)

---------------6

data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]
-----------------a
vazia :: Imagem -> Bool
vazia (Quadrado _ ) = True 
vazia (Mover (_,_) i) = vazia i 
vazia (Juntar l ) = if elem True (map vazia l) then True  else False   

------------b

maior :: Imagem -> Maybe Int
maior i = maximum' aux  

aux :: Imagem -> [Int] 
aux Quadrado x = x
aux (Mover (Int,Int) i) = aux i 
aux (Juntar l ) = map aux l 

maxinum' :: [Int] -> Maybe Int
maximum' [] = Nothing 
maximum' l  = Just maximum l 

-----------c

