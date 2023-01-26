import Data.Char
import Text.Read (Lexeme(String))

---------1
digitAlpha :: String -> (String,String)
digitAlpha []= ([],[])
digitAlpha l = foldl digitAlphaux ("","") l
  where 
    digitAlphaux:: (String,String ) -> Char->(String ,String)
    digitAlphaux (alpha, digit) c
      |isAlpha c = (alpha ++[c],digit )
      |isDigit c =( alpha, digit ++[c])
      |otherwise = (alpha , digit )

digitAlpha':: String-> (String,String)
digitAlpha' l = (filter isAlpha  l, filter isDigit l)


digitAlpha'' ::String-> (String,String)
digitAlpha'' l = foldl aux ("","") l
  where aux :: (String,String)-> Char->(String, String)
        aux (alpha,digit) x = if (isDigit x) then (alpha, digit ++ [x]) else if (isAlpha x) then (alpha++[x],digit)else (alpha,digit) 
    

-----------2) 
nzp :: [Int] -> (Int,Int,Int)
nzp []= (0,0,0)
nzp (h:t)
      |x<0= (x+1,y,z)
      |x>0 =(x,y,z+1)
      |otherwise = (x,y+1,z)
    where (x,y,z)= nzp t

nzp':: [Int] -> (Int,Int,Int)
nzp' l = (length (filter negatives l), length ( filter neutros l),length ( filter positivos l))
  where
    negatives x = x<0
    neutros x = x==0
    positivos x = x>0

nzp'':: [Int] -> (Int,Int,Int)
nzp'' l= foldl (\ (x,y,z) n ->if (x<0 )then (x+1,y,z) else (if x==0 then (x,y+1,z) else (x,y,z+1)))


-------------3)
divMod :: Integral a => a -> a -> (a, a)
divMod x y = ((div x y),(mod x y ))


divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = foldl (\ (a,b) x ->(a-1,b-y)) 

-------------4
fromDigits :: [Int] -> Int
fromDigits []= 0
fromDigits l= foldl (\ x  r -> r + x*10)
----------5

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- init l]




{-------9
a)
  [x|x<- [1..2], mod (2*x) 2

b) 
  [(x,y)| x<-[1..5],y<-[1..5],x+y== 6]
c)




-}