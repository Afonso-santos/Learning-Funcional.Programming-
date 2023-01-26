
import Data.List
{-
IO
   | getChar (le 1 char)
le | getLine  (le +1 char)
   
           |putChar  ( escrever 1 char)
 escrevre  |putStr   ( escrever +1 char)
           |putStrLn (escrever +1 char e linha em branco)
           |print ( putStrln + show)



randomIO (1 nª aleatorio)
randomRIO (a,b)  ( 1 nº aleatorio entre a e b )

do
  repetir:: IO()
  repetir = do print("Escreve 1 nª")
             x<- getChar
             print x ++ " " ++ x
-}
import System.Random
import GHC.CmmToAsm.AArch64 (ncgAArch64)

{---------1

randomIO :: Random a => IO
randomRIO :: Random a => (a,a) -> IO

-------------a -}
bingo :: IO ()
bingo = do aux []

aux::[Int]-> IO()
aux l 
  |length l == 90 = print ("Jogo Terminou")
  |otherwise = do n<-randomRIO ((1, 90))
                  if elem n l then aux l else print(n)
                  getChar 
                  aux (n:l)

--------------b
{-
mastermind :: IO ()
mastermind = do c <-geraCodigo 
                nc<-getLine
                r <-verificaCodigo c nc 0
                print("Tem " ++   r   ++" digitos na posiçao correta" )
                print("Tem " ++ (4-r) ++ "digitos na posição errada")

verificaCodigo:: [Int]-> [Int] -> Int-> Int
verificaCodigo (h:t) (x:xs) acc
  |h== x =verificaCodigo t xs  (acc+1)
  |otherwise = verificaCodigo t xs acc
verificaCodigo [] [] acc = acc


geraCodigo::[Int]->IO[Int]
geraCodigo l
  | length l == 4 = print( l) 
  |length l <= 4 = do x<- randomRIO (1, 9)
                      geraCodigo (l++[x])

-}
------------------2
data Aposta = Ap [Int] (Int,Int)

-------------------a

valida :: Aposta -> Bool
valida (Ap num@[n1,n2,n3,n4,n5] (e1,e2))=all (elem[1..50 ]) num && nub num== num && e1/=e2 && elem[1..12] e1 && elem[1..12] e2  
valida _ = False













{-
bingo :: IO ()
bingo = do  acumula []
            
            
acumula ::[Int]-> IO ()
acumula l 
  | length l == 90 = print "Feito"
  | otherwise =  do v<-randomRIO(1,90)
                    if (elem v l) then  acumula l else  print v
                    getChar 
                    acumula (v:l)-}