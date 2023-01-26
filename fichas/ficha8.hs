import Data.Time.Format.ISO8601 (yearFormat)
import GHC.CmmToAsm.AArch64.Instr (x0)
import Data.List (sortBy)
{-
Classses de tp( n desperdiçar )
-> Eq    (==)   : i Eq 
-> Ord   (<=) ou compare   fazer este "<=" ou >= ou < ou >       : i Ord
-> Show  (show)  converter para string
-> Num   ( + , - , *) negative  signum FromInteger
-> Enum  (toEnum from Enum)
-> Read  (readPree)

sintaxe:
    instace

         
         where

instance EqNat where                     instance (Eq a)=> Eq (BTree a) where -}
data Frac = F Integer Integer 

------------1
-----------------a
normaliza :: Frac -> Frac
normaliza (F a b) 
  |b < 0 = normaliza (F (-a) (-b))
  |otherwise =  F (div a x) (div b  x)
    where 
      x = mdc  a b

mdc:: Integer-> Integer-> Integer
mdc x 0 = x
mdc 0 y = y 
mdc x y  =  mdc y (mod x y)

---------------b
instance(Eq Frac) where 

  (==)::Frac-> Frac -> Bool
  (==)(F a b)(F x y) 
    | a*y == b *x = True
    |otherwise = False  
     

------------c
instance ( Ord Frac) where 
  (<=)::Frac -> Frac -> Bool
  f1 <= fa = x1* b1 <= a1*y1
    
      where
        F x1 y1 = normaliza f1
        F a1 b1 = normaliza fa
------------d)
instance ( Show Frac ) where
  show:: Frac -> String
  show f = show a ++ "/" ++show b
    where F a b = normaliza f
-------------e
{-
instance (Eq a, Show a) => Num a where
(+), (*), (-) :: a -> a -> a
negate, abs, signum :: a -> a
fromInteger :: Integer -> a
-}
instance Num Frac where
  (+) ::Frac-> Frac -> Frac
  (+) f1 f2
    |b == y =  F (a+x) y  
    |otherwise = F (a*y + x*b) (b*y)
       where 
        (F a b )=normaliza f1
        (F x y )=normaliza f2
  (-)::Frac-> Frac-> Frac 
  (-) f1 f2
    |b == y = F (a-x) y
    |otherwise = F (a*y - x*b)  (b*y)
       where 
        (F a b )=normaliza f1
        (F x y )=normaliza f2
  (*)::Frac -> Frac-> Frac
  (*) (F a b)(F x y)= F (a*x) (b*y)

  negate:: Frac-> Frac
  negate (F a b) = normaliza (F (-a) b)

  abs:: Frac-> Frac
  abs (F a b) = normaliza (F (abs x) y)
    where (F x y) = normaliza(F a b)
  
  signum::Frac-> Frac
  signum (F a b)= F (signum x) 1
      where (F x y) = normaliza(F a b)

  fromInteger :: Integer-> Frac
  fromInteger x = F x 1
-----------------------------f
proMaior:: Frac -> [Frac]-> [Frac]
proMaior f1 l1
  |f2 * (F 2 1)< (head l2)= (head l2): proMaior f2 (tail l2)
  |otherwise = proMaior f2 (tail l2)
    where
      f2= normaliza f1
      l2 = map normaliza l1
  
------------------2

data Exp a = Const a
  | Simetrico (Exp a)
  | Mais (Exp a) (Exp a)
  | Menos (Exp a) (Exp a)
  | Mult (Exp a) (Exp a)
----------------a)
instance (Show a)=> Show (Exp a) where
  show:: Exp a-> String 
  show e = 
    case e of
      Const a     -> show a
      Simetrico a -> "-" ++show a
      Mais a b    -> "(" ++show a ++ " + " ++ show b++ ")"
      Menos a b   -> "(" ++show a ++ " - " ++ show b++ ")"
      Mult a b    -> "(" ++show a ++ " * " ++ show b++ ")"
-----------------b)
instance (Num a,Eq a)=> Eq(Exp a) where
    (==) :: Eq a => Exp a -> Exp a -> Bool
    a==b = solve a == solve b

solve:: Num a => Exp a-> a
solve (Const a)     = a
solve (Simetrico a) = - (solve a)
solve (Mais a b)    = solve a + solve b
solve (Menos a b )  = solve a - solve b
solve (Mult a b )   = solve a * solve b 

------------------c)

instance (Ord a, Num a) => Num ( Exp a) where 
  (+)::Num a=>Exp a->Exp a -> Exp a
  a + b = Mais a b 
  (-)::Num a=>Exp a->Exp a -> Exp a
  a-b = Menos a b
  (*)::Num a=>Exp a->Exp a -> Exp a
  a*b = Mult a b

  signum:: Exp a->  Exp a
  signum a 
    | solve a == 0 = Const(0)
    | solve a < 0 = Const (-1)
    |otherwise = Const ( 1)

  abs::(Ord a,Num a)=> Exp a-> Exp a
  abs (Const a)     = (Const (abs a)) 
  abs (Simetrico a) = abs a
  abs e 
    | solve e > 0 = e
    |solve e < 0 = negate e 
    | otherwise = 0

  fromInteger:: Integer -> Exp a
  fromInteger x = Const ( fromInteger x)

------------------------3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]
------------------------a
instance Ord Data where 
  compare:: Data -> Data -> Ordering
  compare (D d1 m1 a1) (D d2 m2 a2)
    |a1>= a2 && m1>=m2 && d1> d2 = GT
    |a1== a2 && m1==m2 && d1== d2 = EQ
    |otherwise = LT
-------------------------------b
instance Show Data where
  show:: Data-> String
  show (D d m a) = show a ++ "/" ++show m ++"/"++ show a

-----------------------------c
ordena :: Extracto -> Extracto
ordena (Ext x l) = Ext x (sortBy (\ (data1,_,_) ( data2,_,_)-> compare data1 data2) l)
------------------------------d)

































