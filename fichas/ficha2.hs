import Data.Char
-----2.a
dobros::[Float]->[Float]
dobros []=[]
dobros (h:t)=(2*h):dobros t

--------2.b)
num0corre:: Char-> String->Int 
num0corre _ [] = 0
num0corre c (h:t)= if h==c then 1+ num0corre c t
                   else num0corre c t
---------2.c)
positivos::[Int]-> Bool
positivos []= True
positivos (h:t)=if h>0 then  positivos t
                else False
--------2.d)
soPos::[Int]-> [Int]
soPos []=[]
soPos (h:t)=if h>0 then h:soPos t
            else soPos t 
-----2.e)
somaNeg::[Int]->Int
somaNeg []= 0
somaNeg (h:t)=if h<0 then h+ somaNeg t
              else somaNeg t
------2,f)
tresUlt::[a]->[a]
tresUlt []=[]
tresUlt [x]=[x]
tresUlt [x,y]=[x,y]
tresUlt [x,y,z]=[x,y,z]
tresUlt (_:t)= tresUlt t
--------------------2.g)
segundos::[(a,b)]->[b]
segundos []=[]
segundos ((_,xs):t)= xs :segundos t 
----------------------2.h)
nosPrimeiros::(Eq a)=> a -> [(a,b)]->Bool
nosPrimeiros _ [] = False
nosPrimeiros c ((x,_):t) =if c==x then True 
                           else nosPrimeiros c t 
---------------------2.i)
sumTriplos:: (Num a ,Num b, Num c)=>[(a,b,c)]-> (a,b,c)
sumTriplos []= (0,0,0)
sumTriplos [(a,b,c)]=(a,b,c)
sumTriplos ((a,b,c):(d,e,f):t)= sumTriplos ((a+d,b+e,c+f):t)

----------------------3.a) 
--- Não consigo testar 787
soDigitos::[Char]->[Char]
soDigitos []=[]
soDigitos(h:t)=if ord h>=48 && ord h<58 then h:soDigitos t 
               else soDigitos t 
---------------------3.b) 787
minusculas:: [Char]->Int
minusculas[]= 0
minusculas (h:t)=if ord h>=97 && ord h<123 then 1+ minusculas t
                 else minusculas t
----------------------3.c)
nums:: String->[Int]
nums []= []
nums (h:t)=if ord h>=48 && ord h<58 then( digitToInt h):nums t
           else nums t


----------------------4.a)
type Polinomio =[Monomio]
type Monomio = (Float,Int)

conta:: Int-> Polinomio -> Int 
conta _ [] = 0
conta g ((_,q):t)=if g==q then 1+conta g t 
                  else conta g t 
---------------------4.b)
grau:: Polinomio -> Int
grau []= 0
grau ((_,q):t)=if q>= grau t then q
               else grau t
---------------------4.c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ []=[]
selgrau x ((p,q):t)=if x==q then (p,q):selgrau x t
                    else selgrau x t 
------------------4.d)
deriv::Polinomio-> Polinomio
deriv []=[]
deriv((p,q):t)=if q>0 then (p*( fromIntegral q),q-1 ): deriv t 
               else deriv t 
-------------------4.e)
calcula ::Float-> Polinomio-> Float
calcula _ []= 0.0 
calcula x ((p,q):t)= if x==0 then 0.0
                   else p*(x^q) + calcula x t 
-------------------4.f)
simp::Polinomio-> Polinomio 
simp []= []
simp ((p,q):t)=if p>0 || p<0 then (p,q):simp t
               else simp t
--------------------4.g)
mult::Monomio-> Polinomio->Polinomio 
mult _ []=[]
mult (x,y) ((p,q):t) =if x==0 then []
                      else ((x*p,y+q):mult (x,y) t)
--------------------4.h)
normaliza::Polinomio -> Polinomio
normaliza []=[]
normaliza [(p,q)]=[(p,q)]
normaliza ((p,q):(r,s):t)|q==s =  normaliza (((p+r),q):t)
                         |(conta q t)==0 = (p,q):normaliza ((r,s):t)
                         |otherwise= normaliza ((p,q):(t++[(r,s)]))
--------------------4.i)
soma::Polinomio -> Polinomio -> Polinomio
soma p1 p2 =normaliza p1++p2
--------------------4.j)
produto::Polinomio-> Polinomio-> Polinomio
produto[] _ = []
produto (h:t) p2 = soma (mult h p2 ) (produto t p2)
--------------------4.k)
ordena::Polinomio->Polinomio
ordena []=[]
ordena(h:t)=insere h (ordena t)


insere::Monomio -> Polinomio->Polinomio
insere (x,y) []= [(x,y)]
insere (x,y) ((a,b):t)
  |y <=b = (x,y):(a,b):t
  |otherwise = (a,b):insere (x,y) t