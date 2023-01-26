data Hora = H Int Int deriving Show
type Etapa =(Hora, Hora)
type Viagem = [Etapa]

----------------1a)
comparacao:: Hora -> Hora -> Bool
comparacao(H h1 m1) (H h2 m2)
  |h2>h1 = True
  |h2==h1 && m2>m1 = True
  |otherwise = False

valida::Hora -> Bool
valida (H h m)=if h>=0 && h<= 24 && m>=0 && m<= 60 then True 
               else False 

bemCons:: Etapa -> Bool
bemCons (t1, t2) =  if comparacao t1 t2 == True && valida t1 ==True  && valida t2 ==True then True
                    else False
----------b)
etapacons:: Viagem -> Bool
etapacons []= True
etapacons (( H h1 m1,H h2 m2):t)
  |h1<h2 && bemCons(H h1 m1, H h2 m2)==True && etapacons t = True
  |h1==h2 && m1<m2  && bemCons(H h1 m1, H h2 m2)==True  && etapacons t = True
  |otherwise = False 
--------c)
tempViagem:: Viagem-> Etapa
tempViagem l = (fst( head l),  snd (last l))
-------d)
converterM ::Hora -> Int
converterM (H h m)= h*60 +m 

converterH::Int-> Hora
converterH x= H (div x 60) (mod x 60)




  

-----------------e)




calculaTemp ::Viagem->Hora
calculaTemp l =if etapacons l ==True then converterH(converterM(snd (tempViagem l)) - converterM( fst (tempViagem l)))
               else converterH 0

--------------------------3)
data Contacto = Casa Integer| Trab Integer| Tlm Integer| Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]
---------------------a)
acresEmail:: Nome -> String -> Agenda -> Agenda
acresEmail n e []= [(n, [(Email e)])]
acresEmail n e ((n1,c):t)
  |n == n1 = ((n , c ++ [Email e]):t)
  |otherwise = acresEmail n e t

--------------------------b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing 
verEmails n ((n1,c):t)
  | n== n1 = Just (findEmails c)
  | otherwise = verEmails n t

findEmails:: [Contacto]-> [String]
findEmails []= []
findEmails (Email e :t)=  (e : findEmails t)
findEmails (h:t) = findEmails t
  
--------------------c)
consTelefs :: [Contacto] -> [Integer]
consTelefs []= []
consTelefs(Casa c:t)=(c:consTelefs t)
consTelefs(Trab nt:t)= ( nt : consTelefs t)
consTelefs(Tlm np :t)= (np : consTelefs t) 
consTelefs( Email e :) = consTelefs t
--------------------d)
casa :: Nome -> Agenda -> Maybe Integer
casa
