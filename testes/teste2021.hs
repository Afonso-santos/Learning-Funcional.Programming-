{-
teste 20/janeiro/20-21
-}
--------1
pp:: Eq a => [a] -> [a] ->[a]
pp [] []= []
pp l1 [] = l1 
pp [] l2 = []
pp (h:t) (x:xs)
  |h==x = pp t xs 
  |otherwise = h: pp t (x:xs)  

---------2
type MSet a = [(a,Int)]
-----------------a
removeMSet:: Eq a => a -> [(a.Int)]-> [(a,Int)]
removeMSet _ [] =[]
removeMSet x ((n,num):t)
  |x == n = if num > 1 then (n, num-1):t else t 
  |otherwise = (n,num): removeMSet x t 

----------------b
calcula :: MSet a -> ([a],Int)
calcula [] =( , )
calcula l = foldr (\(n,num) (lett,na) -> (n:lett,na+num)) ([] ,0) l

----------------------3

parte ::String -> Char ->[String]
parte [] _ = []
parte (h:t) x = 
    case span (\= x) (h:t) of 
        (" ",rest) -> partes (tail rest) x
        (rest," ") -> [rest] 
        (cois,rest)-> [cois] : partes (tail rest) x 

----------------------4

data BTree a = Empty | Node a (BTree a ) (BTree a )
---------------------a
remove :: Ord a => a -> BTree -> Btree a 
remove _ Empty =Empty
remove x (Node r e d )
    | x > r = Node r e (remove x d) 
    | x < r  = Node r (remove x e) d 
    |otherwise =
      case (e,d) of 
        (Empty,d)->d
        (e,Empty)-> e
        (e,d)-> Node a e g 
            where (a,g)= minSMin r 

minSMin::BTree a -> (a,Btree a) 
minSMin (Node r Empty d) = (e ,r)
minSMin (Node r e d ) = (a ,Node r g d )
    where (a,g)  = minSMin e 

---------------------b 
instance Show a => Show Btree a  where
    Show:: Btree a -> String 
    Show Empty = "*"
    Show (Node r Empty Empty) = "(*<-" ++ show r ++ "->*)"
    show (Node r e d) ="("++ Show e ++ "<-" ++ show r ++ "->"++ show d ++ ")"
     
---------5
sortOn ::Ord b => (a -> b ) -> [a] -> [a]
sortOn _ [] = [] 
sortOn f (h:t) = sortOn f s ++ [h] ++ sortOn f b 
  where (s,b)= foldr(\x (small,big) -> if f x > f h then (sm,x: bib) else (x :sm , bib) ) ([],[]) (h:t) 

---------6
data FileSystem = File Nome | Dir Nome  [FileSystem]
type Nome  = String 

---------------a
fichs:: FileSystem -> [Nome]
fichs (File n)  = [n]
fichs (Dir n [] ) = [] 
fichs (Dir n (h:t=)) = concatMap fichs (h:t)

--------------b
dirFiles ::FileSystem -> [Nome] -> Maybe [Nome]
dirFiles (File n ) [] = Just [n]
dirFiles (Dir n files ) [h] 
  | n == h = Just $ map(\(File n) -> n) $ filter(\x -> case x of File n -> True  
                                                                 _      -> False  
  |otherwise = Nothing
dirFiles(Dir n files) (h:t)
  |n== h  = getJust (map (\x -> dirFiles x t) files
  |otherwise =False 


getJust::[Maybe a] -> Maybe a 
getJust [] = Nothing
getJust (Just x :t) = Just x
getJust (Nothing : t) = getJust t 



------------c

listaFich:: FileSystem -> IO ()
listaFich f  =do
  path<-getLine
  case  dirFiles f path ( partes path  '/') of 
    Just files -> print files
    Nothing -> putStrLn "Não é uma diretori."++