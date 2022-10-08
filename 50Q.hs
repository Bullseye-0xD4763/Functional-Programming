module CinquentaQuestoes where

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo a b | a <= b = a: myenumFromTo (a+1) b
			     | otherwise = []

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c | a <= c = a: myenumFromThenTo (a+b) b c
					   | otherwise = []   


concatena :: [a] -> [a] -> [a]
concatena [] l = l
concatena (h:t) l = h: concatena t l


p4 :: [a] -> Int -> a
p4 (h:t) 0 = h 
p4 (h:t) n = p4 t (n-1)


myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]


mytake :: Int -> [a] -> [a]
mytake 1 (h:t) = [h]
mytake n (h:t) = h: mytake (n-1) t

mydrop :: Int -> [a] -> [a]
mydrop 1 (h:t) = t  
mydrop n (h:t) = mydrop (n-1) t


myzip :: [a] -> [b] -> [(a,b)]
myzip [] _  = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y): myzip xs ys 

myreplicate :: Int -> a -> [a]
myreplicate 1 a = [a]
myreplicate n a = a: myreplicate (n-1) a

myintersperse :: a -> [a] -> [a]
myintersperse a [] = []
myintersperse a [x] = [x]
myintersperse a (h:t) = h : a : myintersperse a t 


mygroup :: Eq a => [a] -> [[a]]
mygroup (h:t) = (h:takeWhile (==h) t) : mygroup (dropWhile (== h) t)

{--
mygroup [] = []
mygroup	(h:t) = aux [h] t where
	aux a [] = [a]
	aux a (h:t) | elem h a = aux (h:a) t
			    | otherwise = a: aux [h] t-

dropWhile (==1) [2,2,3,4,4,4,5,4,1]
 [1,2,2,3,4,4,4,5,4,1]
 1: mygroup (dropwhile (==1) [2,2,3,4,4,4,5,4,1])
 [[1],[2,2]]: mygroup (dropWhile (==2) [3,4,4,4,5,4,1])
-}

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat t

myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l]

mytails :: [a] -> [[a]]
mytails [] = [[]]
--mytails (h:t) = [h:t] ++ mytails t
mytails l = l : mytails (tail l)


myheads :: [[a]] -> [a]
myheads [] = []
myheads (h:t) = (head h) : myheads t
--myheads l = (head (head l)): myheads (tail l)


total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t 

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c): fun t

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((string,b,c):t) = string ++ cola t

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano age ((s,a):t) | (ano-a) >= age = s: idade ano age t 
						| otherwise = idade ano age t


powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = [n^a | a <- [0..(m-1)] ]

{--
isPrime :: Int -> Bool
isPrime n | n > 2 = primeCheck n 2
		  | otherwise = False

checkPrime :: Int -> Int -> Int
checkPrime n m | m * m > n = True -- equivalente a: m > √n (assim trabalhamos apenas com valores inteiros)
   			   | mod n m == 0 = False
   			   | otherwise = checkPrime n (m + 1)
-}



primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True -- equivalente a: m > √n (assim trabalhamos apenas com valores inteiros)
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)


isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (h:t) | x == h = isPrefixOf xs t
						| otherwise = False  


isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l lmain | last l == last lmain = isSuffixOf (init l) (init lmain)
				   | otherwise = False

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (h:t) | x == h = isSubsequenceOf xs t
							 | otherwise = isSubsequenceOf (x:xs) t
{---[10,30] [10,20,30,40]
	10 = 10 -> [30] [20,30,40]
	30 /= 20 -> [30] [30,40]
	30 = 30 -> [] = [40] : True
	
	[30,10] [10,20,30,40]
	30 /= 10 -> [30,10] [20,30,40]
	30 /= 20 -> [30,10] [30,40]
	30 = 30 -> [10] [40]
	10 /= 40 -> [10] [] : False
---}

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x [] = []
elemIndices x l = function 0 x l 
	where 
		function _ _ [] = []
		function n x (h:t) | x == h = n : function (n+1) x t 
		  				   | otherwise = function (n+1) x t


elemIndices' :: Eq a => a -> [a] -> [Int] 
elemIndices' _ [] = []
elemIndices' n (h:t) | n == h = 0 : map (+1) (elemIndices' n t)
                     | otherwise = map (+1) (elemIndices' n t)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) = h : nub (filter (/= h) t)

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t) | h == x =  t 
			   | otherwise = h : delete x t


(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) [] _ = []
(\\) l (x:xs) = (\\) (delete x l) xs


union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union [] l = l
union l (h:t) | elem h l = union l t
			  | otherwise = union (l ++ [h]) t





intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (h:t) l | elem h l = h : intersect l t 
				  | otherwise = intersect l t

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) | x <= h = x : h : t
		       | otherwise = h : insert x t


myunwords :: [String] -> String
myunwords [] = ""
myunwords [x] = x
myunwords (h:t) = h ++ " " ++ myunwords t

myunlines :: [String] -> String
myunlines [] = ""
myunlines [x] = x ++ "\n"
myunlines (h:t) = h ++ "\n" ++ myunlines t

--Dúvidas
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0 -- [_] → lista com um elemento. Underscore substitui nome de variável
pMaior (h:t) | h > (t !! (pMaior t)) = 0 
			 | otherwise = 1 + (pMaior t)

mylookup :: Eq a => a -> [(a,b)] -> Maybe b
mylookup _ [] = Nothing
mylookup x ((a,b):t) | x == a = Just b 
					 | otherwise = mylookup x t  

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:t) | h <= (head t) = h : preCrescente t 
				   | otherwise = [h]


iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

menor :: String -> String -> Bool
menor [] [] = False
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys) = menor xs ys


elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False
elemMSet x ((a,b):t) | x == a = True
					 | otherwise = elemMSet x t

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,0):t) = converteMSet t
converteMSet ((a,b):t) = a : converteMSet ((a,b-1):t)


insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) | x == a = (a,b+1) : t
					   | otherwise = (a,b) : insereMSet x t


removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t) | x == a && b == 1 = removeMSet x t
					   | x == a = (a,b-1) : removeMSet x t
					   | otherwise = (a,b) : removeMSet x t

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (h, 1 + length (takeWhile (==h) t)) : constroiMSet (dropWhile (==h) t)

--Dúvida---- WTf are eithers
{--partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of 
                           Left a -> ((a:x),y)
                           Right b -> (x,(b:y))
                           where (x,y) = (partitionEithers t)


--}
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a : t) = a : catMaybes t
catMaybes (Nothing : t) = catMaybes t


data Movimento = Norte | Sul | Este | Oeste deriving Show


posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte : t) = posicao (x,y+1) t
posicao (x,y) (Sul : t) = posicao (x,y-1) t
posicao (x,y) (Este : t) = posicao (x+1,y) t
posicao (x,y) (Oeste : t) = posicao (x-1,y) t



caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1) | y < y1 = Norte : caminho (x,y+1) (x1,y1)
					  | y > y1 = Sul : caminho (x,y-1) (x1,y1)
					  |	x < x1 = Este : caminho (x+1,y) (x1,y1)
					  | x > x1 = Oeste : caminho (x-1,y) (x1,y1)
				 	  | otherwise = []

--N sei esta merda
--hasLoops :: (Int,Int) -> [Movimento] -> Bool
--hasLoops (x,y) (h:t) | 

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops pi ms | pi == posicao pi ms = True
			   | otherwise = hasLoops pi (init ms)

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

--contaQuadrados :: [Rectangulo] -> Int
--contaQuadrados rect (x,y) (a,b) |

data Equipamento = Bom | Razoavel | Avariado deriving Show




naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of
					Bom -> 1 + naoReparar t
					Razoavel -> 1 + naoReparar t 
					Avariado -> naoReparar t