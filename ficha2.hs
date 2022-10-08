module Ficha2 where
import Data.Char

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

--funA [2,3,5,1]
-- Soma ds quadrados de cada elemento 
-- 4+9+25+1 = 39

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
						     else (funB t)

--funB [8,5,12]
-- Se elem é par fica na lista otherwise sai
-- [8,12]

funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

-- funC [1,2,3,4,5]
-- [3,4,5]
-- [5]

-- Devolve ultimo elem se a lista é ímpar
-- senão devolve []

funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

{-- funD "otrec"

g [] "certo"

g ("o":[]) "trec"
g "to" "rec"
g "rto" "re"
g "erto" "r"
g "certo" [] -> "certo"
-}

--Exercício 2

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h*2): dobros t

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t) | c == h = 1 + numOcorre c t
				  | otherwise = numOcorre c t


positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | h > 0 = positivos t
	 			| otherwise = False


soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h >= 0 = h:soPos t 
 			| otherwise = soPos t


somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t 
			  | otherwise = somaNeg t


tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) | length (h:t) <= 3 = (h:t)
			  | otherwise = tresUlt t


segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y: segundos t  

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((a,b):t) | x == a = True
						 | otherwise = nosPrimeiros x t


sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+x,b+y,c+z) where (x,y,z) = sumTriplos t

--Exercício 3

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h: soDigitos t
 				| otherwise = soDigitos t 

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + minusculas t 
				 | otherwise = minusculas t

nums :: String -> [Int]
nums "" = []
nums (h:t) | isDigit h = digitToInt h : nums t 
		   | otherwise = nums t

--Exercicio 4
type Polinomio = [Monomio]
type Monomio = (Float,Int)


conta :: Int -> Polinomio -> Int
conta n lista = length [y | (x, y) <- lista, y == n]
{--
conta _ [] = 0
conta n ((a,b):t) | n == b = 1 + conta n t 
				  | otherwise = conta n t
				  --}

grau :: Polinomio -> Int
grau lista = maximum [b | (a, b) <- lista]

selgrau :: Int -> Polinomio -> Polinomio
selgrau n lista = [(x,y) | (x,y) <- lista, y == n]

--Perigoso Mandar dúvida
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((b,expo):ps) | expo > 0 = (b*fromIntegral expo,expo-1) : deriv ps
 				    | otherwise = deriv ps

calcula :: Float -> Polinomio -> Float
calcula n lista = sum [a*(n^b) | (a,b) <- lista]
{-
calcula x [] = 0
calcula x ((b,e):t) = b*(x^e) + calcula x t
--}

simp :: Polinomio -> Polinomio
simp lista = [(a,b) | (a,b) <- lista, a /= 0]
--simp [] = []
--simp ((b,e):t) | b == 0 = simp t 
-- 			   | otherwise = (b,e): simp t 

			   
mult :: Monomio -> Polinomio -> Polinomio
mult (b,e) lista = [(b*x, e+y) | (x,y) <- lista]
--mult _ [] = []
--mult (b,e) ((bs,es):t) = (b*bs,e+es): mult (b,e) t


--Fdd checkar a Math
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):t) = (sum [bs |(bs,es) <- selgrau e t] + b,e) : normaliza [(bo,eo) | (bo,eo) <- t, eo /= e]
{-}
normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' [(b,e)] = [(b,e)]
normaliza' ((b, e):(b2, e2):ps)
  | e == e2 = normaliza' ((b + b2, e) : ps)
  | conta e ps == 0 = (b, e) : normaliza' ((b2, e2) : ps)
  | otherwise = normaliza' ((b, e) : ps ++ [(b2, e2)])
--}

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza $ concat [ mult (a, b) p2 | (a, b) <- p1]
--produto [] _ = []
--produto (h1:t1) p2 = soma (mult h1 p2) (produto t1 p2) 


--quicksort 
-- Rever e pedir ajuda ao jbb
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((b,e):t) = ordena [(x,y) | (x,y) <- t, y <= e] ++ 
							 [(b,e)] ++ 
						   ordena [(x,y) | (x,y) <- t, y > e]

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena p1 == ordena p2
-- O resultado ordena p1 == ordena p2 -> True || False



)]
