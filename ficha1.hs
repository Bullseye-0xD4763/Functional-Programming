module Ficha1 where
import Data.Char

--Exercicio 1
perimetro :: Double -> Double
perimetro r = 2*pi*r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo m n | m `mod` n == 0 = True
 			 | otherwise = False


truncaImpar :: [a] -> [a]
truncaImpar l | length l `mod` 2 == 0 = l
			  | otherwise = tail l


max2 :: Int -> Int -> Int
max2 a b | a < b = b 
		 | a > b = a 
		 | otherwise = a 

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z
--max3 x y = max2 (max2 x y) também funcina

--Exercicio 2
nRaizes a b c | delta > 0 = 2
			  | delta == 0 = 1
			  | otherwise = 0
			  where delta = (b^2-4*a*c)

raizes a b c| (nRaizes a b c) >  1 = [((-b) - f)/2*a,((-b) + f)/2*a]
			| (nRaizes a b c) == 1 = [(-b)/2*a]
 			| otherwise = []
 			where f = sqrt(b^2 - 4*a*c)

--Exercício 3
type Hora = (Int, Int)

horavalida (h,m) = elem h [0..23] && elem m [0..59] 

horadepois (h1,m1) (h2, m2) = if (horavalida (h1,m1) && horavalida(h2,m2))
							  then if (m1 >= m2 && h1 >= h2)
							  	   then False
							  	   else True
							  else error "Hora inválida bacano"
							  


horastominutos (h,m) = h*60 +m

minutostohoras m = (m `div` 60, m `mod` 60)

diffhoras (h1,m1) (h2,m2) = (horastominutos(h2,m2) - horastominutos (h1,m1))

addmintohoras minuto (h,m) = (h + minuto `div` 60 ,m + minuto `mod` 60)

--Execício 4
data Hora2 = H Int Int deriving (Show, Eq)

horavalida' (H h m) = elem h [0..23] && elem m [0..59]

(H h1 m1) `horaDepois` (H h2 m2) = (h1,m1) <= (h2,m2)

horastominutos' (H h m) = h*60 + m 

minutostohoras' m = (H (m `div` 60) (m `mod`60))


diffhoras' (H h1 m1) (H h2 m2) = (h2*60 + m2 - h1*60 + m1)

addmintohoras' m (H h1 m1) = (H (h1 + m `div` 60) (m1 + m `mod`60))

--Exercício 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
nest s = case of s Verde -> Amarelo
				   Amarelo -> Vermelho
				   Vermelho -> Verde


stop :: Semaforo -> Bool
stop s | s == Vermelho = True
	   | otherwise = False


safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Vermelho || s2 == Vermelho

--Exercício 6
data Ponto = Cartesiano Double Double | Polar {distPonto :: Double, anguloPonto :: Double} deriving (Show,Eq)


posx :: Ponto -> Double
pox ponto = case ponto of Cartesiano x _ -> x
 						  Polar d a -> if a == pi/2 then 0 else d * cos a	 

posy :: Ponto -> Double
posy ponto = case ponto of Cartesiano _ y -> y
                           Polar d a -> if a == pi then 0 else d * sin a

raio :: Ponto -> Double
raio ponto = case ponto of Cartesiano x y -> sqrt $ x^2 + y^2
                           Polar d _ -> d
--sqrt 4 + sqrt 4 = 4
--sqrt 4 + 4 = 6
--sqrt $ 4 + 4 = √(4+4) = 2.82842712 = sqrt (4+4) = sqrt 8 
-- o $ aplica a ação ao conjunto dado

angulo :: Ponto -> Double
angulo ponto = case ponto of Cartesiano x y -> if x < 0 && y == 0 then pi else
                                               if x < 0 then pi + atan (y/x) else
                                               atan (y/x)
                             Polar _ a -> a

distponto :: Ponto -> Ponto -> Double
distponto p1 p2 = sqrt (((posx p1 - posx p2)^2) + (posy p1 - posy p2)^2)

--Exercício 7

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto 
            deriving (Show, Eq)

poligono :: Figura -> Bool
poligono (Circulo c r) = r > 0 -- Verificia que o raio é positivo
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2 --Verifica que os pontos nao tem o mesmo x ou y
poligono (Triangulo p1 p2 p3) = (posy p2 - posy p1) / (posx p2 - posx p1) /= (posy p3 - posy p2) / (posx p3 - posx p2) -- Verifica que os pontos não pertencem todos à mesma reta


vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices retang@(Retangulo p1 p2) = if poligono retang then [(p1), (Cartesiano (posx p1) (posy p2)), (Cartesiano (posx p2) (posy p1)), (p2)] else []
vertices triang@(Triangulo p1 p2 p3) = if poligono triang then [p1, p2, p3] else []

--fun (n,(x:xs)) = (x(n,(x:xs))) é o mesmo que ..
--fun par@(n,(x:xs)) = (x,par)    e o mesmo que
--fun (n(x:xs)) = let par = (n,(x:xs))
--                in (x,par)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distponto p1 p2
        b = distponto p2 p3
        c = distponto p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron
area (Circulo _ r) = pi * (r ^ 2)
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1) 

perimetro' :: Figura -> Double
perimetro' (Circulo _ r) = 2 * pi * r
perimetro' (Retangulo p1 p2) = 2 * abs (posx p2 - posx p1) + 2 * abs (posy p2 - posy p1)
perimetro' (Triangulo p1 p2 p3) = distponto p1 p2 + distponto p2 p3 + distponto p1 p3

--Exercício 8

isLower' :: Char -> Bool
isLower' ch = elem (ord ch) [97..122]
--ord :: Char -> Int // converte char no valor da tabela ascii

isDigit' :: Char -> Bool
isDigit' ch = elem (ord ch) [48..57]

isAlpha' :: Char -> Bool
isAlpha' ch = elem (ord ch) [97..122] || elem (ord ch) [65..90]

toUpper' :: Char -> Char
toUpper' ch | isLower' ch = chr ((ord ch) - 32 )
            | otherwise = ch

intToDigit' :: Int -> Char
intToDigit' n = chr (n+48)

digitToInt' :: Char -> Int
digitToInt' ch = (ord ch) - 48 