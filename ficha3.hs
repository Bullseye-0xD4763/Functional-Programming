module Ficha3 where

data Hora = H Int Int deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]


-- Auxiliares ------------------------------------------------------------------------------------------------------------------
horaValida :: Hora -> Bool -- Se hora é válida
horaValida (H h m ) = elem h [0..23] && elem m [0..59]

horaDepois :: Hora -> Hora -> Bool --Se hora acontece depois
horaDepois h1 h2 = (horaToSeconds h1) < (horaToSeconds h2) 

horaToSeconds :: Hora -> Int 
horaToSeconds (H h m) = h*60*60 + m*60

secondsToHora :: Int -> Hora 
secondsToHora s = (H ((s `mod` 86400) `div` 3600) (((s `mod` 86400) `mod` 3600) `div` 60))


horastominutos (H h m) = h*60 + m 
minutostohoras m = (H (m `div` 60) (m `mod`60))
addmintohoras m (H h1 m1) = (H (h1 + m `div` 60) (m1 + m `mod`60))

diffhoras (H h1 m1) (H h2 m2) = (h2*60 + m2 - h1*60 + m1)


{-
numberOfHours = (input % 86400 ) / 3600 ;
numberOfMinutes = ((input % 86400 ) % 3600 ) / 60 
numberOfSeconds = ((input % 86400 ) % 3600 ) % 60  ;
--}

etapaToSeconds :: Etapa -> Int
etapaToSeconds (h1,h2) = horaToSeconds(h1) + horaToSeconds(h2)
-- Auxiliares ---------------------------------------------------------------------------------------------------------------------

--ex viagem [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]


--alínea A 
etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida ((H h1 m1),(H h2 m2)) = horaValida (H h1 m1) && (horaValida (H h2 m2)) && (horaDepois (H h1 m1) (H h2 m2))


--Alínea B
viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [e] = etapaBemConstruida e
viagemBemConstruida (e1:e2:t) | (etapaToSeconds e1) < (etapaToSeconds e2) = viagemBemConstruida t 
                              | otherwise = False

--Alínea C
calcularHoraPartidaChegada :: Viagem -> Etapa
calcularHoraPartidaChegada [] = (H 0 0, H 0 0)
calcularHoraPartidaChegada v = (fst(head v),snd(last v))

--Alínea D calcular tempo viagem efetivo MAL FEITO
calcularTempoViagemEfetivo :: Viagem -> Hora
calcularTempoViagemEfetivo [] = (H 0 0)
calcularTempoViagemEfetivo ((a,b):t) =  addmintohoras (diffhoras a b) (calcularTempoViagemEfetivo t)


-- Alínea E calcular tempo total de espera NON EXHAUSTIVE PATTERNS
calculaTempoEspera :: Viagem ->  Hora
calculaTempoEspera [] = (H 0 0) 
calculaTempoEspera ((a,b):(c,d):t) = addmintohoras (diffhoras b c) (calculaTempoEspera ((c,d):t))

--Alínea F
calculaTempoViagemTotal :: Viagem -> Hora
calculaTempoViagemTotal [] = (H 0 0)
calculaTempoViagemTotal [(h1,h2)] = secondsToHora (horaToSeconds (h2) - horaToSeconds (h1))
calculaTempoViagemTotal v = secondsToHora (horaToSeconds (snd (last v)) - horaToSeconds (fst (head v)))

--Exercício 2---------------------------------------------------------------------------------------------
data Ponto = Cartesiano Double Double 
           | Polar Double Double 
           deriving (Show,Eq)

type Poligonal = [Ponto]

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

distan :: Ponto -> Ponto -> Double -- distancia entre 2 pontos
distan (Cartesiano a b) (Cartesiano c d) = sqrt ((abcissas^2)+(ordenadas^2))
                                       where
                                        abcissas  = a-c 
                                        ordenadas = b-d

distan (Cartesiano a b) (Polar c d)      = sqrt ((abcissas^2)+(ordenadas^2))
                                       where
                                        abcissas  = a-(posx (Polar c d)) 
                                        ordenadas = b-(posy (Polar c d))

distan (Polar a b) (Polar c d)           = sqrt ((abcissas^2)+(ordenadas^2))
                                       where
                                        abcissas  = (posx (Polar a b)) - (posx (Polar c d))  
                                        ordenadas = (posx (Polar a b)) - (posy (Polar c d)) 

posx :: Ponto -> Double --  abcissa
posx (Cartesiano a b) = a
posx (Polar a b)      = a * acos b 


posy :: Ponto -> Double --  ordenada
posy (Cartesiano a b) = b
posy (Polar a b)      = a * asin b 


area :: Figura -> Double
area (Triangulo p1 p2 p3) =
                            let a = distan p1 p2
                                b = distan p2 p3
                                c = distan p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                            in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

area (Circulo _ r)        = pi * (r ^ 2)
area (Rectangulo p1 p2)   = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)

areaAux :: [Figura] -> Double
areaAux [] = 0.0
areaAux [x] = area x
areaAux (h:x:t) = area h + areaAux (x:t) 



---Mais auxiliares PTQP jbb que ficha de merda 
--Alinea A
calcula_comprimento :: Poligonal -> Double
calcula_comprimento (h:x:t) = (distan h x) + calcula_comprimento (x:t)

--Alinea B 
testa_poligonal_fechada :: Poligonal -> Bool
testa_poligonal_fechada [p1,p2]       = False
testa_poligonal_fechada [p1,p2,p3]    = p1 == p3
testa_poligonal_fechada (p1:p2:p3:t)  = testa_poligonal_fechada (p1:p3:t)

--Alinea C
triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3]    = [(Triangulo p1 p2 p3)]
triangula (p1:p2:p3:t)  = (Triangulo p1 p2 p3) : triangula (p1:p3:t)

--Alinea D
calcula_area_poligono_convexo:: Poligonal -> Double
calcula_area_poligono_convexo p = areaAux (triangula p)

--Alinea E
mover :: Poligonal -> Ponto -> Poligonal
mover l p = p : l

--Alinea F
zoom :: Double -> Poligonal -> Poligonal
zoom _ []                   = []
zoom x ((Cartesiano a b):t) = (Cartesiano (a*x) (b*x)) : (zoom x t)
zoom x ((Polar a b):t)      = (Polar (a*x) b)          : (zoom x t)

---Exercício 3----------------------------------------------------------------------------------------------------------
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
     deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
verEmails :: Nome -> Agenda -> Maybe [String]
consTelefs :: [Contacto] -> [Integer]
casa :: Nome -> Agenda -> Maybe Integer

--Exercício 4-----------------------------------------------------------------------------------------
type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
            deriving Show

type TabDN = [(Nome,Data)]

--Exercício 5-------------------------------------------------------------------------------------------
data Movimento = Credito Float | Debito Float
                 deriving Show

data Data = D Int Int Int
            deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
                deriving Show
