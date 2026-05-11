module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


data Paquete = UnPaquete{
    destinatario :: String,
    peso :: Number,
    delicadeza :: Number,
    esUrgente :: Bool
} deriving (Show)

peluche :: Paquete
peluche = UnPaquete{
    destinatario = "Ket",
    peso = 1,
    delicadeza = 3,
    esUrgente = True
}

pintura :: Paquete
pintura = UnPaquete{
    destinatario = "Ursula",
    peso = 4,
    delicadeza = 9,
    esUrgente = False
}

torta :: Paquete
torta = UnPaquete{
    destinatario = "Osono",
    peso = 2,
    delicadeza = 6,
    esUrgente = False
}

--Punto 2
type Vuelo = [Paquete]
vueloNormal = [peluche,pintura,torta]

paquetesQuePuedeLlevar :: Vuelo -> Number -> Vuelo
paquetesQuePuedeLlevar vuelo nivelMagia = filter (esProtegible nivelMagia) vuelo

esProtegible :: Number -> Paquete -> Bool
esProtegible nivelMagia paquete = delicadeza paquete < nivelMagia

--Punto 3
data Clima = Llueve | Nevando | Soleado deriving (Show)

recargo ::  Clima -> Number -> Paquete -> Number
recargo clima nivelMagia paquete = peso paquete * nivelMagia + (factorClimatico clima)

factorClimatico :: Clima -> Number
factorClimatico Llueve = 30
factorClimatico Nevando = 50
factorClimatico Soleado = 0

--Punto 4
esAfortunado :: Vuelo -> Bool
esAfortunado = all traeBuenaSuerte 

traeBuenaSuerte :: Paquete -> Bool
traeBuenaSuerte = even.length.destinatario

--Punto 5
sonAccesibles :: Vuelo -> Vuelo
sonAccesibles = filter esAccesible 

esAccesible :: Paquete -> Bool
esAccesible = (<50).(recargo Llueve 10)

--Punto 6
dificilesDeManiobrar :: Vuelo -> Vuelo
dificilesDeManiobrar = filter esDificilManiobrar

esDificilManiobrar :: Paquete -> Bool
esDificilManiobrar paquete = peso paquete > (delicadeza paquete / 2)

{- 
(\x -> peso x > (delicadeza x / 2)) a esto se le denomina funcion lambda, 
la ventaja que brinda es poder definir funciones dentro de la misma funcion sin tener que declararla por fuera
-}

dificilesDeManiobrar2 :: Vuelo -> Vuelo
dificilesDeManiobrar2 = filter (\x -> peso x > (delicadeza x / 2))

--Punto 7
type Hechizo = (Paquete -> Paquete)

alivianar :: Hechizo
alivianar paquete = modificarPeso paquete

modificarPeso :: Paquete -> Paquete
modificarPeso paquete | peso paquete > 2 = paquete {peso = peso paquete - 2} 
                      | otherwise = paquete {peso = 0}

reforzar :: Number -> Hechizo
reforzar valor paquete = reducirDelicadeza valor paquete 

reducirDelicadeza :: Number -> Hechizo
reducirDelicadeza valor paquete = paquete {delicadeza = delicadeza paquete - valor}

hechizoDePaciencia :: Hechizo
hechizoDePaciencia paquete = paquete {esUrgente = False}

--Punto 8
type Catalogo = [Hechizo]

catalogoKiki :: Catalogo
catalogoKiki = [alivianar,hechizoDePaciencia]

catalogoPotente :: Catalogo
catalogoPotente = [alivianar,(reforzar 3),(reforzar 10)]

--Punto 9
simularHechizos :: Paquete -> Catalogo -> [Paquete]
simularHechizos paquete catalogo = map ($ paquete) catalogo

{-
como se busca aplicar un catalogo (lista de funciones) a un paquete y obtener un listado de resultados,
se espera que el map recorra cada hechizo y le aplique un paquete.
el operador $ hace eso, aplicarle un dato especifico a cada funcion de la lista.

el operador $ es el equivalente a hacer:

simularHechizos :: Paquete -> Catalogo -> [Paquete]
simularHechizos paquete catalogo = map (aplicarPaquete paquete) catalogo

aplicarPaquete :: Paquete -> Hechizo -> Paquete
aplicarPaquete paquete hechizo = hechizo paquete 
-}


