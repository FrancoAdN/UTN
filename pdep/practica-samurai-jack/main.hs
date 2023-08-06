import Text.Show.Functions

type Transformacion = Personaje -> Personaje
data Elemento = Elemento {
    tipo:: String, ataque:: Transformacion, defensa:: Transformacion 
} deriving Show

data Personaje = Personaje {
    nombre:: String, salud:: Float, elementos:: [Elemento], anioPresente:: Int 
} deriving Show


{- Punto 1 -}
mandarAlAnio:: Int -> Transformacion
mandarAlAnio anio pj = pj { anioPresente = anio}

modificarSalud::  Float -> Personaje -> Personaje
modificarSalud nSalud pj = pj { salud = nSalud }

meditar:: Transformacion
meditar pj = modificarSalud ((salud pj) * 1.5) pj

causarDanio:: Float -> Transformacion
causarDanio cantidad pj = modificarSalud (((salud pj) - cantidad) `max` 0) pj

{- Punto 2 -}
esMalvado::Personaje -> Bool
esMalvado pj = any (("Maldad"==).tipo) $ (elementos pj)

aplicarAtaque:: Elemento-> Personaje-> Personaje
aplicarAtaque elemento pj = (ataque elemento) pj


danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce pj elemento = (salud pj) - ((salud.(aplicarAtaque elemento)) $ pj)

estaMuerto  = ((==0).salud)

type Enemigo = Personaje

elementosMataPersonaje:: Personaje -> [Elemento] -> Bool
elementosMataPersonaje _ [] = False
elementosMataPersonaje pj (act:sig) | (estaMuerto.(aplicarAtaque act) $ pj) = True
                                   | otherwise = elementosMataPersonaje pj sig


enemigoMataPersonaje:: Personaje -> Enemigo -> Bool
enemigoMataPersonaje pj enemigo = elementosMataPersonaje pj (elementos enemigo)

enemigosMortales::Personaje -> [Enemigo] -> [Enemigo]
enemigosMortales pj enemigos = filter (enemigoMataPersonaje pj) enemigos


{- Punto 3 -}
concentracion :: Int -> Elemento
concentracion n = Elemento "Magia" id (foldr1 (.) (replicate n meditar))

esbirros::Elemento
esbirros = Elemento "Maldad" (causarDanio 1) id

esbirrosMalvados::Int -> [Elemento]
esbirrosMalvados n = replicate n esbirros

jack::Personaje
jack = Personaje {
    nombre = "Jack",
    salud = 300,
    elementos = [
        (concentracion 3),
        Elemento "Magia" (causarDanio 1000) id
    ],
    anioPresente = 200
}

portalAlFuturoDesde:: Int -> Elemento
portalAlFuturoDesde anio = Elemento "Magia" (mandarAlAnio (2800 + anio)) (aku (2800 + anio).salud)

aku :: Int -> Float -> Personaje 
aku anio salud = Personaje {
    nombre = "Aku",
    salud = salud,
    anioPresente = anio,
    elementos = concentracion 4 : portalAlFuturoDesde anio : esbirrosMalvados (100 * anio)
}


{- Punto 4 -}
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
 |estaMuerto atacante = (defensor, atacante)
 |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)

-- Abstraemos cómo hacer para usar uno de los efectos de un conjunto de elementos sobre un personaje
usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afectar personaje funcion = funcion personaje
afectar' = flip ($)