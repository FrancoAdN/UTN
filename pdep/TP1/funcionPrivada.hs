type Actor = String;
data Filmacion = Filmacion { titulo :: String, puntaje :: Int, lanzamiento :: Int, duracion :: Int, actores :: [String]} deriving Show 

armaMortal = Filmacion "Arma Mortal" 7 1987 109 ["Mel Gibson", "Danny Glover", "Gary Busey"]
reinas = Filmacion "9 Reinas" 8 2000 114 ["Gastón Pauls", "Ricardo Darín", "Leticia Bredice","Pochi Ducasse"]
odisea = Filmacion "La odisea de los giles" 8 2019 116 ["Ricardo Darín", "Luis Brandoni", "Verónica Llinás", "Daniel Aráoz", "Rita Cortese"]
flor = Filmacion "La Flor" 7 2018 840 ["Pilar Gamboa"]
speed = Filmacion "Speed" 7 1994 116 ["Keanu Reeves", "Sandra Bullock", "Dennis Hopper", "Jeff Daniels", "Alan Ruck"]
indianaIV = Filmacion "Indiana Jones IV" 6 2007 125 ["Harrison Ford"]
indianaI =  Filmacion "Indiana Jones I" 8 1981 115 ["Harrison Ford"]

{- Ejer1 -}
esDarinesca :: Filmacion -> Bool
esDarinesca film =  head (actores film) == "Ricardo Darín"

pintaBuena :: Filmacion -> Bool
pintaBuena filme = (>=5).length.actores $ filme

minutosExcedentes:: Filmacion -> Int
minutosExcedentes film =  abs (duracion film - 115)

{- Ejer2 -}
pintaGrosa :: Filmacion -> Bool
pintaGrosa filme = (>4).length.actores $ filme

esVieja :: Filmacion -> Bool
esVieja filme = (<1990).lanzamiento $ filme

titleLenPorDos :: Filmacion -> Int
titleLenPorDos filme = (*2).length.titulo $ filme

puntajePorTres :: Filmacion -> Int
puntajePorTres filme = (*3).puntaje $ filme

precioBase :: Filmacion -> Int
precioBase filme | pintaGrosa filme = 200 | esVieja filme = titleLenPorDos filme | otherwise = 100 + puntajePorTres filme

esLarga :: Filmacion -> Bool
esLarga filme = (>115).duracion $ filme

extraPorExcedente :: Filmacion -> Int
extraPorExcedente filme = ((*10).minutosExcedentes $ filme) `min` 100

precioExtra :: Filmacion -> Int
precioExtra filme | esLarga filme = extraPorExcedente filme | not.esVieja $ filme = 50 | otherwise = 0

precioTotal :: Filmacion -> Int
precioTotal film | precioBase film + precioExtra film < 200 = precioBase film + precioExtra film
                 | otherwise = round (fromIntegral (precioBase film + precioExtra film) * 0.9)

{- Ejer3 -}
type Nombre = String;
data Persona = Persona {
  nombre :: Nombre,
  satisfaccion :: Int,
  edad :: Int,
  cantidadVistas :: Int,
  credito :: Int
} deriving Show

actualizarCredito :: Int -> Int -> Int
actualizarCredito credito precio | precio > credito = 0
                                 | otherwise = credito - precio

aplicarPelicula :: Persona -> Filmacion -> Persona
aplicarPelicula p f = p {cantidadVistas = 1 + cantidadVistas p, credito = actualizarCredito (credito p) (precioTotal f)}


type LtsSangre = Int;
terror :: Persona -> LtsSangre -> Persona
terror p lts = p { satisfaccion = (satisfaccion p) - lts}


aplicarTerror :: Persona -> Filmacion -> LtsSangre -> Persona
aplicarTerror p f lts = aplicarPelicula (terror p lts) f


comedia :: Persona -> Persona
comedia p = p { nombre = nombre p ++ " muy alegre", satisfaccion = 2 * satisfaccion p }

aplicarComedia :: Persona -> Filmacion -> Persona
aplicarComedia p f = (aplicarPelicula.comedia) p f

type EscenasFelices = Int
drama :: Persona -> EscenasFelices -> Persona
drama p escenas = p { edad = edad p + 1, satisfaccion = (min 3 escenas) + satisfaccion p}

aplicarDrama :: Persona -> EscenasFelices -> Filmacion -> Persona
aplicarDrama p escenas f = aplicarPelicula (drama p escenas) f

accion :: Persona -> Filmacion -> Persona
accion p film | pintaBuena(film) = p { satisfaccion = 100 + satisfaccion p}
              | otherwise = p

aplicarAccion :: Persona -> Filmacion -> Persona
aplicarAccion p f = aplicarPelicula (accion p f) f

tragicomico :: Persona -> Persona
tragicomico p = (drama.comedia) p 4

aplicarTragicomico :: Persona -> Filmacion -> Persona
aplicarTragicomico p f = aplicarPelicula (aplicarPelicula (tragicomico p) f) f

type VersionMala = String

terminaConVersion :: VersionMala -> String -> Bool
terminaConVersion v titulo = v == drop (length titulo - length v) titulo

aventura :: Persona -> Filmacion -> VersionMala -> Persona
aventura p f v | terminaConVersion v (titulo f) = p
               | otherwise = comedia p

aplicarAventura :: Persona -> Filmacion -> VersionMala -> Persona
aplicarAventura p f v = aplicarPelicula (aventura p f v) f

pepe = Persona "Pepe" 20 30 3 1500