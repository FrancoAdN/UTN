import Text.Show.Functions
import Data.List

{- Punto 1 -}
data Chofer = Chofer {
    nombre::String,
    km:: Float,
    viajes::[Viaje],
    condicion::Condicion
} deriving Show

data Cliente = Cliente {
    nombr::String,
    barrio::String
} deriving Show

data Viaje = Viaje {
    cliente:: Cliente,
    fecha::String,
    costo::Float
} deriving Show

{- Punto 2 -}
type Condicion = Viaje -> Bool

cualquiera::Condicion
cualquiera _ = True

masDeDoscientos::Condicion
masDeDoscientos viaje = (>200).costo $ viaje

nombreMayorQueN::Int -> Condicion
nombreMayorQueN n viaje = (>n).length.nombr.cliente $ viaje

noViveEnZona:: String -> Condicion
noViveEnZona zona viaje = (/=zona).barrio.cliente $ viaje

{- Punto 3 -}

lucas = Cliente "Lucas" "Victoria"
daniel = Chofer "Daniel" 23.500 [Viaje lucas "20/04/2017" 150] (noViveEnZona "Olivos")
alejandra = Chofer "Alejandra" 180.000 [] cualquiera

{- Punto 4 -}

cumpleCondicion:: Viaje -> Chofer -> Bool
cumpleCondicion viaje chofer = (condicion chofer) viaje

{- Punto 5 -}
liquidacion::Chofer -> Float
liquidacion chofer =  sum.(map costo.viajes) $ chofer

{- Punto 6 -}
puedenTomarElViaje:: Viaje -> [Chofer] -> [Chofer]
puedenTomarElViaje viaje choferes = filter (cumpleCondicion viaje) choferes


choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)
-- otra opcion es hacerlo con fold

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2
   | cuantosViajes chofer1 > cuantosViajes chofer2 = chofer2
   | otherwise                                     = chofer1

cuantosViajes = length . viajes

hacerViaje::Viaje -> Chofer -> Chofer
hacerViaje viaje chofer = chofer {
    viajes = viaje : viajes chofer
}

efecturaElViaje:: Viaje -> [Chofer] -> Chofer
efecturaElViaje viaje choferes = (hacerViaje viaje).choferConMenosViajes.(puedenTomarElViaje viaje) $ choferes

