import Text.Show.Functions

type Gema = Personaje -> Personaje

data Personaje = Personaje { nombre:: String, edad::Int, planeta:: String, energia:: Float, habilidades:: [String]} deriving Show
data Guantelete = Guantelete { material::String, gemas:: [Gema] } deriving Show
data Universo = Universo { habitantes::[Personaje]} deriving Show


cantHabitantesRestantes:: Universo -> Int
cantHabitantesRestantes universo = div (length (habitantes universo)) 2

cumpleParaChasquido:: Guantelete -> Bool
cumpleParaChasquido guan = (material guan) == "uru" && length (gemas guan) == 6

chasquido:: Guantelete -> Universo -> Universo
chasquido guantelete uni | cumpleParaChasquido guantelete = uni { habitantes = take (cantHabitantesRestantes uni) (habitantes uni) }
                         | otherwise = uni



aptoParaPendex:: Universo -> Bool
aptoParaPendex uni = any ((45>).edad) (habitantes uni)


energiaTotal:: Universo -> Float
energiaTotal uni = sum.(map energia).(filter ((1<).length.habilidades)) $ (habitantes uni)

quitarEnergia:: Float -> Gema
quitarEnergia valor pj = pj { energia = energia pj - valor}

mente:: Float -> Gema
mente val pj = quitarEnergia val pj


alma :: String -> Gema
alma habilidad personaje = quitarEnergia 10 personaje {
    habilidades = filter (/=habilidad) $ habilidades personaje 
}

espacio:: String -> Gema
espacio planeta pj = quitarEnergia 20 pj { planeta = planeta }

quitarHabilidadesSiCumple:: Personaje -> Personaje
quitarHabilidadesSiCumple pj | (<=2).length.habilidades $ pj = pj { habilidades = []}
                             | otherwise = pj
poder::Gema
poder pj = quitarEnergia (energia pj) (quitarHabilidadesSiCumple pj)

tiempo::Gema
tiempo pj = quitarEnergia 50 pj { edad = max ((edad pj) `div` 2) 18}

gemaLoca:: Gema -> Gema
gemaLoca gema pj = gema.gema $ pj

guanteGoma = Guantelete "goma" [tiempo, (alma "usar Mjolnir"), (gemaLoca (alma "programacion en Haskell"))]


utilizar:: [Gema] -> Personaje -> Personaje
utilizar gemas pj = (foldl1 (.) gemas) pj
-- utilizar gemas pj = foldr ($) pj $ gemas

obtenerMasPoderosa:: [Gema] -> Gema -> Personaje -> Gema
obtenerMasPoderosa [] gema _ = gema
obtenerMasPoderosa (actual: siguientes) ant pj | (energia.actual $ pj) < (energia.ant $ pj) = obtenerMasPoderosa siguientes actual pj
                                               | otherwise = obtenerMasPoderosa siguientes ant pj

gemaMasPoderosa::Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete pj = obtenerMasPoderosa (gemas guantelete) (\p -> p) pj


-- gemaMasPoderosa :: Personaje -> Guantelete -> Gema
-- gemaMasPoderosa personaje guantelte = gemaMasPoderosaDe personaje $ gemas guantelte

-- gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
-- gemaMasPoderosaDe _ [gema] = gema
-- gemaMasPoderosaDe personaje (gema1:gema2:gemas) 
--     | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:gemas)
--     | otherwise = gemaMasPoderosaDe personaje (gema2:gemas)



infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete


{- gemaMasPoderosa punisher guanteleteDeLocos
    No se podria ejecutar debido a que son infinitas gemas y
    la recursividad no terminaria nunca
-}

{- usoLasTresPrimerasGemas guanteleteDeLocos punisher
    Funcionaria sin problemas debido a que tomaria solamente las
    primeras 3 gemas de la lista infinita definida y las aplicaria al personaje
-}



spiderMan :: Personaje
spiderMan = Personaje{
    edad = 21,
    energia = 2300,
    habilidades = ["sentido arácnido", "sacar fotos", "trepar paredes"],
    nombre = "Peter Parker",
    planeta = "tierra"
}    

ironMan :: Personaje
ironMan = Personaje{
    edad = 48,
    energia = 1900,
    habilidades = ["volar", "programacion en Haskell", "programacion en Prolog"],
    nombre = "Tony Stark",
    planeta = "tierra"
}     

thor :: Personaje
thor = Personaje{
    edad = 1500,
    energia = 3000,
    habilidades = ["usar Mjolnir"],
    nombre = "Thor",
    planeta = "asgard"
}

drStrange :: Personaje
drStrange = Personaje{
    edad = 50,
    energia = 1000,
    habilidades = ["levitar", "ver futuros"],
    nombre = "Stephen Strange",
    planeta = "tierra"
}

guanteleteDeGoma = Guantelete {
  material = "100% Acrilo Nitrilo",
  gemas = [mente 10, alma "usar Mjolnir",espacio "Jupiter", poder, tiempo, gemaLoca poder]
}