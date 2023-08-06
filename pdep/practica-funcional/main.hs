import Text.Show.Functions
-- ^
type Elemento = Float -> Float
data Personaje = Personaje {nombre::String, experiencia:: Float, fuerzaBasica:: Float, elemento :: Elemento} deriving Show

espadaOxidada = (1.2*)
katanaFilosa = (10+).(0.9*)
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x = x* 2
espadaMaldita = espadaOxidada.sableLambdico 89

nivel :: Personaje -> Int
nivel (Personaje _ experiencia _ _) = ceiling(experiencia^2 /(experiencia + 1))

capacidad :: Personaje -> Float
capacidad (Personaje _ _ fuerzaBasica elemento) = elemento fuerzaBasica

cecilia = Personaje "cecilia" 40 50 baculoDuplicador
pedro = Personaje "pedro" 50 30 (sableLambdico 30)

type Alquimista = Personaje -> Personaje

alterarElemento :: Elemento -> Alquimista
alterarElemento f personaje = personaje { elemento = f.elemento personaje}
-- Los aprendices, que fusionan el elemento del cazador con un químico secreto que duplica su efecto.
aprendiz :: Alquimista
aprendiz personaje = alterarElemento (2*) personaje

-- Los maestros alquimistas, que hacen lo mismo que los aprendices, 
-- pero además aplican al elemento un 10% extra de capacidad,
-- tantas veces como años de oficio tenga en su profesión

extraPorAntiguedad 0 = id
extraPorAntiguedad años = (*1.1) .extraPorAntiguedad (años -1)

maestro :: Int -> Alquimista
maestro años personaje = alterarElemento (extraPorAntiguedad años).aprendiz $ personaje

-- Los estafadores, que reemplazan el elemento del personaje por un elemento que no hace nada.
estafador :: Alquimista
estafador personaje = personaje { elemento = id }

-- Inventar un nuevo tipo de alquimista que, similar a los anteriores, 
-- haga algo con el elemento del personaje.
-- Debe utilizarse alguna expresión lambda, de una forma que no sea trivial.
inventado :: Alquimista
inventado personaje = personaje { elemento = (\n -> (n*10) + 100)}

-- Todos aquellos que hacen que la capacidad del cazador sea superior a un valor dado.
capacidadesSuperioresA :: Float -> Personaje -> [Alquimista] -> [Alquimista]
capacidadesSuperioresA valor personaje alquimistas = filter (tieneCapacidadSuperiorA valor personaje)  alquimistas

tieneCapacidadSuperiorA :: Float -> Personaje -> Alquimista -> Bool
tieneCapacidadSuperiorA valor personaje alquimista =  (>valor).capacidad.alquimista $ personaje

-- Si todos los alquimistas le convienen al personaje
convieneTodos :: Personaje ->[Alquimista] -> Bool
convieneTodos personaje alquimistas = all (tieneCapacidadSuperiorA (capacidad personaje) personaje) alquimistas

type Habilidad = (String, String)
data Monstruo = Monstruo { especie::String, resistencia::Float, habilidades:: [Habilidad]} deriving Show


-- Nos interesa saber si un monstruo es agresivo.
-- Eso sucede cuando tiene más habilidades ofensivas (tipo mágicas o físicas) que de soporte (las restantes)
-- y además su resistencia es mayor a 0. Los monstruos de las especies animal y chocobo nunca son agresivos.

esAgresivo :: Monstruo -> Bool
esAgresivo  monstruo = (tieneMayoriaHabilidadesOfensivas. habilidades) monstruo && ((>0).resistencia) monstruo && (not.especieInofensiva.especie) monstruo

especieInofensiva :: String -> Bool
especieInofensiva especie = elem especie [ "animal", "chocobo"]

esOfensiva:: String -> Bool
esOfensiva "magia" = True
esOfensiva "fisica" = True
esOfensiva _ = False

descripcion = fst
tipo = snd
tieneMayoriaHabilidadesOfensivas :: [Habilidad] -> Bool
tieneMayoriaHabilidadesOfensivas habilidades = (length.filter(esOfensiva.tipo)) habilidades > div (length habilidades) 2

-- Averiguar si un personaje le gana a un monstruo.
-- Le gana si su capacidad de caza es mayor que la resistencia del monstruo.

leGana :: Personaje -> Monstruo -> Bool
leGana personaje mounstruo = capacidad personaje > resistencia mounstruo

-- Se quiere conocer el estado del personaje luego de explorar un mapa 
-- y pelear con todos los monstruos agresivos que haya.
-- Cada vez que le gana a un monstruo su experiencia aumenta en 100 puntos,
-- si pierde disminuye en 50 y su objeto pierde un 10% de su poder, 
-- pero en todos los casos va por el próximo enemigo del mapa.

pelearConTodos :: Personaje -> [Monstruo] -> Personaje
pelearConTodos personaje monstruos = foldl pelear personaje monstruos

pelear :: Personaje -> Monstruo -> Personaje
pelear personaje monstruo | leGana personaje monstruo = modificarExperiencia 100 personaje
                          | otherwise = alterarElemento (*0.9).modificarExperiencia (-50) $ personaje


modificarExperiencia :: Float -> Personaje -> Personaje
modificarExperiencia valor personaje = personaje { experiencia = experiencia personaje + valor}

-- Ver si hay algún monstruo en el mapa al que, pese a haber recurrido a un alquimista amigo, el cazador no pueda vencer.
pierdeConAlguno :: Personaje -> Alquimista -> [Monstruo] -> Bool
pierdeConAlguno personaje alquimista monstruos = any (not.leGana (alquimista personaje)) monstruos