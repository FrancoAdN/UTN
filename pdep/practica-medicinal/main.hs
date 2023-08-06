import Text.Show.Functions

type Enfermedad = Paciente -> Paciente

data Paciente = Paciente {
    nombre::String,
    edad::Float,
    vitalidad::Float,
    temperatura::Float,
    enfermedades:: [Enfermedad]
} deriving Show

type Letra = String
gripe:: Letra -> Enfermedad
gripe "A" paciente = paciente { vitalidad = 0.5 * vitalidad paciente }
gripe "B" paciente = paciente { temperatura = 10 + temperatura paciente }
gripe "C" paciente | (edad paciente) < 10 = paciente { vitalidad = 0.8 * vitalidad paciente}
gripe _ paciente = paciente

type Nivel = Float
estres:: Nivel -> Enfermedad
estres nivel paciente = paciente { vitalidad = max  (vitalidad paciente - (nivel * 20)) 0 }

neumonia:: Enfermedad
neumonia paciente = paciente { temperatura = min (1.3 * temperatura paciente) 40 }

sonia = Paciente "Sonia" 20 200 38 [(estres 10), neumonia]

nehu = Paciente "NEHU" 20 200 30 [(estres 1)]
franco = Paciente "Franco" 23 400 30 enfs

enfs = [neumonia, (estres 20), (gripe "A")]


aplicarEnfermedad:: Paciente -> Enfermedad -> Paciente
aplicarEnfermedad paciente enfermedad = enfermedad paciente { enfermedades = enfermedades paciente ++ [enfermedad] }

aplicarEnfermedades :: Paciente -> [Enfermedad] -> Paciente
aplicarEnfermedades paciente enfermedades = foldl (aplicarEnfermedad) paciente enfermedades

seContagiaDelAmbiente:: Paciente -> [Enfermedad] -> Paciente
seContagiaDelAmbiente paciente enfermedades = aplicarEnfermedades paciente enfermedades

seContagioDe :: Paciente -> Paciente -> Paciente
seContagioDe paciente1 paciente2 = aplicarEnfermedades paciente1 (enfermedades paciente2)

seContagioMal:: Paciente -> [Paciente] -> Paciente
seContagioMal paciente pacientes = foldl (seContagioDe) paciente pacientes

type Sustancia = Paciente -> Paciente

paracetamol :: Sustancia
paracetamol paciente = paciente { temperatura = min (temperatura paciente) 37 }

type Gramos = Float
ginsen :: Gramos -> Sustancia
ginsen grs paciente = paciente { vitalidad = (0.1 * grs) + vitalidad paciente}

magnesio :: Sustancia
magnesio paciente = paciente { vitalidad = 1.3 * vitalidad paciente }

data Receta = Receta {
    nombr :: String,
    sustancias:: [Sustancia]
} deriving Show

energizante = Receta "Energizante" [(ginsen 50), magnesio]

aplicarReceta :: Receta -> Paciente -> Paciente
aplicarReceta receta paciente = (foldl1 (.) (sustancias receta)) paciente

energiaMayorAX:: Float -> [Paciente] -> Receta -> [Paciente]
energiaMayorAX x pacientes receta = filter (\p -> vitalidad p > x) (map ((gripe "A").(aplicarReceta receta)) pacientes)

todosCurados :: [Paciente] -> Receta -> Bool
todosCurados pacientes receta = all (\p -> vitalidad p > 70 && temperatura p < 37) (map (aplicarReceta receta) pacientes)

-- ordenado:: Float -> [Float] -> Bool
-- ordenado _ [] = True
-- ordenado ant (x:xs) | ant >= x = ordenado x xs
--                     | otherwise = False
ordenado:: [Float] -> Bool
ordenado [x] = True
ordenado (x1:x2:nums) | x1 > x2 = ordenado (x2:nums)
                      | otherwise = False



ordenadoSegunCriterio:: (Paciente -> Float)-> [Paciente] -> Bool
ordenadoSegunCriterio criterio pacientes = ordenado (map criterio pacientes)

maxTemperaturaEdad:: Paciente -> Float
maxTemperaturaEdad paciente = max (temperatura paciente) (edad paciente)

ordenadoSegunVitalidad:: [Paciente] -> Receta -> Bool
ordenadoSegunVitalidad pacientes receta = ordenadoSegunCriterio (maxTemperaturaEdad.(gripe "A").(aplicarReceta receta)) pacientes

restoEntreEnfermedadesyVit:: Paciente -> Float
restoEntreEnfermedadesyVit paciente = fromIntegral (length (enfermedades paciente) `mod` floor (vitalidad paciente))

ordenadoSegunResto:: [Paciente] -> Bool
ordenadoSegunResto pacientes = ordenadoSegunCriterio restoEntreEnfermedadesyVit pacientes

-- ordenadoSegunResto [franco, sonia, nehu]
-- ordenadoSegunVitalidad [sonia, franco, nehu] energizante



