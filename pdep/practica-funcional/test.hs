import Text.Show.Functions

-- 1)
data Pelicula = Pelicula {titulo::String, genero::String, duracion::Integer, origen::String} deriving Show
data Usuario = Usuario {
    nombre::String, 
    categoria::String,
    edad::Integer,
    residencia::String,
    vistas :: [Pelicula],
    salud::Integer} deriving Show

psicosis = Pelicula "Psicosis" "Terror" 109 "Estados Unidos"
perfumeDeMujer= Pelicula "Perfume de Mujer" "Drama" 150  "Estados Unidos"
elSaborDeLasCervezas = Pelicula "El sabor de las cervezas"  "Drama" 60 "Iran"
lasTortugasTambienVuelan = Pelicula "Las tortugas también vuelan" "Drama" 103 "Iran"
pelisDeLaEmpresa = [psicosis, perfumeDeMujer, elSaborDeLasCervezas, lasTortugasTambienVuelan]

juan = Usuario "juan" "estandar" 23  "Argentina" [psicosis] 60
franco = Usuario "franco" "basica" 23  "Argentina" iranies 60


iranies = [elSaborDeLasCervezas, lasTortugasTambienVuelan,
            elSaborDeLasCervezas, lasTortugasTambienVuelan,
            elSaborDeLasCervezas, lasTortugasTambienVuelan,
            elSaborDeLasCervezas, lasTortugasTambienVuelan,
            elSaborDeLasCervezas, lasTortugasTambienVuelan] ++ [elSaborDeLasCervezas, lasTortugasTambienVuelan,elSaborDeLasCervezas, lasTortugasTambienVuelan,elSaborDeLasCervezas, lasTortugasTambienVuelan,elSaborDeLasCervezas, lasTortugasTambienVuelan,elSaborDeLasCervezas, lasTortugasTambienVuelan]
-- 2)
verPelicula :: Usuario -> Pelicula -> Usuario
verPelicula user peli = user { vistas = vistas user ++ [peli]}

-- 3)

esPeliculaUS :: Pelicula -> Bool
-- esPeliculaUS (Pelicula _ _ _ origen) = origen == "Estados Unidos"
esPeliculaUS peli = origen peli == "Estados Unidos"

cumpleParaPremio :: [Pelicula] -> Bool
cumpleParaPremio pelis = (>=20).length.filter (\p -> not.esPeliculaUS $ p) $ pelis 

siguienteCategoria:: String -> String
siguienteCategoria "basica" = "estandar"
siguienteCategoria "estandar" = "premium"
siguienteCategoria "premium" = "premium"

subirCategoriaSiCumple:: Usuario -> Usuario
subirCategoriaSiCumple user | cumpleParaPremio (vistas user) = user {categoria= siguienteCategoria (categoria user)}
                            | otherwise = user

internacionalesFieles:: [Usuario] -> [Usuario]
internacionalesFieles users = map subirCategoriaSiCumple users

-- 4)
teQuedasteCorto :: Pelicula -> Bool
teQuedasteCorto peli =  35 > duracion peli

cuestionDeGenero :: String -> Pelicula -> Bool
cuestionDeGenero indicado peli = indicado == genero peli

deDondeSaliste :: String -> Pelicula -> Bool
deDondeSaliste particular peli = particular == origen peli

vaPorEseLado :: String -> Pelicula -> Bool
vaPorEseLado indicado peli = (deDondeSaliste "Argentina") peli && (cuestionDeGenero indicado) peli

buscarPorCriterio :: (Pelicula -> Bool) -> [Pelicula] -> [Pelicula]
buscarPorCriterio criterio pelis = filter criterio pelis

-- 5)

usuarioVioPeli:: Usuario -> Pelicula -> Bool
usuarioVioPeli user peli = not.(==0).length.filter (\p -> titulo p == titulo peli) $ vistas user

noVistasQueCumplenPorUsuario::[(Pelicula -> Bool)] -> [Pelicula] -> Usuario -> [Pelicula]
noVistasQueCumplenPorUsuario criterios pelis user = filter (\p -> (cumpleCriterios criterios p) && (not.(usuarioVioPeli user) $ p)) pelis

cumpleCriterios:: [(Pelicula -> Bool)] -> Pelicula  -> Bool
cumpleCriterios criterios peli = all (\c -> c peli) criterios

proponerTresPorCriterios:: Usuario -> [(Pelicula -> Bool)] -> [Pelicula]
proponerTresPorCriterios user criterios = (take 3.noVistasQueCumplenPorUsuario criterios pelisDeLaEmpresa) user
