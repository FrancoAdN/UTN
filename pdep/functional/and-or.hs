-- 2) Definir las funciones boolenas estándar. Sin usar las funciones predefinidas.
-- 2.1) Definir la función and’
-- 2.2) Definir la función or’.


and' :: Bool -> Bool -> Bool
and' exp1 exp2 | exp1 = exp2
               | otherwise = False

and'' :: Bool -> Bool -> Bool
and'' True exp2 = exp2
and'' _ _ = False

or' :: Bool -> Bool -> Bool
or' exp1 exp2 | exp1 = True
              | otherwise = exp2

or'' :: Bool -> Bool -> Bool
or'' False exp2 = exp2
or'' _ _ = True
