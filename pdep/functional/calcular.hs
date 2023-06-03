-- Definir la función calcular’, que recibe una tupla de 2 elementos, 
-- y devuelve una nueva tupla según las siguientes reglas:
-- ●  si el primer elemento es par lo duplica; si no lo deja como está
-- ●  si el segundo elemento es impar le suma 1; si no deja como está


double :: Integer -> Integer
double num = num * 2

addOne :: Integer -> Integer
addOne num = num + 1

auxOne num | even num = double(num)
           | otherwise = num

auxTwo num | odd num = addOne(num)
           | otherwise = num

calcular'::(Integer, Integer) -> (Integer, Integer)
calcular' (first, second) = (auxOne(first), auxTwo(second))
