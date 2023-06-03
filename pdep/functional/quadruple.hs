-- Definir la función cuadruple reutilizando la función doble. 
double :: Integer -> Integer
double num = num * 2

quadruple:: Integer -> Integer
quadruple = double . double 
