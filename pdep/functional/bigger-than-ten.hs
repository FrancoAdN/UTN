-- Definir la función esMayorA, que verifique si el doble del siguiente de la suma entre 2 y un número es mayor a 10. 

double :: Integer -> Integer
double num = num * 2

next :: Integer -> Integer
next num = num + 1

doubleOfNext = double . next

biggerThanTen :: Integer -> Bool
biggerThanTen num = doubleOfNext( 2 + num) > 10