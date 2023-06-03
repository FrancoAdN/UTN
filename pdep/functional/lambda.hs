-- 6) Dar expresiones lambda que sean equivalentes a las siguientes expresiones:
-- triple
-- siguiente
-- suma
-- sumarDos

triple = (\x -> 3 * x)
next = (\x -> x + 1)
add = (\x y -> x + y)
addTwo = next . next