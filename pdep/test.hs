import Data.List
import Data.Function

lista = [87, 5, 78, 4, 2,1 ,56,33,54]
lista2 = [(+1), (+3), (*2)]

fn = foldl1 (.) lista2

