# Escuelita de Thanos 


## Primera parte

Los enanos de Nidavellir nos han pedido modelar los guanteletes que ellos producen en su herrería. Un guantelete está hecho de un material (“hierro”, “uru”, etc.) y sabemos las gemas que posee. También se sabe de los personajes que tienen una edad, una energía, una serie de habilidades (como por ejemplo “usar espada”, “controlar la mente”, etc), su nombre y en qué planeta viven. Los fabricantes determinaron que cuando un guantelete está completo -es decir, tiene las 6 gemas posibles- y su material es “uru”, se tiene la posibilidad de chasquear un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos personajes. Por ejemplo si tenemos un universo en el cual existen ironMan, drStrange, groot y wolverine, solo quedan los dos primeros que son ironMan y drStrange. Si además de los 4 personajes estuviera viudaNegra, quedarían también ironMan y drStrange porque se considera la división entera.

**Punto 1: (2 puntos)** Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.


**Punto 2: (3 puntos) *Resolver utilizando únicamente orden superior*.**
- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.


## Segunda parte
A su vez, aunque el guantelete no se encuentre completo con las 6 gemas, el poseedor puede utilizar el poder del mismo contra un enemigo, es decir que puede aplicar el poder de cada gema sobre el enemigo. Las gemas del infinito fueron originalmente parte de la entidad primordial llamada Némesis, un ser todopoderoso del universo anterior quién prefirió terminar su existencia en lugar de vivir como la única conciencia en el universo. Al morir, dio paso al universo actual, y el núcleo de su ser reencarnó en las seis gemas: 
- La **mente** que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
- El **alma** puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía. 
- El **espacio** que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.
- El **poder** deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).
- El **tiempo** que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del oponente con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.
- La **gema loca** que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

**Punto 3: (3 puntos)** Implementar las gemas del infinito, evitando lógica duplicada. 

**Punto 4: (1 punto)** Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.


**Punto 5: (2 puntos). *No se puede utilizar recursividad*.** Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.


**Punto 6: (2 puntos). *Resolver utilizando recursividad*.** Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 


**Punto 7: (1 punto)** Dada la función generadora de gemas y un guantelete de locos:
```
    infinitasGemas :: Gema -> [Gema]
    infinitasGemas gema = gema:(infinitasGemas gema)

    guanteleteDeLocos :: Guantelete
    guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)
``` 

Y la función 
```
    usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
    usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete
```

Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
```
    gemaMasPoderosa punisher guanteleteDeLocos
    usoLasTresPrimerasGemas guanteleteDeLocos punisher
```
