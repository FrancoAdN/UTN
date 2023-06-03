-- 3) Definir la función notaMaxima que dado un alumno devuelva la máxima nota del alumno. (Nota resolverlo sin guardas).

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)


maxGrade :: Alumno -> Nota
maxGrade (_, grade1, grade2, grade3) = max grade1 (max grade2 grade3)
