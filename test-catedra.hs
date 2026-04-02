import Test.HUnit
import Data.List
import Solucion
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests
  
allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo
testAuxVueloValido = test[
    "Caso 1: Origen y destino iguales, duracion mayor que 0"~: vueloValido ("BsAs", "BsAs", 1.0) ~?= False,
    "Caso 2: Origen y destino iguales, duracion no mayor que 0"~: vueloValido ("BsAs", "BsAs", -1.0) ~?= False,
    "Caso 3: Origen y destino Distintos, duracion mayor que 0"~: vueloValido ("BsAs", "Bogota", 1.0) ~?= True,
    "Caso 4: Origen y destino Distintos, duracion no mayor que 0"~: vueloValido ("Bogota", "BsAs", 0) ~?= False
    ]

testAuxVueloRepetido = test[
    "Caso 1: Vuelos con elementos, vuelo pertenece"~: vueloRepetido ("BsAs", "Bogota", 1.0) [("BsAs", "Córdoba", 3.0), ("BsAs","Saltá", 1.5),("BsAs", "Bogota", 1.0)] ~?= True,
    "Caso 2: Vuelos con elementos, vuelo no pertenece"~: vueloRepetido ("BsAs", "Bogota", 1.0) [("BsAs", "Córdoba", 3.0), ("BsAs","Saltá", 1.5)] ~?= False,
    "Caso 3: Vuelos sin elementos"~: vueloRepetido ("BsAs", "Bogota", 1.0) [] ~?= False
    ]

testsEjvuelosValidos = test [
    "vuelosValidos Caso 0: sin elementos" ~: 
    vuelosValidos [] ~?= True,
    "vuelosValidos Caso 1: Alguno con origen y destino iguales, Todos con duracion mayor que 0, alguno repetido" ~: 
    vuelosValidos [("BsAs", "Rosario", 5.0),("BsAs", "Rosario", 5.0),("Córdoba", "Córdoba", 5.0)] ~?= False,
    "vuelosValidos Caso 2: Alguno con origen y destino iguales, alguno sin duracion mayor que 0, alguno repetido" ~: 
    vuelosValidos [("Bogotá", "Bogotá", 2.0),("BsAs", "Saltá", 0),("Bogotá", "Bogotá", 2.0)] ~?= False,
    "vuelosValidos Caso 3: Alguno con origen y destino iguales, alguno con duracion menor que 0, ninguno repetido"~: 
    vuelosValidos [("BsAs", "Rosario", -2.5),("BsAs", "BsAs", 2.5)] ~?= False,
    "vuelosValidos Caso 4: Alguno con origen y destino iguales, alguno con duracion mayor que 0, alguno repetido" ~:
    vuelosValidos [("Medellín", "Medellín", 0),("Medellín", "Medellín", 5.0),("Barranquilla", "Bogotá", 5.0)]  ~?= False,
    "vuelosValidos Caso 5: Ninguno con origen y destino iguales, Todos con duracion mayor que 0, ninguno repetido" ~: 
    vuelosValidos [("Bogotá", "Medellín", 2.0), ("Bogotá", "Tolima", 2.0)] ~?= True,
    "vuelosValidos Caso 6: Ninguno con origen y destino iguales, ninguno con duracion mayor que 0, alguno repetido" ~: 
    vuelosValidos [("Bogotá", "Medellín", -2.0), ("Bogotá", "Medellín", -2.0)] ~?= False,
    "vuelosValidos Caso 7: Ninguno con origen y destino iguales, ninguno con duracion mayor que 0, ninguno repetido" ~: 
    vuelosValidos [("Bogotá", "Medellín", 0), ("Medellín", "Cali", -3.0), ("Cali", "Cartagena", 0)] ~?= False
    ]

testsEjciudadesConectadas = test [
    "ciudadesConectadas Caso 0: agencia vacia " ~: 
    ciudadesConectadas [] "A" ~?= [],
    "ciudadesConectadas Caso 1: no hay vuelos que salen ni vuelos que llegen a ciudad" ~: 
    ciudadesConectadas [("A", "B", 2.0), ("B", "C", 3.0)] "D" ~?= [],
    "ciudadesConectadas Caso 2: agencia con vuelos que salen de ciudad" ~: 
    esPermutacion (ciudadesConectadas [("F", "Z", 5.0),("F", "T", 4.0),("F", "U", 5.0),("F", "G", 5.0)] "F") ["Z","T","U","G"] ~?= True ,
    "ciudadesConectadas Caso 3: agencia con vuelos que llegan a ciudad" ~: 
    esPermutacion (ciudadesConectadas [("M", "N", 2.0), ("F", "N", 3.0), ("B", "N", 1.0),("T", "F", 2.0),("T", "N", 4.0)] "N") ["M","F","B","T"] ~?= True,
    "ciudadesConectadas Caso 5: agencia con vuelos que llegan y salen de ciudad" ~: 
    esPermutacion (ciudadesConectadas [("G", "A", 2.0),("B", "G", 2.0),("Y", "G", 2.0),("G", "K", 2.0),("P", "G", 2.0)] "G") ["A","B","Y","K","P"] ~?= True,
    "ciudadesConectadas Caso 6: agencia con vuelos que llegan y salen de ciudad con ciudades repetidas(que tienen viajes de ida y vuelta con ciudad)" ~: 
    esPermutacion (ciudadesConectadas [("X", "Y", 3.0), ("Y", "X", 3.0), ("X", "Z", 4.0), ("Z", "X", 4.0), ("Y", "Z", 5.0)] "X") ["Y", "Z"]  ~?= True
    ]

testsEjmodernizarFlota = test [
    "modernizarFlota Caso 0: agencia vacia" ~: 
    modernizarFlota [] ~?= [],
    "modernizarFlota Caso 1: con un elemento" ~: 
    expectlistProximity(modernizarFlota [("BsAs", "Rosario", 10.0)]) [("BsAs", "Rosario", 9)],
    "modernizarFlota Caso 2: con mas de un elemento" ~: 
    expectlistProximity(modernizarFlota [("BsAs", "Rosario", 5.0),("cordoba", "san juan", 4.64)]) [("BsAs","Rosario",4.5),("cordoba","san juan",4.1759996)],
    "modernizarFlota Caso 3: con mas de un elemento--mas extensa" ~: 
    expectlistProximity(modernizarFlota [("BsAs", "Maimi", 9.0),("Maimi", "Bogota", 6.0),("Maimi", "Bogota", 5.0),("Bogota", "BsAs", 8.5)]) 
    [("BsAs", "Maimi", 8.099999),("Maimi", "Bogota", 5.3999996),("Maimi", "Bogota", 4.5),("Bogota", "BsAs", 7.6499996)]
    ]

testsEjciudadMasConectada = test [
    "ciudadMasConectada Caso 1: una sola ciudad" ~: 
    expectAny (ciudadMasConectada [("A", "B", 10.0)]) ["A","B"],
    "ciudadMasConectada Caso 2: 2 ciudades sin empates" ~: 
    ciudadMasConectada [("D", "A", 10.0),("F", "D", 7.0)] ~?= "D",
    "ciudadMasConectada Caso 3: con varias ciudades sin empates" ~: 
    ciudadMasConectada [("A", "D", 10.0),("D", "F", 7.0),("D", "T", 7.0),("T", "D", 7.0)] ~?= "D",
    "ciudadMasConectada Caso 5: 2 ciudades empatadas" ~: 
    expectAny (ciudadMasConectada [("X", "Y", 2.0), ("Y", "Z", 3.0), ("Z", "W", 4.0)]) ["Y","Z"],
    "ciudadMasConectada Caso 6: 3 ciudades empatadas" ~: 
    expectAny (ciudadMasConectada [("M", "N", 2.0), ("P", "O", 4.0), ("M", "P", 5.0), ("P", "N", 4.0),("M", "Q", 6.0),("Q", "N", 2.0)]) ["M","P","O"]
    ]

testsEjsePuedeLlegar = test [
    "Se puede llegar caso verdadero con una escala" ~: 
    sePuedeLlegar [("BsAs", "Rosario", 5.0),("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True,
    "Se puede llegar caso falso sin vuelos" ~: 
    sePuedeLlegar [] "BsAs" "Rosario" ~?= False,
    "Se puede llegar caso falso sin vuelo de llegada" ~: 
    sePuedeLlegar [("BsAs", "Rosario", 5.0),("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Miami" ~?= False,
    "Se puede llegar caso falso con dos escalas" ~:
    sePuedeLlegar [("BsAs", "Rosario", 5.0),("Rosario", "Miami", 5.0), ("Miami", "Córdoba", 8.0)] "BsAs" "Córdoba" ~?= False,
    "Se puede llegar caso verdadero directo" ~: 
    sePuedeLlegar [("BsAs", "Rosario", 5.0),("Rosario", "Miami", 5.0)] "BsAs" "Rosario" ~?= True,
    "Se puede llegar caso verdadero con una escala, pero mas de un vuelo llega a destino" ~: 
    sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Cordoba","Miami",7.0), ("Rosario", "Miami", 5.0)] "BsAs" "Miami" ~?= True,
    "Se puede ir y volver" ~: 
    sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Miami", 5.0), ("Rosario", "BsAs", 5.0)] "BsAs" "BsAs" ~?= True,
    "Se puede ir de B a A pero no de A a B" ~: 
    sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Miami", 5.0)] "Rosario" "BsAs" ~?= False
    ]

testsEjduracionDelCaminoMasRapido = test [
    "duracionDelCaminoMasRapido Caso 1: directo sin escalas" ~: 
    aproximado(duracionDelCaminoMasRapido [("A", "B", 5.0), ("B", "C", 5.0)] "A" "B") 5.0 ~?= True,
    "duracionDelCaminoMasRapido Caso 2: con una escala" ~: 
    aproximado (duracionDelCaminoMasRapido [("F", "B", 5.0), ("B", "G", 5.0)] "F" "G") 10.0 ~?= True,
    "duracionDelCaminoMasRapido Caso 3: con escala más rapido que directo" ~: 
    aproximado (duracionDelCaminoMasRapido [("C", "B", 5.0), ("B", "V", 5.0),("C", "V", 15.0)] "C" "V") 10.0 ~?= True,
    "duracionDelCaminoMasRapido Caso 3: con directo más rapido que escala" ~: 
    aproximado (duracionDelCaminoMasRapido [("H", "B", 5.0), ("H", "J", 5.0),("H", "J", 4.0)] "H" "J") 4.0 ~?= True,
    "duracionDelCaminoMasRapido Caso 4: con empate directo y escala" ~: 
    aproximado (duracionDelCaminoMasRapido [("K", "G", 5.0), ("G", "L", 5.0),("G", "L", 5.0), ("K", "L", 10.0)] "K" "L") 10.0 ~?= True,
    "duracionDelCaminoMasRapido Caso 5: con empate escala y escala" ~: 
    aproximado (duracionDelCaminoMasRapido [("X", "B", 2.5), ("B", "V", 3.5),("X", "T", 3.5), ("T", "V", 2.5)] "X" "V") 6.0 ~?= True,
    "duracionDelCaminoMasRapido Caso 6: con escala más rapido que directo, con varias escalas diferentes" ~: 
    aproximado (duracionDelCaminoMasRapido [("N", "J", 1.5), ("J", "M", 3.0),("N", "M", 10.0),("N", "T", 3.0), ("G", "M", 6.0) ,("T", "M", 7.0)] "N" "M") 4.5 ~?= True
    ]

testsEjpuedoVolverAOrigen = test [
    "puedoVolverAOrigen Caso 0: sin vuelos" ~: 
    puedoVolverAOrigen [] "A" ~?= False,
    "puedoVolverAOrigen Caso 1: sin vuelta" ~: 
    puedoVolverAOrigen [("M", "N", 3.0)] "M" ~?= False,
    "puedoVolverAOrigen Caso 2: sin vuelta mas elementos" ~: 
    puedoVolverAOrigen [("A", "B", 5.0), ("B", "C", 5.0)] "A" ~?= False,
    "puedoVolverAOrigen Caso 3: con vuelta" ~: 
    puedoVolverAOrigen [("A", "B", 2.0), ("B", "A", 2.0)] "A" ~?= True,
    "puedoVolverAOrigen Caso 4: ciudad como destino pero no hay escala con la ciudad como origen" ~: 
    puedoVolverAOrigen [("A", "B", 2.0), ("B", "D", 2.0), ("T", "D", 2.0)] "D" ~?= False,
    "puedoVolverAOrigen Caso 5: ciudad esta como origen y destino pero no vuelve" ~: 
    puedoVolverAOrigen [("H", "T", 5.0),("T", "F", 5.0),("B", "H", 8.0)] "H" ~?= False,
    "puedoVolverAOrigen Caso 6: vuelve a origen con mas de una escala" ~: 
    puedoVolverAOrigen [("H", "R", 5.0),("R", "Y", 5.0),("Y", "H", 8.0)] "H" ~?= True,
    "puedoVolverAOrigen Caso 7: con un ciclo posible" ~: 
    puedoVolverAOrigen [("A", "B", 2.0), ("B", "C", 3.0), ("C", "B", 4.0),("B", "A", 5.0)] "A" ~?= True,                                                         
    "puedoVolverAOrigen Caso 8: múltiples caminos, solo uno válido" ~: 
    puedoVolverAOrigen [("X", "Y", 3.0), ("Y", "D", 5.0), ("D", "X", 2.0),("D", "Z", 6.0), ("Y", "T", 4.0), ("T", "Y", 4.0)] "X" ~?= True,
    "puedoVolverAOrigen Caso 9: varias escalas necesarias pero vuele a origen" ~: 
    puedoVolverAOrigen [("M", "N", 3.0), ("N", "O", 4.0), ("O", "P", 2.0),("P", "Q", 1.0), ("Q", "M", 8.0)] "M" ~?= True,
    "puedoVolverAOrigen Caso 10: muchas escalas pero no vuelve" ~: 
    puedoVolverAOrigen [("A", "B", 2.0), ("A", "E", 5.0),("B", "C", 3.0),("B", "D", 2.0),("C", "D", 4.0),("D", "E", 5.0), ("D", "F", 3.0), ("E", "F", 6.0), ("F", "G", 7.0)] "A" ~?= False
    ]



-- Funciones extras

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()

--- expectlistProximity modificada para usar en test modernizar flota
expectlistProximity:: [(String,String,Float)] -> [(String,String,Float)] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [(String,String,Float)] -> [(String,String,Float)] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [(String,String,Float)] -> [(String,String,Float)] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno ((a,b,x):xs) ((c,d,y):ys) = (a == c && b == d) && (aproximado x y) && (esParecidoUnaAUno xs ys)  

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)