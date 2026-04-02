{- 
Nombre grupo: f_class_programmer

+Nombre: Josefina Hermida
+Mail: jota.herma8@gmail.com
+DNI: 45013356

+Nombre: Facundo Mazzoni
+Mail: facumazzoni2000@gmail.com
+DNI: 42646643

+Nombre: Pedro Julio Comas Gomez
+Mail: comaspedro6@gmail.com
+DNI: 95544754
-}
module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad,Ciudad,Duracion)
type AgenciaDeViajes =[Vuelo]

-- EJERCICIO 1
vueloRepetido :: Vuelo -> AgenciaDeViajes -> Bool
vueloRepetido _ [] = False
vueloRepetido (origen0, destino0, d) ((origen1,destino1,_):xs) | origen0 == origen1 && destino0 == destino1 = True
                                                               | otherwise = vueloRepetido (origen0, destino0, d) xs

vueloValido:: Vuelo -> Bool
vueloValido (origen, destino, duracion) = origen /= destino && duracion > 0

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (vuelo:xs) | vueloValido vuelo && not (vueloRepetido vuelo xs) = vuelosValidos xs
                         | otherwise = False

-- EJERCICIO 2
sacarCiudad:: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes 
sacarCiudad [] _ = []
sacarCiudad ((o,d,t):xs) c | o == c || d == c = sacarCiudad xs c
                           | otherwise = (o,d,t):sacarCiudad xs c


ciudadesConectadas :: AgenciaDeViajes -> Ciudad-> [Ciudad]
ciudadesConectadas [] c= []
ciudadesConectadas ((o,d,t):xs) c | c == o = d:ciudadesConectadas (sacarCiudad xs d) c
                                  | c == d = o:ciudadesConectadas (sacarCiudad xs o) c
                                  | otherwise = ciudadesConectadas xs c

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((x,y,z):xs) = (x,y,z*0.9): modernizarFlota xs

-- EJERCICIO 4
masConexiones:: AgenciaDeViajes-> [Ciudad]-> Ciudad
masConexiones _ [x]= x
masConexiones agencia (x:y:xs) | cantidad agencia x >= cantidad agencia y = masConexiones agencia (x:xs)
                               | otherwise = masConexiones agencia (y:xs)

cantidad :: AgenciaDeViajes -> Ciudad -> Int
cantidad [] _ = 0 
cantidad ((o,d,t):xs) ciudad | o == ciudad || d == ciudad = 1 + cantidad xs ciudad
                             | otherwise = cantidad xs ciudad

todasLasCiudades :: AgenciaDeViajes -> [Ciudad]
todasLasCiudades [] = []
todasLasCiudades ((a,b,c):xs)  | pertenece a (todasLasCiudades xs) && pertenece b (todasLasCiudades  xs) = todasLasCiudades xs
                               | pertenece a (todasLasCiudades xs) = b:todasLasCiudades xs  
                               | pertenece b (todasLasCiudades xs) = a:todasLasCiudades xs
                               | otherwise = a:b:todasLasCiudades xs 

pertenece :: (Eq t)=> t ->[t]->Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs 

eliminarrepetidos :: AgenciaDeViajes -> AgenciaDeViajes
eliminarrepetidos [] =[]
eliminarrepetidos ((a,b,c):xs) | (pertenece (a,b,c) xs || pertenece (b,a,c) xs)=eliminarrepetidos xs
                               | otherwise =(a,b,c):eliminarrepetidos xs

ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada agencia = masConexiones (eliminarrepetidos agencia) (todasLasCiudades agencia)

-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False
sePuedeLlegar agencia origen destino | llegaVueloADestino agencia destino && saleVueloDeOrigen agencia origen = conectanOrigenDestino agencia origen destino
                                     | otherwise = False

{-Chequeo que exista al menos un vuelo que salga de origen y llegue a destino-}

llegaVueloADestino :: AgenciaDeViajes -> Ciudad -> Bool
llegaVueloADestino [] _ = False
llegaVueloADestino ((x,y,z):xss) destino | destino == y = True
                                         | otherwise = llegaVueloADestino xss destino

saleVueloDeOrigen :: AgenciaDeViajes -> Ciudad -> Bool
saleVueloDeOrigen [] _ = False
saleVueloDeOrigen ((x,y,z):xss) origen | origen == x = True
                                       | otherwise = saleVueloDeOrigen xss origen

{-Uso conectanOrigenDestino como funcion aux para ver si hay vuelo directo o si se puede llegar con maximo una coneccion-}

conectanOrigenDestino :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
conectanOrigenDestino (x:xss) origen destino = vueloDirecto (x:xss) origen destino || conectanConEscala (x:xss) (x:xss) origen destino

vueloDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
vueloDirecto [] _ _ = False
vueloDirecto ((x,y,z):xss) origen destino | origen == x && destino == y = True
                                          | otherwise = vueloDirecto xss origen destino 

{-Utilizo dos Agencias para mantener una completa (la que se utiliza para buscar el primer vuelo) y otra que con cada 
  recursion va a descartar el primer vuelo (para asegurarme de que, en caso de haber mas de un vuelo hacia destino, pueda 
  chequearlos todos)-}

conectanConEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
conectanConEscala yss [] _ _ = False
conectanConEscala viajes (x:xss) origen destino | auxDestinoTieneEscala viajes origen (destinoTieneEscala (x:xss) origen destino) = True
                                                | otherwise = conectanConEscala viajes xss origen destino

{-destinoTieneEscala devuelve la ciudad con la que hace escala-}

destinoTieneEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> Ciudad
destinoTieneEscala [] _ _ = ""
destinoTieneEscala ((x,y,z):xss) origen destino | destino == y = x
                                                | otherwise = destinoTieneEscala xss origen destino

{-auxDestinoTieneEscala se fija que haya un viaje de origen a la ciudad que nos da la funcion anterior, en caso de no haber se vuelve a conectanConEscala,
  que desecha el viaje que se utilizo para ver si en la lista hay otro viaje que llegue a destino-}

auxDestinoTieneEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
auxDestinoTieneEscala [] _ _ = False
auxDestinoTieneEscala (x:xss) origen "" = False
auxDestinoTieneEscala ((x,y,z):xss) origen conex | origen == x && conex == y = True
                                                 | otherwise = auxDestinoTieneEscala xss origen conex

 -- EJERCICIO 6
explorarDestinos0:: [Vuelo] -> [Vuelo] -> Ciudad -> Bool
explorarDestinos0 [] _ _  = False
explorarDestinos0 [(x,y,d)] [] destino = y == destino
explorarDestinos0 ((x,y,d):agencia) agenciaBase destino | y == destino = True
                                                        | otherwise = explorarDestinos0 agencia agenciaBase destino


{--vuelosHasta: devuelve todos los vuelos que llegen a destino, que esten dentro de una lista de vuelos determinada--} 
vuelosHasta::Ciudad -> [Vuelo] -> [Vuelo]  
vuelosHasta _ [] = []
vuelosHasta destino ((x,y,d):agencia) | y == destino = (x,y,d) : vuelosHasta destino agencia
                                      | otherwise = vuelosHasta destino agencia

{--Caminos: devuelve una lista de caminos posibles desde la ciudad de origen al destino de a lo sumo 1 escala--} 
caminos:: Ciudad -> [Vuelo] -> [Vuelo] -> [[Vuelo]] 
caminos _ [] _  = []
caminos destino ((x,y,d):xs) zs | y == destino = [(x,y,d)] : caminos destino xs zs  
                                | explorarDestinos0 (vuelosDesde zs y) (sinXCiudad zs y) destino = vuelos : caminos destino xs zs
                                | otherwise = caminos destino xs zs
                                where vuelos = (x,y,d) : vuelosHasta destino (vuelosDesde zs y)

{-duracionCamino: devuelve la suma total de la duracion de viajes dentro de una lista de vuelos-}
duracionCamino:: [Vuelo] -> Duracion 
duracionCamino [] = 0.0
duracionCamino ((_,_,duracion):xs) = duracion + duracionCamino xs

{-caminoMasRapido: devuelve la duracion del camino mas rapido de una lista de listas de vuelos-}
caminoMasRapido:: [[Vuelo]] -> Duracion
caminoMasRapido [x] = duracionCamino x
caminoMasRapido (x:y:zs) | duracionCamino x <= duracionCamino y = caminoMasRapido (x:zs)
                         | otherwise = caminoMasRapido (y:zs)


duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido agencia origen destino = caminoMasRapido(caminos destino (vuelosDesde agencia origen) (sinXCiudad agencia origen))

-- EJERCICIO 7
sinXCiudad:: [Vuelo] -> Ciudad -> [Vuelo] 
sinXCiudad [] _  = []
sinXCiudad ((x,y,d):agencia) ciudad | x == ciudad = sinXCiudad agencia ciudad
                                    | otherwise = (x,y,d) : sinXCiudad agencia ciudad

vuelosDesde:: [Vuelo] -> Ciudad -> [Vuelo] 
vuelosDesde [] _ = []
vuelosDesde ((x,y,d):agencia) origen | x == origen = (x,y,d) : vuelosDesde agencia origen
                                     | otherwise = vuelosDesde agencia origen

explorarDestinos:: [Vuelo] -> [Vuelo] -> Ciudad -> Bool
explorarDestinos [] _ _  = False
explorarDestinos [(x,y,d)] [] origen = y == origen
explorarDestinos ((x,y,d):agencia) agenciaBase origen | y == origen = True
                                                      | explorarDestinos (vuelosDesde agenciaBase y) (sinXCiudad agenciaBase y) origen = True
                                                      | otherwise = explorarDestinos agencia agenciaBase origen 

puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = explorarDestinos (vuelosDesde vuelos origen) (sinXCiudad vuelos origen) origen 