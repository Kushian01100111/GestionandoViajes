# Gestionando Viajes

Proyecto de programación funcional en Haskell centrado en el modelado y análisis de una red de vuelos entre ciudades.

El sistema representa la oferta de vuelos de una agencia de viajes y resuelve distintos problemas sobre conectividad, validación, rutas y tiempos de viaje. A partir de una lista de vuelos, permite verificar si la información es válida, encontrar ciudades conectadas, reducir duraciones, buscar caminos posibles y calcular la ruta más rápida entre dos destinos.

## Qué resuelve

El proyecto trabaja sobre una agencia de viajes modelada como una secuencia de vuelos entre ciudades, donde cada vuelo tiene:

- ciudad de origen
- ciudad de destino
- duración

A partir de esa estructura, el sistema implementa operaciones como:

- validación de vuelos y ofertas
- detección de ciudades conectadas
- modernización de la flota reduciendo tiempos de viaje
- identificación de la ciudad con mayor conectividad
- búsqueda de rutas directas o con escalas
- cálculo del camino más rápido
- verificación de ciclos para volver al origen

## Ideas principales del proyecto

Este proyecto me permitió practicar varios conceptos importantes de programación funcional y modelado de problemas:

- **Modelado de dominio** con tipos simples como `Ciudad`, `Vuelo` y `AgenciaDeViajes`.
- **Validación de datos** para garantizar consistencia en la red de vuelos.
- **Análisis de conectividad** entre nodos de una red de ciudades.
- **Búsqueda de rutas** con restricciones sobre escalas y duración.
- **Resolución declarativa de problemas** usando las herramientas vistas en la materia.
- **Testing** para cubrir todos los ejercicios pedidos por el trabajo práctico.

## Funcionalidades principales

Entre los problemas resueltos se encuentran:

- `vuelosValidos`: verifica si una agencia tiene vuelos consistentes y sin repeticiones inválidas
- `ciudadesConectadas`: obtiene las ciudades conectadas directamente con una ciudad dada
- `modernizarFlota`: reduce en un 10 % la duración de todos los vuelos
- `ciudadMasConectada`: encuentra una ciudad con la mayor cantidad de conexiones
- `sePuedeLlegar`: determina si existe una ruta directa o con una escala entre dos ciudades
- `duracionDelCaminoMasRapido`: calcula la duración mínima para llegar entre dos ciudades
- `puedoVolverAOrigen`: verifica si existe un circuito que permita regresar a la ciudad de origen

## Qué muestra este proyecto

Más allá de ser un trabajo académico, este proyecto me permitió ejercitar:

- razonamiento sobre relaciones entre ciudades
- modelado de estructuras de datos simples pero expresivas
- resolución de problemas de conectividad y rutas
- diseño declarativo en Haskell
- escritura de tests para validar comportamiento

## Tecnologías

- **Haskell**

## Contexto

Este proyecto fue desarrollado en el marco de la materia **Introducción a la Programación**, con foco en programación funcional, recursión, pattern matching, declaratividad y testing.

## Estructura del problema

El dominio se modela a partir de:

```haskell
type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]
```

A partir de estas definiciones, se implementan las funciones pedidas para operar sobre la red de vuelos.

## Ejemplo de entrada

```haskell
[("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)]
```

Con este tipo de estructura, el sistema puede responder preguntas como:

- ¿los vuelos son válidos?
- ¿a qué ciudades se puede llegar?
- ¿cuál es la ciudad más conectada?
- ¿cuál es la ruta más rápida?
- ¿se puede volver al origen?
