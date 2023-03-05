# Autómatas celulares con agentes

## Idea general

El proyecto es un DSL para hacer simulaciones que combina funcionalidades de autómatas celulares y modelos basados en agentes.
El modelo consiste en una matriz de células. Éstas pueden ser de distintos tipos, los cuales tienen atributos propios, estados diferentes y reglas de transición entre ellos diferentes también. En el lenguaje se especifican las condiciones de la simulación y en la ejecución se recopila toda esa información y se simulan las interacciones entre las células.

Para crear reglas de transición hay expresiones booleanas que tienen en cuenta el estado del vecindario de cada célula (el cual puede ser mayor o menor dependiendo del atributo 'sight' de cada agente) y los atributos propios. También en las reglas se incluye la acción a realizar si la expresión fue 'true'. Se puede cambiar el estado actual de la célula o modificar el valor de cierto atributo de la misma.

## Sintaxis

Se definen los siguientes comandos:

- definir agente: con nombre, estados (junto con un color para graficarlo), reglas de transición, atributos propios, etc
- incluir agente: dado el nombre de un agente antes definido y una cantidad, se lo incluye n veces en la próxima matriz de simulación
- setear iteraciones: establece el numero de iteraciones para la próxima simulación
- simular: dadas las dimensiones de la matriz, la crea con los agentes establecidos. La ubicación de los mismos sería aleatoria o se puede pasar un archivo con cierta matriz ya establecida.

Ejemplo:

```
define (nombre_de_agente, sight 2) {
  attributes:
    att1 0, att2 10
  
  states:
    st1 white, st2 color 36 114 255
  
  rules:
    st1 : true -> newState st2
}

setAgent nombre_de_agente 16
unsetAgent nombre_de_agente
setIterations 50
start 4 4
startPath "archivo.txt"
```

### Gramática de Reglas de Transición

```
Rule          : STRING ':' BoolExp '->' Result

Result        : 'newState' STRING
              | 'changeAttribute' STRING IntExp

BoolExp       : 'true'
              | 'false'
              | BoolExp 'and' BoolExp
              | BoolExp 'or' BoolExp
              | 'not' BoolExp
              | 'neighborState' NUM '==' STRING
              | 'neighborType' NUM '==' STRING
              | IntExp '==' IntExp
              | IntExp '<' IntExp
              | IntExp '>' IntExp
              | '(' BoolExp ')'

IntExp        : NUM
              | 'countTypes' STRING Neighbors
              | 'countStatus' STRING Neighbors
              | 'attribute' STRING
              | IntExp '+' IntExp
              | IntExp '-' IntExp
              | IntExp '/' NUM
              | IntExp '*' IntExp
              | '(' IntExp ')'
```

## Organización

Los archivos están organizados de la siguiente manera:

- app/
  - Main.hs : archivo principal, parsea y ejecuta la simulación

- src/
  - Agents.hs : funciones útiles que operan con el tipo de datos 'Agent'
  - AST.hs : se definen todos los tipos de datos. 'Agent' contiene la información referente a una célula de un tipo. 'Env' es el estado que se utiliza en la mónada StateError. 'Simulation' es el tipo de datos que obtenemos luego de evaluar los comandos y previo a ejecutar las mismas.
  - Cellular.hs : contiene la lógica para crear la simulación y sus gráficos. Se usa la función simulate de Graphics.Gloss.
  - Environment.hs
  - Eval.hs : Funciones para pasar de un comando a obtener un estado en el que se incluyen los datos de tipo 'Simulation'.
  - Monads.hs
  - Parse.hs
  - Parse.y : Define la gramática y crea el parser.
  - ParseGrid : Parsea usando Parsec matrices dadas para poder simular no sólo agentes con posiciones aleatorias.

- examples/ : alli se guardan los archivos a ejecutar en el lenguaje

- grids/ : matrices usadas en algunas simulaciones

## Instalación y uso

Primero compilar usando:

```
> stack build
```

Para correr un archivo (por ejemplo conway.sim):

```
> stack exec cellular-automaton ./examples/conway.sim 20 5
```

Se pueden pasar como argumentos el tamaño de cada célula y el número de iteraciones a graficar por segundo.

## Diseño

Se representan las matrices de agentes mediante vectores para un acceso más rápido en la ejecución.

En `Eval` se transforman comandos en tipos `Simulation`.
Pero el trabajo se realiza después al ejecutar las funciones de `Cellular`. Porque al momento de transformar un comando no se sabe nada sobre el estado de la simulación, entonces sólo se crean funciones que se ejecutarán luego (Sobre todo al transformar `BoolExp` e `IntExp`).

## Bibliografía

Documentación de Gloss:

- https://hackage.haskell.org/package/gloss 

Se usó de inspiración para graficar las matrices.

- https://www.47deg.com/blog/game-of-life-haskell/

Se usó para implementar la lógica de los vecindarios de cada célula.

- https://blog.jayway.com/2020/10/26/cellular-automaton-in-haskell-i-conways-game-of-life/

También se usaron los trabajos realizados en el cursado para crear `ParseGrid`, `Monads` y para la estructuración del código.