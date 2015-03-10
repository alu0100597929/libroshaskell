Lambda Cálculo (cap. 2 del Greg)

Hemos hecho una función a partir de una fórmula reemplazando un objeto con un nombre e identificando el 
nombre que hemos usado. Hemos evaluado la función cambiando el nombre en la fórmula por un nuevo objeto y 
evaluando la fórmula resultante.

La abstracción se basa en la generalización a través de la introducción de un nombre para reemplazar un 
valor y la especialización a través del reemplazo de un nombre con otro valor.

Hay que tener cuidado con esto, y asegurar que los reemplazos se hacen con cosas compatibles, en los 
ejemplos del principio del capítulo, los operandos con números y los operadores con funciones que tomen 2 
números y devuelvan otro.

La abstracción se introduce (creando su función o funciones) y se especializa (mediante su aplicación):

    \<expresión\> ::= \<nombre\> | \<función\> | \<application\>

Los nombres pueden ser cualquier secuencia de carácteres no blancos.

Una lambda función es una abstracción sobre una lambda expresión y tiene la forma:

    \<función\> ::= λ\<nombre\>.\<body\>

donde:

    \<body\> ::= \<expresión\>

por ejemplo:

    λx.x λfirst.λsecond.first λf.λa.(f a)

La λ precede e introduce un nombre usado para la abstracción. El nombre es llamado la variable de ligadura 
y es como un parámetro formal en una declaración de funciones en C++. El `.` separa el nombre de la 
expresión en la cual ese nombre tiene lugar. Esta expresión es llamada el cuerpo de la función.

El cuerpo de la función puede ser cualquier lambda expresión incluyendo otra función. Esto permite a las 
funciones retornar funciones como valores.

nota: las funciones no tienen nombre (las lambdas se llaman también funciones anónimas)

Una aplicación de función tiene la forma:

    \<application\> ::= (\<expresión de la función\> \<argumento de la expresión\>)

donde

    \<expresión de la función\> ::= \<expresión\>
    \<argumento de la expresión\> ::= \<expresión\>

por ejemplo:

    (λx.x λa.λb.b)

La aplicación de una función especializa una abstracción dándole un valor al nombre.

En la aplicación de funciones, también conocida como par ligado, la expresión de la función se aplica a la 
expresión argumento . El lambda cálculo permite a las definiciones de funciones aparecer directamente en 
las llamadas a funciones.

Hay dos formas de evaluar la aplicación de funciones. Ambas devuelven otra función. A continuación, todas 
las ocurrencias de la variable de ligadura son reemplazadas por una de estas dos opciones:

1) el valor de la expresión argumento
2) la expresión argumento  sin evaluar

Finalmente, la expresión del cuerpo de la función se evalúa.

La manera 1) se denomina orden aplicativo, es como la llamada por valor en C++ se evalúa primero la 
expresión del parámetro actual, que luego pasa a ser formal.

La manera 2) se denomina orden normal y es como la llamada por nombre de ciertos lenguajes de 
programación: la expresión del parámetro actual no se evalúa antes de ser pasada al parámetro formal.

/****************************************************/

Los parámetros formales (formal parameters) son aquellos especificados en la declaración de tipos de la 
función. Al declarar un parámetro formal hay que especificar su tipo de dato. Los parámetros formales sólo 
se conocen dentro de la función.

Los parámetros actuales (actual parameters) son las expresiones pasadas como argumento s en la llamada a 
una función. Llamada:

\<nombre-función\> (\<lista parámetros actuales\>);

/****************************************************/

El orden normal es más poderoso y versátil que el aplicativo, pero podría ser más ineficiente.

Recuerda:

\<application\> ::= (\<expresión de la función\> \<argumento de la expresión\>)

donde

  \<expresión de la función\> ::= \<expresión\>
  \<argumento de la expresión\> ::= \<expresión\>

--Supongamos que la identidad se aplica a sí misma: (\\x.x \\x.x)

--expresión de función: \\x.x de la izq. (cuerpo de expresión, la x a la derecha del punto)
--expresión de argumento : \\x.x

Cuando se evalúa esta aplicación, la variable ligada x de la expresión de función se reemplaza por la 
expresión argumento  en el cuerpo de la expresión.

La función identidad es la operación identidad para las lambda funciones.

2.7 Función que se aplica a sí misma.

--(\\x.x \\s . (s s))

Si aplicamos identidad:

\\x.x \\s . (s s) se convierte en: \\s . (s s)

Ahora, hagamos lo contrario:

(\\s . (s s) \\x.x)

(\\x.x \\x.x) lo cual hemos visto que da (\\x.x)

Ahora, apliquemos la función que se aplica a sí misma sobre sí misma:

(λs.(s s) λs.(s s))

(λs.(s s) λs.(s s)) como el resultado es exactamente el mismo, concluímos con que esto no va a avanzar 
más, quedando un "bucle infinito".

Por tanto, se ha visto que no toda la evaluación de funciones termina. Además, se verá que no hay forma de 
decir a priori si una evaluación terminará en algún momento.

\\func.\\arg. (func arg)

Variable de ligadura: func
Expresión de cuerpo (otra función): \\arg. (func arg)
Esta otra función tiene
Variable de ligadura: arg
Expresión de cuerpo: (func arg) la cual es una aplicación de función donde
expresión de función: func
expresión de argumento : arg

((λfunc.λarg.(func arg) λx.x) λs.(s s)) 

--2.6 función identidad:

identidad = \x -> x
Lambda Cálculo (cap. 2 del Greg)

Hemos hecho una función a partir de una fórmula reemplazando un objeto con un nombre e identificando el 
nombre que hemos usado. Hemos evaluado la función cambiando el nombre en la fórmula por un nuevo objeto y 
evaluando la fórmula resultante.

La abstracción se basa en la generalización a través de la introducción de un nombre para reemplazar un 
valor y la especialización a través del reemplazo de un nombre con otro valor.

Hay que tener cuidado con esto, y asegurar que los reemplazos se hacen con cosas compatibles, en los 
ejemplos del principio del capítulo, los operandos con números y los operadores con funciones que tomen 2 
números y devuelvan otro.

La abstracción se introduce (creando su función o funciones) y se especializa (mediante su aplicación):

    \<expresión\> ::= \<nombre\> | \<función\> | \<application\>

Los nombres pueden ser cualquier secuencia de carácteres no blancos.

Una lambda función es una abstracción sobre una lambda expresión y tiene la forma:

    \<función\> ::= λ\<nombre\>.\<body\>

donde:

    \<body\> ::= \<expresión\>

por ejemplo:

    λx.x λfirst.λsecond.first λf.λa.(f a)

La λ precede e introduce un nombre usado para la abstracción. El nombre es llamado la variable de ligadura 
y es como un parámetro formal en una declaración de funciones en C++. El `.` separa el nombre de la 
expresión en la cual ese nombre tiene lugar. Esta expresión es llamada el cuerpo de la función.

El cuerpo de la función puede ser cualquier lambda expresión incluyendo otra función. Esto permite a las 
funciones retornar funciones como valores.

nota: las funciones no tienen nombre (las lambdas se llaman también funciones anónimas)

Una aplicación de función tiene la forma:

    \<application\> ::= (\<expresión de la función\> \<argumento de la expresión\>)

donde

    \<expresión de la función\> ::= \<expresión\>
    \<argumento de la expresión\> ::= \<expresión\>

por ejemplo:

    (λx.x λa.λb.b)

La aplicación de una función especializa una abstracción dándole un valor al nombre.

En la aplicación de funciones, también conocida como par ligado, la expresión de la función se aplica a la 
expresión argumento . El lambda cálculo permite a las definiciones de funciones aparecer directamente en 
las llamadas a funciones.

Hay dos formas de evaluar la aplicación de funciones. Ambas devuelven otra función. A continuación, todas 
las ocurrencias de la variable de ligadura son reemplazadas por una de estas dos opciones:

1) el valor de la expresión argumento
2) la expresión argumento  sin evaluar

Finalmente, la expresión del cuerpo de la función se evalúa.

La manera 1) se denomina orden aplicativo, es como la llamada por valor en C++ se evalúa primero la 
expresión del parámetro actual, que luego pasa a ser formal.

La manera 2) se denomina orden normal y es como la llamada por nombre de ciertos lenguajes de 
programación: la expresión del parámetro actual no se evalúa antes de ser pasada al parámetro formal.

/****************************************************/

Los parámetros formales (formal parameters) son aquellos especificados en la declaración de tipos de la 
función. Al declarar un parámetro formal hay que especificar su tipo de dato. Los parámetros formales sólo 
se conocen dentro de la función.

Los parámetros actuales (actual parameters) son las expresiones pasadas como argumento s en la llamada a 
una función. Llamada:

\<nombre-función\> (\<lista parámetros actuales\>);

/****************************************************/

El orden normal es más poderoso y versátil que el aplicativo, pero podría ser más ineficiente.

Recuerda:

\<application\> ::= (\<expresión de la función\> \<argumento de la expresión\>)

donde

  \<expresión de la función\> ::= \<expresión\>
  \<argumento de la expresión\> ::= \<expresión\>

--Supongamos que la identidad se aplica a sí misma: (\\x.x \\x.x)

--expresión de función: \\x.x de la izq. (cuerpo de expresión, la x a la derecha del punto)
--expresión de argumento : \\x.x

Cuando se evalúa esta aplicación, la variable ligada x de la expresión de función se reemplaza por la 
expresión argumento  en el cuerpo de la expresión.

La función identidad es la operación identidad para las lambda funciones.

2.7 Función que se aplica a sí misma.

--(\\x.x \\s . (s s))

Si aplicamos identidad:

\\x.x \\s . (s s) se convierte en: \\s . (s s)

Ahora, hagamos lo contrario:

(\\s . (s s) \\x.x)

(\\x.x \\x.x) lo cual hemos visto que da (\\x.x)

Ahora, apliquemos la función que se aplica a sí misma sobre sí misma:

(λs.(s s) λs.(s s))

(λs.(s s) λs.(s s)) como el resultado es exactamente el mismo, concluímos con que esto no va a avanzar 
más, quedando un "bucle infinito".

Por tanto, se ha visto que no toda la evaluación de funciones termina. Además, se verá que no hay forma de 
decir a priori si una evaluación terminará en algún momento.

\\func.\\arg. (func arg)

Variable de ligadura: func
Expresión de cuerpo (otra función): \\arg. (func arg)
Esta otra función tiene
Variable de ligadura: arg
Expresión de cuerpo: (func arg) la cual es una aplicación de función donde
expresión de función: func
expresión de argumento : arg

((λfunc.λarg.(func arg) λx.x) λs.(s s)) 
dobleAplicacionIdentidad = (\x -> identidad x)

{-Recuerda:

<application> ::= (<function expression> <argument expression>)

donde

	<function expression> ::= <expression>
	<argument expression> ::= <expression>-}

--Supongamos que la identidad se aplica a sí misma: (\x.x \x.x)


--expresión de función: \x.x de la izq. (cuerpo de expresión, la x a la derecha del punto)
--expresión de argumento: \x.x

Cuando se evalúa esta aplicación, la variable ligada x de la expresión de función
se reemplaza por la expresión argumento en el cuerpo de la expresión.

La función identidad es la operación identidad para las lambda funciones.

2.7 Función que se aplica a sí misma.

--(\x.x \s . (s s))

Si aplicamos identidad:

\x.x \s . (s s) se convierte en: \s . (s s)

Ahora, hagamos lo contrario:

(\s . (s s) \x.x)

(\x.x \x.x) lo cual hemos visto que da (\x.x)

Ahora, apliquemos la función que se aplica a sí misma sobre sí misma:

(λs.(s s) λs.(s s))

(λs.(s s) λs.(s s)) como el resultado es exactamente el mismo, concluímos
con que esto no va a avanzar más, quedando un "bucle infinito".

Por tanto, se ha visto que no toda la evaluación de funciones termina. Además,
se verá que no hay forma de decir a priori si una evaluación terminará en
algún momento.