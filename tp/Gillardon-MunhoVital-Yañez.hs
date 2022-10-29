-- Gilardon Bautista 
-- Munhó Vital Facundo Nicolas
-- Yañez Carolina 

-- Inicio Funciones Auxiliares

{-----------------------------------------------------------------------------------------}
menorDivisordesde :: Integer -> Integer -> Integer
{-Funciń que dado x e y busca cual es el menor divisore de x desde el numero y. Ambos naturales-}
menorDivisordesde x y | mod x y == 0 = y
                      | otherwise = menorDivisordesde x (y+1)
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
menorDivisor :: Integer -> Integer 
{-dado x natural, devuelve su menor divisor-}
menorDivisor x = menorDivisordesde x 2
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
{-funcion que dado dos numeros busca su menor divisor comun-}
mcd :: Integer -> Integer -> Integer
mcd x y  | a < b = mcd b a
         | a == b = a   -- si son iguales el MCD es cualquiera de ellos
         | mod a b == 0 = b -- uso Algoritmo de Euclides
         | otherwise = mcd b (mod a b)
                where      -- El MCD es siempre positivo, por tanto tomo el valor absoluto
                        a = abs x
                        b = abs y
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
esPrimo :: Integer -> Bool
{-funcion que verifica si un numero x es primo o no-}
esPrimo x | menorDivisor x == x = True
          | otherwise= False
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
congruentes :: Integer -> Integer -> Integer -> Bool
{-funcion que dado numeros enteros d a r verifica si a es conruente r mod d
o equivalentemente d|a-r -}
congruentes d a r | mod (a-r) d == 0 = True
                  | otherwise = False
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
esKPseudoprimo :: Integer -> Integer -> Bool
{-Funcion que verifica si n es kpseudoprimo-}
esKPseudoprimo n k  | not (esPrimo n) && (mod (k^(n-1)-1) n == 0) = True    -- Si no es primo pero/y cumple que n divide a k^(n-1)-1 es k-pseudoprimo
                    | otherwise = False
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
cantidadKPseudoprimos :: Integer -> Integer -> Integer
{-Funcion que cuenta entre 1 y m cuantos k-psuedoprimos hay-}
cantidadKPseudoprimos 1 _ = 0
cantidadKPseudoprimos m k   | esKPseudoprimo m k = cantidadKPseudoprimos (m-1) k + 1  -- Si es k-pseudoprimo sumo 1 sino no
                            | otherwise = cantidadKPseudoprimos (m-1) k
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
aPseudoprimosDesde :: Integer -> Integer -> Bool
{-Funcion que verifica desde donde n es a pseudoprimo-}
aPseudoprimosDesde n a  | a == 1 = True
                        | esPrimo n = False                                                                 -- n no sea primo
                        | not (sonCoprimos n a) = aPseudoprimosDesde n (a-1)
                        | sonCoprimos n a && esKPseudoprimo n a && aPseudoprimosDesde n (a-1) = True        -- si para n no primo se cumple que para todo a es a-pseudoprimo, entonces cumple las condiciones de Charmichael 
                        | otherwise = False
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
es3Pseudoprimo :: Integer -> Bool
{-Funcion que verifica si a es 3 pseudoprimo-}
es3Pseudoprimo a = esKPseudoprimo a 3
{-----------------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------------}
siguiente2y3psudoprimo :: Integer -> Integer
{-Funcion que dado un numero n natural verifica cual es el siguiente numero que es 2pseudoprimo y 3pseudoprimo-}
siguiente2y3psudoprimo n
                         | es3Pseudoprimo (n+1) && es2Pseudoprimo (n+1) = n+1
                         | otherwise = siguiente2y3psudoprimo (n+1)
{-----------------------------------------------------------------------------------------}

-- Fin Funciones Auxiliares

-- EJERCICIO 1. sonCoprimos
sonCoprimos :: Integer -> Integer -> Bool
{-Funcion que dado dos Enteros verifica si son coprimos, mirando su mcd-}
sonCoprimos n m = mcd n m == 1

-- EJE3RCICO 2: es2Pseudoprimo
es2Pseudoprimo :: Integer -> Bool
{-Función que verifica si a es 2 pseudoprimo-}
es2Pseudoprimo a = esKPseudoprimo a 2


-- EJERCICIO 3: cantidad3Pseudoprimos
cantidad3Pseudoprimos :: Integer -> Integer
{-Funcion que dado un rango [1,m] dice cuantos 3psudoprimos hay-}
cantidad3Pseudoprimos m = cantidadKPseudoprimos m 3


-- EJERCICIO 4: kesimo2y3Pseudoprimo
kesimo2y3Pseudoprimo :: Integer -> Integer
{-Funcion que te devuelve que k-esimo numero que es 2psudoprimo y 3pseudoprimo-}
kesimo2y3Pseudoprimo k | k == 1 = 1105
                       | otherwise = siguiente2y3psudoprimo (kesimo2y3Pseudoprimo(k-1))


-- EJERCICIO 5: esCarmichael
esCarmichael :: Integer -> Bool
{-Funcion que decide si n es un numero de Carmichael o no-}
esCarmichael n = aPseudoprimosDesde n (n-1)