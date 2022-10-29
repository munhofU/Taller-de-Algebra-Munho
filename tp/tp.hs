-- Apellido Nombre #1
-- Apellido Nombre #2
-- Apellido Nombre #3


-- Funciones auxiliares

menorDivisordesde :: Integral t => t -> t -> t
{-Funciń que dado x e y busca cual es el menor divisore de x desde el numero y. Ambos naturales-}
menorDivisordesde x y | mod x y == 0 = y
                      | otherwise = menorDivisordesde x (y+1)


{-Funcion que dado un x da el menor divisor de este numero distinto de 1, utiliza menorDivisordesde-}
menorDivisor :: Integral t => t -> t
{-dado x natural, devuelve su menor divisor-}
menorDivisor x = menorDivisordesde x 2

{-funcion que verifica si un numero x es primo o no-}
esPrimo :: Integer -> Bool
esPrimo x | menorDivisor x == x = True
          | otherwise= False

{-funcion que dado numeros enteros d a r verifica si a es conruente r mod d
o equivalentemente d|a-r -}
congruentes :: Integer -> Integer -> Integer -> Bool
congruentes d a r | mod (a-r) d == 0 = True
                  | otherwise = False

{-funcion que dado dos a n verfica si n compuesto es a pseudoprimo
es decir, verifica si n| a^(n-1)-1 -}
apseudoprimos :: Integer -> Integer -> Bool
apseudoprimos a n = congruentes n (a^(n-1)) 1 && not (esPrimo n)

es3Pseudoprimo :: Integer -- ^ n natural al cual se le verifica si es 3psudoprimos
  -> Bool
es3Pseudoprimo n = apseudoprimos 3 n

cantidad3Pseudoprimosdesde :: Num t => Integer -- ^ numero m que da el maximo donde contar 3pseudoprimos
  -> Integer -- ^ p contador auxiliar que empieza desde algun natural que aumenta hasta llegar a m
  -> t -- ^ n contador auxiliar que aumenta cuando el numero p es 3pseudprimos, tambien es el resultado
  -> t
cantidad3Pseudoprimosdesde m p n | p == m = n
                                 | p >= m = n
                                 | p < m && es3Pseudoprimo p = cantidad3Pseudoprimosdesde m (p+1) (n+1)
                                 | otherwise = cantidad3Pseudoprimosdesde m (p+1) n

siguiente3y2psudoprimo :: Integer -> Integer
siguiente3y2psudoprimo n 
                         | es3Pseudoprimo (n+1) && es2Pseudoprimo (n+1) = n+1
                         | otherwise = siguiente3y2psudoprimo (n+1)

{-funcion que dado dos numeros busca su menor divisor comun-}
mcd :: Integer -> Integer -> Integer
mcd x y | abs y > abs x = mcd y x
mcd x 0 = abs x
mcd x y = mcd y (mod x y)


-- EJERCICIO 1. sonCoprimos
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x y | mcd x y == 1 = True
                | otherwise = False

-- EJE3RCICO 2: es2Pseudoprimo
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = apseudoprimos 2 n

-- EJERCICIO 3: cantidad3Pseudoprimos
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = cantidad3Pseudoprimosdesde (m+1) 2 0

-- EJERCICIO 4: kesimo2y3Pseudoprimo
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k | k == 1 = 1105
                       | otherwise = siguiente3y2psudoprimo (kesimo2y3Pseudoprimo(k-1))

-- EJERCICIO 5: esCarmichael
--esCarmichael :: Integer -> Bool