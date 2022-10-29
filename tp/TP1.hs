
maximoComunDivisor :: Integer -> Integer -> Integer 
maximoComunDivisor x y  | a == b = a                                    -- si son iguales el MCD es cualquiera de ellos
                        | a > b && mod a b == 0 = b                     -- uso Algoritmo de Euclides
                        | b > a && mod b a == 0 = a                     
                        | a > b = maximoComunDivisor b (mod a b)
                        | b > a = maximoComunDivisor (mod b a) a
                            where                                       -- El MCD es siempre positivo, por tanto tomo el valor absoluto
                                a = abs x
                                b = abs y

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | maximoComunDivisor a b == 1 = True                    -- Por definicion si el MCD = 1 son Coprimos
                | otherwise = False



menorDivisorDeDesde :: Integer -> Integer -> Integer                        -- Funcion auxiliar para esPrimo (Clase 5)
menorDivisorDeDesde a n     | mod a n == 0 = n
                            | otherwise = menorDivisorDeDesde a (n+1)

menorDivisor :: Integer -> Integer                                          -- Funcion auxiliar para esPrimo (Clase 5)
menorDivisor n = menorDivisorDeDesde n 2

esPrimo :: Integer -> Bool                                                  -- Si el menor divisor de p es el mismo es porque es primo
esPrimo p   | p == 1 = False
            | menorDivisor p == p = True
            | otherwise = False


esKPseudoprimo :: Integer -> Integer -> Bool
esKPseudoprimo n k  | not (esPrimo n) && (mod (k^(n-1)-1) n == 0) = True    -- Si no es primo pero/y cumple que n divide a k^(n-1)-1 es k-pseudoprimo
                    | otherwise = False

es2Pseudoprimo :: Integer -> Bool                                           -- Aplico para 2
es2Pseudoprimo a = esKPseudoprimo a 2

es3Pseudoprimo :: Integer -> Bool                                           -- Aplico para 3
es3Pseudoprimo a = esKPseudoprimo a 3

cantidadKPseudoprimos :: Integer -> Integer -> Integer 
cantidadKPseudoprimos 1 k = 0
cantidadKPseudoprimos m k   | esKPseudoprimo m k = (cantidadKPseudoprimos (m-1) k) + 1  -- Si es k-pseudoprimo sumo 1 sino no
                            | otherwise = cantidadKPseudoprimos (m-1) k

cantidad3Pseudoprimos :: Integer -> Integer                                             -- Aplico para 3
cantidad3Pseudoprimos m = cantidadKPseudoprimos m 3

proximo2y3Pseudoprimo :: Integer -> Integer -> Integer
proximo2y3Pseudoprimo m n   | m <= 1105 = 1105
                            | es2Pseudoprimo (m + n) && es3Pseudoprimo (m + n) = m + n          -- Voy subiendo de 1 en 1 viendo si son 2y3-pseudoprimo hasta encontrar 1 que sea
                            | otherwise = proximo2y3Pseudoprimo m (n+1)

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k  | k == 1 = 1105
                        | k > 1 = proximo2y3Pseudoprimo (kesimo2y3Pseudoprimo (k - 1) + 1) 1    -- Aplico empezando desde el 2y3-pseudoprimo anterior, por tanto concigo el siguiente


aPseudoprimosDesde :: Integer -> Integer -> Bool                                                            -- Busco que cumplan las condiciones de Charmichael:
aPseudoprimosDesde n a  | a == 1 = True
                        | esPrimo n = False                                                                 -- n no sea primo
                        | not (sonCoprimos n a) = aPseudoprimosDesde n (a-1)
                        | sonCoprimos n a && esKPseudoprimo n a && aPseudoprimosDesde n (a-1) = True        -- si para n no primo se cumple que para todo a es a-pseudoprimo, entonces cumple las condiciones de Charmichael 
                        | otherwise = False

esCarmichael :: Integer -> Bool
esCarmichael n = aPseudoprimosDesde n (n-1)                                                                 -- Defino que si cumple lo de arriba, es de Charmichael 


