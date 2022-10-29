-- Apellido Nombre #1
-- Apellido Nombre #2
-- Apellido Nombre #3

sumaDivH n 1 = 1
sumaDivH n k = sumaDivH n (k - 1) + a
               where a | mod n k == 0 = k
                       | otherwise = 0

sumaDiv n = sumaDivH n n
esPrimo n = sumaDiv n == n + 1

-- EJERCICIO 1. sonCoprimos

mcd :: Integer -> Integer -> Integer
-- Es necesario que m<=n
mcd n m | abs m > abs n = mcd m n
        | mod n m == 0 = m
        | otherwise = mcd n (mod n m)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = mcd n m == 1

-- EJE3RCICO 2: es2Pseudoprimo
--n compuesto para los cuales n divide a 2^(n−1) − 1.

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = esPrimo n == False && mod (2^(n-1) - 1) n == 0

-- EJERCICIO 3: cantidad3Pseudoprimos
--cantidad3Pseudoprimos :: Integer -> Integer
--Dado un número natural m calcula la cantidad de 3-pseudoprimos que hay entre 1 y m inclusive

es3Pseudoprimo n = esPrimo n == False && mod (3^(n-1) - 1) n == 0

cantidad3Pseudoprimos m | m == 1 = 0
                        | es3Pseudoprimo m == True = 1 + cantidad3Pseudoprimos (m - 1)
                        | otherwise = cantidad3Pseudoprimos (m - 1)

-- EJERCICIO 4: kesimo2y3Pseudoprimo
--kesimo2y3Pseudoprimo :: Integer -> Integer
--Dado un número natural k calcula el k-ésimo número que es simuláneamente 2-pseudoprimo y 3-pseudoprimo

sigPseudo n | es2Pseudoprimo (n + 1) == True && es3Pseudoprimo (n + 1) == True = n + 1
            | otherwise = sigPseudo (n + 1)

kEsimoPseudo k | k == 1 = 1105
               | otherwise = sigPseudo (kEsimoPseudo (k - 1))

-- EJERCICIO 5: esCarmichael
--esCarmichael :: Integer -> Bool
--Números naturales compuestos n que son a-pseudoprimos para todo número natural a entre 1 y n−1 que sea coprimo con n
