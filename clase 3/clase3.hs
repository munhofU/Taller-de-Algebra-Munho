{-factorial :: Int -> Int
{-Funcion que dado un numero entero positivo devuelve su factorial-}
factorial n 
    |n == 0 =1
    |otherwise n * factorial (n-1)-}

factorial :: Int -> Int
{-Funcion que dado un numero entero positivo devuelve su factorial-}
factorial 0 = 1
factorial n = n * (factorial (n -1))


esPar :: Int -> Bool
{-Funcion que dado numero entero positivo devuelve True o False según si es para o no-}
esPar n 
    | n ==0 = True
    | otherwise = not ( esPar (n -1) )

    
fib :: Int -> Int
{-Funcion que dado un n Entero mayor igual al 0 devuelve el n-esimo termino de fibonacci-}
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

parteEntera :: (Ord t, Num t, Num p) => t -> p
{-Funcion que dado un k Entero mayor igual a 0 devuelve la parte Enterea de un Entero mayor a uno-}
parteEntera a | a < 1 = 0
              | otherwise = (parteEntera (a-1)) + 1

multiplo3 :: (Eq t, Num t) => t -> Bool
{-Funcion que dado un k Entero mayor igual a 0 devuelve si es o no Multiplo de 3-}
multiplo3 k | k == 0 = True
            | k == 1 || k == -1 = False
            | otherwise = (multiplo3 (k-3))

sumaImpares :: Int -> Int
{-Funcion que dado un k Entero mayor igual a 0 devuelve las suma de los k impares-}
sumaImpares n | n == 1 = 1
              | otherwise = (2*n-1)+sumaImpares(n-1)

mediofactorial :: Int -> Int
{-Funcion que dado un numero entero positivo devuelve su factorial cada 2 numeros-}
mediofactorial n | n <= 0 = 1
                 | otherwise = n * mediofactorial (n-2)

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = digitoUnidades (div (x - digitoUnidades x) 10)

sumaDigitos :: Int -> Int
{-Funcion que dado un k Entero mayor igual a 0 devuelve las suma de sus digitos-}
sumaDigitos n | n == 0 = 0
              | otherwise = (sumaDigitos (div (n-digitoUnidades n) 10)) + digitoUnidades n

digitosIguales:: Int -> Bool
{-Funcion que dado un k Entero mayor igual a 0 devuelve si es o no con todos los digitos iguales-}
digitosIguales n | (n <= 9 && n > 0 ) = True
                 | digitoUnidades n == digitoDecenas n = digitosIguales (div (n-digitoUnidades n) 10)
                 | otherwise = False