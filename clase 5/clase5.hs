{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


{-dado dos numeros x y enteros devuelve y si y divide a x o 0 si y no divide a x-}
daDivisor :: Int -> Int -> Int
daDivisor x y | mod x y == 0 = y
              | otherwise = 0

{-dado dos numeros enteros suma los divisores de x hasta el numero y-}
sumaDivisoreshasta :: Int -> Int -> Int
sumaDivisoreshasta x y |  y == 1 = 1
                       | otherwise = sumaDivisoreshasta x (y-1) + daDivisor x y

{-dado un numero x entero, suma todos sus divisores-}
sumaDivisores :: Int -> Int
sumaDivisores x = sumaDivisoreshasta x x

{-dado tres numeros naturales x y z, decide cual es el
menor divisor de x entre y o z y lo devuelve, si ni y o z 
son divisores de x, devuelve 1-}
cualeselmenorDivisor :: Int -> Int -> Int -> Int
cualeselmenorDivisor x y z | daDivisor x y == 0 && daDivisor x z /= 0 = z
                           | daDivisor x z == 0 && daDivisor x y /= 0 = y
                           | daDivisor x y <= daDivisor x z = y
                           | daDivisor x y > daDivisor x z = z
                           | otherwise = 1

{-dado x e y da el menor divisor de x a partir de un nuemero y-}
{-funcionamientos: verifico si y es divisor de x, si y efectivamente lo es, devuelvo ese valor
en caso de que y no sea divisor, pregunto si y+1 lo es, si lo es devuelvo y si no repito con y+2-}
menorDivisordesde :: Integral t => t -> t -> t
menorDivisordesde x y | mod x y == 0 = y
                      | otherwise = menorDivisordesde x (y+1) 

menorDivisor :: Integral t => t -> t
{-dado x natural, devuelve su menor divisor-}
menorDivisor x = menorDivisordesde x 2

esPrimo :: Integral a => a -> Bool
{-dado un numero x decide si es primo o no-}
esPrimo x | menorDivisor x == x = True
          | otherwise= False

{-dado un numero x da su siguiente primo-}
siguienteprimo :: Integral t => t -> t
siguienteprimo x | esPrimo x = x
                 | otherwise = siguienteprimo (x+1)

{-dado n natural y x un numero natural,
devuelve el n esimo siguiente primo de x-}

nesimoprimodesde :: (Integral t1, Num t2, Eq t2) => t2 -> t1 -> t1
nesimoprimodesde n x | n == 0 = x
                     | otherwise = nesimoprimodesde (n-1) (siguienteprimo (x+1))


{-dado n un numero natural devuelve el n esimo numero primo-}
nesimoprimo :: (Integral t1, Num t2, Eq t2) => t2 -> t1
nesimoprimo n = nesimoprimodesde n 1 

factorial :: Integral t => t -> t
{-Funcion que dado un numero entero positivo devuelve su factorial-}
factorial 0 = 1
factorial n = n * (factorial (n -1))

menorfactdesdeaux :: Integral t => t -> t -> t
menorfactdesdeaux x y | x <= factorial y = factorial y
                      | otherwise = menorfactdesdeaux x (y+1) 

menorFactdesde :: Integral t => t -> t
menorFactdesde x = menorfactdesdeaux x 0

mayorfacthastaaux :: Integral t => t -> t -> t
mayorfacthastaaux x y | factorial y <= x = factorial y
                      | otherwise = mayorfacthastaaux x (y-1) 

mayorFacthasta :: Integral t => t -> t
mayorFacthasta x = mayorfacthastaaux x x          

esFact :: Int -> Bool
esFact x = menorFactdesde x == mayorFacthasta x

fib :: Int -> Int
{-Funcion que dado un n Entero mayor igual al 0 devuelve el n-esimo termino de fibonacci-}
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

menorfibdesdeaux :: Int -> Int -> Int
menorfibdesdeaux x y | x <= fib y = fib y
                     | otherwise = menorfibdesdeaux x (y+1) 

menorfibdesde :: Int -> Int
menorfibdesde x = menorfibdesdeaux x 0

mayorfibhastaaux :: Int -> Int -> Int
mayorfibhastaaux x y | fib y <= x = fib y
                      | otherwise = mayorfibhastaaux x (y-1) 

mayorFibthasta :: Int -> Int
mayorFibthasta x = mayorfibhastaaux x x     

esFib :: Int -> Bool
esFib x = mayorFibthasta x == menorfibdesde x


tomovalormaxaux :: Int -> Int -> Int -> Int
tomovalormaxaux n1 n2 m | n1-1 == n2 = m
                        | n1 <= n2 && sumaDivisores n1 > m = tomovalormaxaux n1 n2 (sumaDivisores (n1)) 
                        | otherwise = tomovalormaxaux (n1+1) n2 m

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = tomovalormaxaux n1 n2 (sumaDivisores n1)

tomovalorminaux :: Int -> Int -> Int -> Int
tomovalorminaux n1 n2 m | n1-1 == n2 = m
                        | n1 <= n2 && sumaDivisores n1 < m = tomovalorminaux n1 n2 (sumaDivisores (n1)) 
                        | otherwise = tomovalorminaux (n1+1) n2 m

tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 = tomovalorminaux n1 n2 (sumaDivisores n1)