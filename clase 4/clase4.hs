sumatoria_i:: Int -> Int

sumatoria_i n 
    | n == 1 = 1
    | otherwise = sumatoria_i(n-1) + n

sumatoria_inv:: Float -> Float
sumatoria_inv n 
    | n == 1 = 1
    | otherwise = sumatoria_inv(n-1) + 1/n

f1 :: Int -> Int
f1 0 = 1
f1 n = (f1 (n-1)) + 2^n

f2 :: Int -> Float -> Float
f2 1 q = q**1
f2 n q = (f2 (n-1) q) + q^n

f3 :: Int -> Float -> Float
f3 n q = f2 (2*n) q


f4 :: Int -> Float -> Float
f4 n q = f3 n q - f2 n q

factorial :: Int -> Int
{-Funcion que dado un numero entero positivo devuelve su factorial-}
factorial 0 = 1
factorial n = n * (factorial (n -1))

eAprox :: Int  -> Float
eAprox 0 = 1
eAprox n = (eAprox (n-1)) + (1 / (fromIntegral (factorial n)))

e :: Float
e = eAprox 10

f :: Int -> Int -> Float
f n m = f2 n (f2 m (fromIntegral n))

sumaPotencia:: Int -> Int -> Float -> Float
sumaPotencia n m q = (f2 n q)*(f2 m q)

sumaRacionales:: Int->Float->Float
sumaRacionales n m = (fromIntegral (sumatoria_i n))*(sumatoria_inv m)