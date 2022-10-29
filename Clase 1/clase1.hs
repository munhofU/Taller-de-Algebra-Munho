{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

f :: Int -> Int -> Int
f x y = x * x + y * y

g :: Float -> Float -> Float -> Float
g x y z = x + y + z * z

doble :: Float -> Float
doble x = 2*x

suma :: Float -> Float -> Float
suma x y = x + y

normavectorial :: Float -> Float -> Float
normavectorial x y = sqrt( x ** 2 + y ** 2 )

funcioncst :: Int -> Int
funcioncst x = 8

{-fn n | n == 0 = 1
     | n /= 0 = 0-}

{- 
fn n | n == 0 = 1
     | otherwise = 0
     -}

fn :: Int -> Int
fn 0 = 1
fn n = 0

signo :: Int -> Int
signo n | n > 0 = 1
     | n ==0 = 0
     | n < 0 = -1

maximo :: Int -> Int -> Int
maximo x y
        | x >= y = x
        | otherwise = y

f1 :: Int -> Int
f1 n | n >= 3 = 5

f2 :: Int -> Int
f2 n | n >= 3 = 5
     | n <= 1 = 8

f3 :: Int -> Int
f3 n | n >= 3 = 5
     | n == 2 =  undefined
     | otherwise = 8

f4 :: Int -> Int
f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 :: Int -> Int
f5 n | n <= 9 = 7
     | n >= 3 = 5

cantidadDeSoluciones :: Int -> Int -> Int
cantidadDeSoluciones b c | b ^2 - 4* c > 0 = 2
                         | b ^2 - 4* c == 0 = 1
                         | otherwise = 0

cantidadDeSoluciones2 :: Int -> Int -> Int
cantidadDeSoluciones2 b c | d > 0 = 2
                          | d == 0 = 1
                          | otherwise = 0
                          where d= b^2 - 4*c

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y z = ( x >= y ) || z

absoluto :: Int -> Int
absoluto x | x <= 0 = -x
           | x >  0 =  x

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y = maximo ( absoluto x ) ( absoluto y )

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y = maximo ( maximo x y)

{- algunoEs0 x y | x == 0 = True
              | y == 0 = True
              | otherwise = False 
              -}

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x 0 = True
algunoEs0 0 y = True
algunoEs0 x y = False

{- ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False -}

ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 x y = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y |  x `mod` y == 0 = True
                 | otherwise = False

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = digitoUnidades (div (x - digitoUnidades x) 10)