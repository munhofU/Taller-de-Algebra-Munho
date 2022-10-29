{-comando tipo de operacion :t (operacion)-}


f1:: (Floating a, Ord a) => a -> a -> a -> Bool
f1 x y z = x**y + z <= z+y**z

f2:: (Floating a) => a -> a -> a
f2 x y = ( sqrt x ) / ( sqrt y )

f3:: (Integral a, Floating a) => a -> a -> a
f3 x y = div ( sqrt x ) ( sqrt y )

f4::(Floating a, Ord a) => a -> a -> a -> a
f4 x y z | x == y = z
         | x**y == y = x
         | otherwise = y

f5::(Floating a, Ord a) => a -> a -> b -> b
f5 x y z | x == y = z
         | x**y == y = z
         | otherwise = z

estanrealacionados :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> Bool
estanrealacionados x y = (x<=3 && y<=3) || ((3<x && x<=7) && (3<y && y<=7)) || (x>7 && y>7)

prodInt:: Num a => (a, a) -> (a, a) -> a
prodInt (x1,x2) (y1,y2) = x1 * y1 + x2 * y2

todoMenor:: Ord a => (a, a) -> (a, a) -> Bool
todoMenor (x1,x2) (y1,y2) = (x1<y1) && (x2<y2)

distanciaPuntos:: (Floating a) => (a, a) -> (a, a) -> a
distanciaPuntos (x1,x2) (y1,y2) = sqrt ((x1-y1)**2 + (x2-y2)**2)

sumaTerna:: Num a =>  (a, a, a) -> (a, a, a) -> (a, a, a) -> (a, a, a)
sumaTerna (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) = (x1+x2+x3,y1+y2+y3,z1+z2+z3)

posicPrimerPar :: (Integral a1, Integral a2, Integral a3, Num p) => (a1, a2, a3) -> p
posicPrimerPar (x1, x2, x3) | (mod x1 2 == 0) = 1
                            | (mod x2 2 == 0) = 2 
                            | (mod x3 2 == 0) = 3
                            | otherwise = 4

crearPar:: a -> b -> (a,b)
crearPar a b = (a,b)

invertir :: (b, a) -> (a, b)
invertir ( a , b ) = ( b , a )