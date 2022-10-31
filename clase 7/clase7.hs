type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar n c | elem n c = c
            | otherwise = n:c

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True 
incluido (x:xs) ys = elem x ys && incluido xs ys


iguales :: Set Int -> Set Int -> Bool
iguales xs ys = incluido xs ys && incluido ys xs

perteneceC ::Set Int -> Set (Set Int)-> Bool
perteneceC _ [] = False
perteneceC xs (ys:yss) | iguales xs ys = True
                       | otherwise = perteneceC xs yss

agregarATodos :: Int -> Set (Set Int)-> Set (Set Int)
agregarATodos n [] = []
agregarATodos n (xs:xss) = agregar n xs : agregarATodos n xss 

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss == True = xss
                | otherwise = xs:xss


partes :: Int -> Set (Set Int)
partes 0 = [[]]
partes n = partes(n-1) ++ (agregarATodos n (partes (n-1)))


productoCartesianoUno :: Int -> Set Int -> Set (Int, Int)
productoCartesianoUno n [] = []
productoCartesianoUno n (x:xs) = (n,x):productoCartesianoUno n (xs) 

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] ys = []
productoCartesiano (x:xs) ys = (productoCartesianoUno x ys) ++ (productoCartesiano xs ys)