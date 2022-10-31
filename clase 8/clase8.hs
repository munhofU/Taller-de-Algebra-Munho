type Set a = [a]

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

agregarTodosATodos :: Set Int -> Set [Int]-> Set [Int] 
agregarTodosATodos [] l = []
agregarTodosATodos (x:xs) xss = agregarATodos x xss ++ agregarTodosATodos xs xss

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss == True = xss
                | otherwise = xs:xss

combinatorio::Int->Int-> Int
{-funcion que dado dos numeros enteros n,m 
devuelve las combinaciones de [0...n] con m elementos-}
combinatorio n 0 = 1
combinatorio n m | n==m = 1
                 | n<m = 0
                 | otherwise = combinatorio (n-1) m + combinatorio (n-1) (m-1)

agregarATodas :: Int -> Set [Int]-> Set [Int]
agregarATodas _ [] = []
agregarATodas n (xs:xss) = (n:xs) : agregarATodas n xss 

agregarTodosATodas :: Set Int -> Set [Int]-> Set [Int] 
agregarTodosATodas [] l = []
agregarTodosATodas (x:xs) xss = agregarATodas x xss ++ agregarTodosATodas xs xss

separarconjunto:: Set Int->Set [Int]
separarconjunto [] = []
separarconjunto (x:xs) = [x]:separarconjunto xs

variaciones:: Set Int -> Int -> Set [Int]
variaciones [] _ = []
variaciones xs 1 = separarconjunto xs
variaciones xs n = agregarTodosATodas xs (variaciones xs (n-1))

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn [] a _ = a:[]
insertarEn xs a 1 = a:xs
insertarEn (x:xs) a p = x:insertarEn xs a (p-1)

insertarEnTodas :: Set [Int] -> Int -> Int -> Set [Int]
insertarEnTodas [] a _ = []
insertarEnTodas (xs:xss) a p = insertarEn xs a p : insertarEnTodas xss a p


insertarEnTodoslados :: Int -> Set [Int] -> Set[Int]
insertarEnTodoslados  _  [] = []
insertarEnTodoslados n (xs:xss) = insertarEn xs n ((length xs)) : insertarEnTodoslados n (xss)


partes :: Int -> Set (Set Int)
partes 0 = [[]]
partes n = partes(n-1) ++ (agregarATodos n (partes (n-1)))

union:: Set [Int] -> Set [Int] -> Set [Int]
union [] yss = yss
union (xs:xss) yss| perteneceC xs yss = union xss yss
                  | otherwise = union xss (xs:yss)

permutaciones :: Int -> Set [Int]
permutaciones 0 = [[]]
permutaciones n = insertarEnTodoslados n (permutaciones (n-1))