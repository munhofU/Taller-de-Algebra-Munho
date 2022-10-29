{- listas = [1,2,3,...] <- las listas se declaran de esta forma,
                           y solo pueden tener los mismos elementos 
si los elemntos son del tipo A, la lista de elementos a es de tipo [A]-}

recorrer :: [Int] -> Int -> Int
recorrer l n | n == 0 = head l
             | otherwise = recorrer (tail l) (n-1)


sumatoriasobrea :: [Int] -> Int -> Int
sumatoriasobrea l a | l==[] = a
                    | otherwise = sumatoriasobrea (tail l) ( a + head l)


longitudsobren :: [Int] -> Int -> Int
longitudsobren l n | l == [] = n
                   | otherwise = longitudsobren (tail l) (n+1)

invertirlist :: [Int] -> Int -> [Int] -> [Int]
invertirlist l n lr | n == longitudsobren l 0 = lr
                    | otherwise = invertirlist l (n+1) ((recorrer l n) : [])

pertence :: [Int] -> Int -> Bool
pertence [] n = False
pertence l n | head l == n = True
             | otherwise = pertence (tail l) n