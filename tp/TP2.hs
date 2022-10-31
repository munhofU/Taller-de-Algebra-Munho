-- Gilardon Bautista 
-- Munhó Vital Facundo Nicolas
-- Yañez Carolina 


type Complejo = (Float, Float)
-- defino a los complejos z pertencientes a C z=a+ib como 
-- el par z perteneciente a R2 z=(a,b)

-- auxiliares
-- defino la funcion signo 
signo :: Float -> Float
signo a | a >= 0 = 1
        | otherwise = -1

-- defino el producto escalar para complejos
productoescalar:: Float -> Complejo -> Complejo
productoescalar k (a,b) = (k*a,k*b)

-- busco las N-esimas raices has un cierto k
raicesNEsimashasta :: Int -> Int -> [Complejo]
raicesNEsimashasta n 0 = [(cos (2*pi*0/fromIntegral n), sin (2*pi*0/fromIntegral n))]
raicesNEsimashasta n k = (cos (2*pi* (fromIntegral k/fromIntegral n)), sin (2*pi*(fromIntegral k/fromIntegral n))):raicesNEsimashasta n (k-1)

-- calculo todas las distancias entre todos los complejos de los listas
distancias:: [Complejo] -> [Complejo] -> [Float]
distancias [] [] = []
distancias [] ys = 0 : distancias [] ys
distancias zs [] = 0 : distancias zs []
distancias (z:zs) (y:ys) = distancia z y : distancias zs ys

-- calculo la de un z a todos los complejos de una lista
distanciaatodos:: Complejo -> [Complejo] -> [Float]
distanciaatodos _ [] = []
distanciaatodos y (z:zs) = distancia y z : distanciaatodos y zs

-- verifico que todos los numeros reales en una lista sean menores a un cierto valor real
todasmenores:: [Float] -> Float-> Bool
todasmenores [] _ = True
todasmenores (z:zs) k | z > k = False
                      | otherwise = todasmenores zs k

-- verifico si alguno de los elementos de una lista, de reales, es menor a cierto valor k
algunamenor:: [Float] -> Float-> Bool
algunamenor [] _ = False
algunamenor (z:zs) k | z < k = True
                     | otherwise = algunamenor zs k

--------------------------------------------------------------------------------------------------------------------------------
-- 1
-------------------------------------------------------------------------------------------------------------------------------
-- aplico la definicion de parte real
re :: Complejo -> Float
re (a,_) = a

-- aplico la definicion de parte imaginaria
im :: Complejo -> Float
im (_,b) = b

-- aplico la definicion de parte suma en complejos
suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c,b+d)

-- aplico la definicion de producto en complejos para forma binomica
producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c-b*d,a*d+b*c)

-- aplico la definicion de conjugado de un numero complejo z*=(a,-b)
conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,-b)

-- aplico la definicion de inverso apoyandome en la definicion de modulo de z
-- |z|^2= z.z* => z^-1 = -z*/|z|
inverso :: Complejo -> Complejo
inverso (a,b) = (re (conjugado (a,b)) /(modulo (a,b)**2) , im (conjugado (a,b)) /(modulo (a,b)**2))

-- aplico que el conciente de dos numero z1,z2 es multiplicar z1 por el inverso de z2
cociente :: Complejo -> Complejo -> Complejo
cociente (a,b) (c,d) = producto (a,b) (inverso (c,d))

-- aplico que si z=|z|e^(io), con o arg de z => z^n=|z|^ne^(ino)
potencia :: Complejo -> Int -> Complejo
potencia (a,b) k = pasarACartesianas (modulo (a,b) ^ k) ( fromIntegral k * argumento (a,b))

-- aplico el algoritmo de raices de un polinomio, considerando la posibilidad de
-- raices complejas
raicesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
raicesCuadratica a b c | w >= 0 = ( ((-b)/(2*a)+ sqrt w,0) , ((-b)/(2*a)- sqrt w,0) )
                       | otherwise =  ( ((-b)/(2*a), sqrt(sqrt (w**2))/(2*a)) , ((-b)/(2*a),-sqrt(sqrt (w**2))/(2*a)) )
                        where  w = b**2 - 4 * a * c

--------------------------------------------------------------------------------------------------------------------------------
-- 2
-------------------------------------------------------------------------------------------------------------------------------

-- aplico la defincion de modulo, el re es un formalismo para que producto sea un flotante
modulo :: Complejo -> Float
modulo z = re(producto z (conjugado z))

-- aplico defincion de distancia
distancia:: Complejo -> Complejo -> Float
distancia (a,b) (c,d) = modulo (suma (a,b) (-c,-d))

-- aplico una defincion de argumento
argumento :: Complejo -> Float
argumento (a,b) | a > 0 = atan (b/a)+2*pi
                | b >= 0 && a < 0 = pi + atan (b/a)+2*pi
                | b < 0 && a < 0 = -pi + atan (b/a)+2*pi
                | a == 0 = pi/2*signo b +2*pi
                | otherwise = 0

-- aplico defincion de la transformacion de sistemas de coord
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas m arg = (m * cos arg ,m* sin arg)

-- resuelvo el problema en forma de potencia |w|^2=|z| y arg(w) = (arg(z)+2kpi)/2
raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada z = (pasarACartesianas (sqrt (modulo z)) (argumento z/2),pasarACartesianas (sqrt (modulo z)) (argumento z/2 + pi))

-- resuelvo el algoritmo considerando a,b,c complejos
raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (cociente (suma (productoescalar (-1) b) w1) (productoescalar 2 a),cociente (suma (productoescalar (-1) b) w2) (productoescalar 2 a))
                                where  (w1,w2) = raizCuadrada (suma (potencia b 2)  (productoescalar (-4) (producto a c)))

--------------------------------------------------------------------------------------------------------------------------------
-- 3
-------------------------------------------------------------------------------------------------------------------------------

-- aplico la funcion auxialiar hasta n
raicesNesimas :: Int -> [Complejo]
raicesNesimas n = raicesNEsimashasta n (n-1)

-- verifico si en todas las distancias de los z y las n-enesimas raices
-- al menos uno es menor que epsilon
sonRaicesNesimas :: Int -> [Complejo] -> Float -> Bool
sonRaicesNesimas _ [] _ = True
sonRaicesNesimas n (z:zs) eps = algunamenor (distanciaatodos z ys) eps && sonRaicesNesimas n zs eps
                              where ys= raicesNesimas n 

                        