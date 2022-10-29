-- Gilardon Bautista 
-- Munhó Vital Facundo Nicolas
-- Yañez Carolina 

-- auxiliares
signo :: Float -> Float
signo a | a >= 0 = 1
        | otherwise = -1

productoescalar:: Float -> Complejo -> Complejo
productoescalar k (a,b) = (k*a,k*b)


type Complejo = (Float, Float)
-- defino a los complejos z pertencientes a C z=a+ib como 
-- el par z perteneciente a R2 z=(a,b)
--

re :: Complejo -> Float
re (a,_) = a

im :: Complejo -> Float
im (_,b) = b

suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c,b+d)

producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c-b*d,a*d+b*c)

conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,-b)

inverso :: Complejo -> Complejo
inverso (a,b) = (re (conjugado (a,b)) /(modulo (a,b)**2) , im (conjugado (a,b)) /(modulo (a,b)**2))

cociente :: Complejo -> Complejo -> Complejo
cociente (a,b) (c,d) = producto (a,b) (inverso (c,d))

potencia :: Complejo -> Int -> Complejo
potencia (a,b) k = pasarACartesianas (modulo (a,b) ^ k) ( fromIntegral k * argumento (a,b))

raicesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
raicesCuadratica a b c | w >= 0 = ( ((-b)/(2*a)+ sqrt w,0) , ((-b)/(2*a)- sqrt(w),0) )
                       | otherwise =  ( ((-b)/(2*a), sqrt(sqrt (w**2))/(2*a)) , ((-b)/(2*a),-sqrt(sqrt (w**2))/(2*a)) )
                        where  w = b**2 - 4 * a * c

modulo :: Complejo -> Float
modulo (a,b) = sqrt (a**2+b**2)

distancia:: Complejo -> Complejo -> Float
distancia (a,b) (c,d) = modulo (suma (a,b) (-c,-d))

argumento :: Complejo -> Float
argumento (a,b) | a > 0 = atan (b/a)+2*pi
                | b >= 0 && a < 0 = pi + atan (b/a)+2*pi
                | b < 0 && a < 0 = -pi + atan (b/a)+2*pi
                | a == 0 = pi/2*signo b +2*pi
                | otherwise = 0

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas m arg = (m * cos arg ,m* sin arg)

raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada z = (pasarACartesianas (sqrt (modulo z)) (argumento z/2),pasarACartesianas (sqrt (modulo z)) (argumento z/2 + pi))

raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (cociente (suma (productoescalar (-1) b) w1) (productoescalar 2 a),cociente (suma (productoescalar (-1) b) w2) (productoescalar 2 a))
                                where  (w1,w2) = raizCuadrada (suma (potencia b 2)  (productoescalar (-4) (producto a c)))

