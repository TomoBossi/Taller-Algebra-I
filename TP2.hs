-- Bossi Tomás tomasbossi97@gmail.com
-- Briccola Francina franbriccola2309@gmail.com
-- Campos Ezequiel zecu95@gmail.com


type Complejo = (Float,Float)


-- 1.1
re :: Complejo -> Float
re z = fst z

-- 1.2
im :: Complejo -> Float
im z = snd z

-- 1.3
suma :: Complejo -> Complejo -> Complejo
suma z w = (re z + re w, im z + im w)

-- 1.4
producto :: Complejo -> Complejo -> Complejo
producto z w = (re z * re w - im z * im w, re z * im w + im z * re w)

-- 1.5
conjugado :: Complejo -> Complejo
conjugado z = (re z, -im z)

-- 1.6
inverso :: Complejo -> Complejo
inverso z = (re conj/mod2, im conj/mod2)
            where conj = conjugado z
                  mod2 = (modulo z)^2

-- 1.7
cociente :: Complejo -> Complejo -> Complejo
cociente z w = producto z (inverso w)

-- 1.8
potencia :: Complejo -> Integer -> Complejo
potencia z 1 = z
potencia z k = producto z (potencia z (k-1))

-- 1.9
raicesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
raicesCuadratica a b c = ((r, d/(2*a)), (r, -d/(2*a)))
                         where d = sqrt (abs (b^2 - 4*a*c))
                               r = -b/(2*a)

-- 2.1
modulo :: Complejo -> Float
modulo z = sqrt ((re z)^2 + (im z)^2)

-- 2.2
distancia :: Complejo -> Complejo -> Float
distancia z w = sqrt ((re z - re w)^2 + (im z - im w)^2)

-- 2.3
argumento :: Complejo -> Float
argumento z | a > 0 && b > 0 = atan (b/a)
            | a > 0 && b < 0 = atan (b/a) + 2*pi
            | otherwise = atan (b/a) + pi
              where a = re z
                    b = im z

-- 2.4
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r a = (r*cos a, r*sin a)

-- 2.5
raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada z = (w, (-re w, -im w))
               where w = (x, y)
                     x = sqrt (abs ((re z + modulo z)/2))
                     y = s*sqrt (abs ((modulo z - re z)/2))
                     s = im z/abs (im z) -- signo

-- 2.6
-- raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
-- raicesCuadraticaCompleja a b c = 

-- 3.1
-- raicesNEsimas :: Integer -> [Complejo]
-- raicesNEsimas n = -- Probablemente requiera aux raicesNEsimasDesde n m con caso base m == 0 para siempre poder sumar ángulo dividido por n

-- 3.2
-- sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> [Bool]
-- sonRaicesNEsimas n rs eps = 