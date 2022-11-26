-- Bossi Tomás tomasbossi97@gmail.com
-- González Gastón gasti.g.gg@gmail.com
-- Muñoz Sandra sm9654136@gmail.com

type Complejo = (Float,Float)

-- Auxiliares
prodPorEscalar :: Float -> Complejo -> Complejo
prodPorEscalar k (a, b) = (k*a, k*b)

-- 1.1
re :: Complejo -> Float
re (a, _) = a

-- 1.2
im :: Complejo -> Float
im (_, b) = b

-- 1.3
suma :: Complejo -> Complejo -> Complejo
suma (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

-- 1.4
producto :: Complejo -> Complejo -> Complejo
producto (a1, b1) (a2, b2) = (a1*a2 - b1*b2, a1*b2 + a2*b1)

-- 1.5
conjugado :: Complejo -> Complejo
conjugado (a, b) = (a, -b)

-- 1.6
inverso :: Complejo -> Complejo
inverso (a, b) = (a/m, -b/m)
               where m = a**2 + b**2

-- 1.7
cociente :: Complejo -> Complejo -> Complejo
cociente z1 z2 = producto z1 (inverso z2)

-- 1.8
potencia :: Complejo -> Integer -> Complejo
potencia z 0 = (1, 0)
potencia z k = producto z (potencia z (k - 1))

-- 1.9
raicesCuadratica :: Float -> Float -> Float -> (Complejo,Complejo)
raicesCuadratica a b c
  | disc < 0 = (w, conjugado w)
  | otherwise = (((re w + im w), 0), ((re w - im w), 0))
  where
    disc = b**2 - 4*a*c
    w = prodPorEscalar (1/(2*a)) (-b, (sqrt (abs disc)))

-- 2.1
modulo :: Complejo -> Float
modulo (a, b) = sqrt (a**2 + b**2)

-- 2.2
distancia :: Complejo -> Complejo -> Float
distancia (a1, b1) (a2, b2) = modulo (a1 - a2, b1 - b2)

-- 2.3
argumento :: Complejo -> Float
argumento (a, b)
  | a < 0 = pi + t
  | b < 0 = 2*pi + t
  | otherwise = t
  where t = atan (b/a)

-- 2.4
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r t = (r*cos t, r*sin t)

-- 2.5
raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada (0, 0) = ((0, 0), (0, 0))
raizCuadrada z = (w, prodPorEscalar (-1) w)
  where 
    t = argumento z
    w = prodPorEscalar (sqrt (modulo z)) (cos (t/2), sin (t/2))

-- 2.6
raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (producto (suma (prodPorEscalar (-1) b) (fst w)) (inverso (prodPorEscalar 2 a)), producto (suma (prodPorEscalar (-1) b) (snd w)) (inverso (prodPorEscalar 2 a)))
  where
    w = raizCuadrada (suma (potencia b 2) (prodPorEscalar (-4) (producto a c)))

-- 3.1
raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = gn n 0
  where
    gn :: Integer -> Integer -> [Complejo]
    gn n k
      | k >= n = []
      | otherwise = (cos t, sin t) : gn n (k + 1)
      where
        t = (2*(fromIntegral k)*pi/(fromIntegral n))

-- 3.2
sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas _ [] _ = True
sonRaicesNEsimas n (z:zs) tol = modulo (suma (potencia z n) (-1, 0)) < tol && sonRaicesNEsimas n zs tol
