-- 
-- 
-- 

-- TP 2

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

neg :: Complejo -> Complejo
neg z = producto z (-1, 0) -- -z

resta :: Complejo -> Complejo -> Complejo
resta z w = suma z (neg w) -- z - w

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
raicesCuadratica :: Float -> Float -> Float -> (Complejo,Complejo)
raicesCuadratica a b c | d >= 0 = ((r + sd/(2*a), 0), (r - sd/(2*a), 0)) -- raíces reales
                       | otherwise = ((r, sd/(2*a)), (r, -sd/(2*a)))     -- races complejas
    where d = b^2 - 4*a*c -- discriminante
          sd = sqrt (abs d) -- raíz del discriminante
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
raizCuadrada z = (w, neg w)
    where w = (r, i)
          r = sqrt (abs ((re z + modulo z)/2))
          i = s*sqrt (abs ((modulo z - re z)/2))
          s | im z == 0 = 1
            | otherwise = im z/abs (im z) -- signo

-- 2.6
raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (w1, w2)
    where w1 = cociente (suma  (neg b) sd) (producto (2, 0) a)
          w2 = cociente (resta (neg b) sd) (producto (2, 0) a)
          sd = fst (raizCuadrada (resta (potencia b 2) ac4)) -- raíz del "discriminante" w
          ac4 = producto (4, 0) (producto a c) -- 4ac

-- 3.1
reverso :: [Complejo] -> [Complejo]
reverso xs = reversoAux xs []
    where reversoAux :: [Complejo] -> [Complejo] -> [Complejo]
          reversoAux [] sxs = sxs
          reversoAux (x:xs) sxs = reversoAux xs (x:sxs)

raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = reverso (raicesNEsimasDesde n n)
    where raicesNEsimasDesde :: Integer -> Integer -> [Complejo]
          raicesNEsimasDesde n 1 = [(1,0)] -- [w0]
          raicesNEsimasDesde n m = producto w1 (head wp):wp
              where w1 = pasarACartesianas 1 (2*pi/(fromIntegral n))
                    wp = raicesNEsimasDesde n (m-1)

-- 3.2
sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] tol = True
sonRaicesNEsimas n (w:ws) tol = error < tol && sonRaicesNEsimas n ws tol
    where error = modulo (resta (potencia w n) (1,0))
