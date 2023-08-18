f1 :: Int -> Int
f1 n 
  | n == 0 = 2^n 
  | otherwise = 2^n + f1 (n-1)

f2 :: Int -> Float -> Float
f2 n q 
  | n == 1 = q
  | otherwise = q^n + f2 (n-1) q

f3 :: Int -> Float -> Float
f3 n q 
  | n == 1 = q + q^2
  | otherwise = q^(2*n) + q^((2*n)-1) + f3 (n-1) q

f4 :: Int -> Float -> Float
f4 n q 
  | n == 1 = f3 n q
  | otherwise = f3 n q - f2 (n-1) q

fact :: Int -> Int
fact n 
  | n == 1 = 1
  | otherwise = n * fact (n-1)

eAprox :: Int -> Float
eAprox n 
  | n == 0 = 1.0
  | otherwise = 1/(fromIntegral (fact n)) + eAprox (n-1)

e :: Float
e = eAprox 9 -- Primeros 10 términos

-- 1
f :: Int -> Int -> Float
f n m 
  | n == 1 = f2 m (fromIntegral n)
  | otherwise = f2 m (fromIntegral n) + f (n-1) m

-- 2
sumaPotAux :: Float -> Int -> Int -> Float
sumaPotAux q n m 
  | m == 1 = q^(n+m)
  | otherwise = q^(n+m) + sumaPotAux q n (m-1)

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n m 
  | n == 1 = sumaPotAux q n m
  | otherwise = sumaPotAux q n m + sumaPotencias q (n-1) m

-- 3
sumaRacAux :: Int -> Int -> Float
sumaRacAux n m 
  | m == 1 = fromIntegral n
  | otherwise = fromIntegral (n)/fromIntegral (m) + sumaRacAux n (m-1)

sumaRacionales :: Int -> Int -> Float
sumaRacionales n m 
  | n == 1 = sumaRacAux n m
  | otherwise = sumaRacAux n m + sumaRacionales (n-1) m

-- 4
g1 :: Int -> Int -> Int
g1 i n 
  | n == i = i^n
  | otherwise = i^n + g1 i (n-1)

-- 5
g2Aux1 :: Int -> Int -> Int
g2Aux1 q n 
  | n == 1 = q^n
  | otherwise = q^n + g2Aux1 q (n-1)

g2Aux2 :: Int -> Int -> Int
g2Aux2 n m 
  | n == 1 = g2Aux1 n m
  | otherwise = g2Aux1 n m + g2Aux2 (n-1) m

g2 :: Int -> Int
g2 n = g2Aux2 n n

-- 6
esPar :: Int -> Bool
esPar n 
  | n == 0 = True
  | otherwise = not (esPar (n-1))

g3 :: Int -> Int
g3 n 
  | n == 1 = 0
  | esPar n = 2^n + g3 (n-1)
  | otherwise = g3 (n-1)

-- 7
digitosIguales :: Int -> Bool
digitosIguales z 
  | az == unidad = True
  | otherwise = unidad == decena && digitosIguales div10
  where 
    az = abs z
    div10 = div az 10
    unidad = mod az 10
    decena = mod div10 10

g4 :: Int -> Int
g4 n 
  | n == 1 = 1
  | digitosIguales n = n + g4 (n-1)
  | otherwise = g4 (n-1)
