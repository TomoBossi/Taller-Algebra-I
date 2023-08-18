esPar :: Int -> Bool
esPar n 
  | n == 0 = True
  | n == 1 = False
  | otherwise = esPar (n-2)

esPar2 :: Int -> Bool
esPar2 n 
  | n == 0 = True
  | otherwise = not (esPar2 (n-1))

fib :: Int -> Int
fib n 
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

parteEntera :: Float -> Int
parteEntera n 
  | n < 1 = 0
  | otherwise = 1 + parteEntera (n-1)

esMult :: Int -> Int -> Bool
esMult n m 
  | n == 0 = True
  | n < 0 = False
  | otherwise = esMult (n-m)  m

sumaImpares :: Int -> Int
sumaImpares n 
  | n == 1 = 1
  | otherwise = 2*n-1 + sumaImpares (n-1)

medioFact :: Int -> Int
medioFact n 
  | n < 1 = 1
  | otherwise = n * medioFact (n-2)

-- Asumo z en Z>=0
sumaDigitos :: Int -> Int
sumaDigitos z 
  | z == 0 = 0
  | otherwise = mod z 10 + sumaDigitos (div z 10)

-- Asumo z en Z
digitosIguales :: Int -> Bool
digitosIguales z 
  | az == unidad = True
  | otherwise = unidad == decena && digitosIguales div10
  where 
    az = abs z
    div10 = div az 10
    unidad = mod az 10
    decena = mod div10 10
