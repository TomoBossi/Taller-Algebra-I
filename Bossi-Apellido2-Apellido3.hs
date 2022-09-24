-- Bossi Tomás
-- Apellido Nombre #2
-- Apellido Nombre #3



-- EJERCICIO 1. sonCoprimos
minimo :: Integer -> Integer -> Integer
minimo n1 n2 | n1 < n2 = n1
             | otherwise = n2

sonCoprimosHasta :: Integer -> Integer -> Integer -> Bool
sonCoprimosHasta n1 n2 m | m == 1 = True
                         | mod n1 m == 0 && mod n2 m == 0 = False
                         | otherwise = sonCoprimosHasta n1 n2 (m-1)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n1 n2 = sonCoprimosHasta n1 n2 (minimo n1 n2)

-- EJERCICO 2: es2Pseudoprimo
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n m | n == m = n
                      | mod n m == 0 = m
                      | otherwise = menorDivisorDesde n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n = n == menorDivisor n

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = not (esPrimo n) && mod (2^(n-1)-1) n == 0

-- EJERCICIO 3: cantidad3Pseudoprimos
esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo n a = not (esPrimo n) && mod (a^(n-1)-1) n == 0

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m | m == 1 = 0
                        | esAPseudoprimo m 3 = 1 + cantidad3Pseudoprimos (m-1)
                        | otherwise = cantidad3Pseudoprimos (m-1)

-- EJERCICIO 4: kesimo2y3Pseudoprimo
kesimo2y3PseudoprimoAux :: Integer -> Integer -> Integer -> Integer
kesimo2y3PseudoprimoAux k c v | k == c = (v-1)
                              | es2P && es3P = kesimo2y3PseudoprimoAux k (c+1) (v+1)
                              | otherwise = kesimo2y3PseudoprimoAux k c (v+1)
                                where es2P = esAPseudoprimo v 2
                                      es3P = esAPseudoprimo v 3

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = kesimo2y3PseudoprimoAux k 0 1

-- EJERCICIO 5: esCarmichael
esCarmichaelHasta :: Integer -> Integer -> Bool
esCarmichaelHasta n m | m <= 1 = not (esPrimo n)
                      | sonCoprimos n m = esAPseudoprimo n m && esCarmichaelHasta n (m-1)
                      | otherwise = esCarmichaelHasta n (m-1)

esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelHasta n (n-1)
