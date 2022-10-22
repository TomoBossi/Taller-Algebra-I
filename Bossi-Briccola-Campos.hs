-- Bossi Tomás
-- Briccola Francina
-- Campos Ezequiel

-- TP 1

-- EJERCICIO 1: sonCoprimos
minimo :: Integer -> Integer -> Integer
minimo n1 n2 | n1 < n2 = n1
             | otherwise = n2

sonCoprimosHasta :: Integer -> Integer -> Integer -> Bool
sonCoprimosHasta n1 n2 m | m == 1 = True
                         | mod n1 m == 0 && mod n2 m == 0 = False
                         | otherwise = sonCoprimosHasta n1 n2 (m-1)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n1 n2 = sonCoprimosHasta n1 n2 (minimo n1 n2)

-- EJERCICIO 2: es2Pseudoprimo
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n m | n == m = n
                      | mod n m == 0 = m
                      | otherwise = menorDivisorDesde n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n = n == menorDivisor (n) && n /= 1

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = not (esPrimo n || n == 1) && mod (2^(n-1)-1) n == 0

-- EJERCICIO 3: cantidad3Pseudoprimos
esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo n a = not (esPrimo n || n == 1) && mod (a^(n-1)-1) n == 0

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m | m == 1 = 0
                        | esAPseudoprimo m 3 = 1 + cantidad3Pseudoprimos (m-1)
                        | otherwise = cantidad3Pseudoprimos (m-1)

-- EJERCICIO 4: kesimo2y3Pseudoprimo
kesimo2y3PseudoprimoAux :: Integer -> Integer -> Integer
kesimo2y3PseudoprimoAux k v | k == 0 = (v-1)
                            | es2P && es3P = kesimo2y3PseudoprimoAux (k-1) (v+1)
                            | otherwise = kesimo2y3PseudoprimoAux k (v+1)
                              where es2P = esAPseudoprimo v 2
                                    es3P = esAPseudoprimo v 3

kesimo2y3Pseudoprimo1 :: Integer -> Integer
kesimo2y3Pseudoprimo1 k = kesimo2y3PseudoprimoAux k 1 

proximo2y3Pseudoprimo :: Integer -> Integer
proximo2y3Pseudoprimo v | es2P && es3P = (v+1)
                        | otherwise = proximo2y3Pseudoprimo (v+1)
                          where es2P = esAPseudoprimo (v+1) 2
                                es3P = esAPseudoprimo (v+1) 3

distanciaEntreProximos2y3P :: Integer -> Integer
distanciaEntreProximos2y3P v = prox (prox v) - prox v
                               where prox = proximo2y3Pseudoprimo

cantidad2y3Pseudoprimos :: Integer -> Integer
cantidad2y3Pseudoprimos m | m == 1 = 0
                          | es2P && es3P = 1 + cantidad2y3Pseudoprimos (m-1)
                          | otherwise = cantidad2y3Pseudoprimos (m-1)
                            where es2P = esAPseudoprimo m 2
                                  es3P = esAPseudoprimo m 3

kesimo2y3Pseudoprimo2Aux :: Integer -> Integer -> Integer
kesimo2y3Pseudoprimo2Aux k v | k == cantidad2y3Pseudoprimos v = v
                             | otherwise = kesimo2y3Pseudoprimo2Aux k (v+1)

kesimo2y3Pseudoprimo2 :: Integer -> Integer
kesimo2y3Pseudoprimo2 k = kesimo2y3Pseudoprimo2Aux k 1 -- Extremadamente lento

kesimo2y3Pseudoprimo3 k | k == 0 = 0
                        | otherwise = prox (kesimo (k-1))
                          where kesimo = kesimo2y3Pseudoprimo3
                                prox = proximo2y3Pseudoprimo
                                
-- EJERCICIO 5: esCarmichael
esCarmichaelHasta :: Integer -> Integer -> Bool
esCarmichaelHasta n m | m == 0 = False
                      | m == 1 = n /= 2
                      | sonCoprimos n m = esMP && esCarmichaelHasta n (m-1)
                      | otherwise = esCarmichaelHasta n (m-1)
                        where esMP = esAPseudoprimo n m

esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelHasta n (n-1)
