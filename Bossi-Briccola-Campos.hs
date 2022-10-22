-- Bossi Tomás
-- Briccola Francina
-- Campos Ezequiel

-- TP 1

-- EJERCICIO 1: sonCoprimos
sonCoprimosDesde :: Integer -> Integer -> Integer -> Bool
sonCoprimosDesde n1 n2 m | m > n1 || m > n2 = True
                         | mod n1 m == 0 && mod n2 m == 0 = False
                         | otherwise = sonCoprimosDesde n1 n2 (m+1)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n1 n2 = sonCoprimosDesde n1 n2 2

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
proximo2y3Pseudoprimo :: Integer -> Integer
proximo2y3Pseudoprimo v | proxEs2P && proxEs3P = (v+1)
                        | otherwise = proximo2y3Pseudoprimo (v+1)
                          where proxEs2P = esAPseudoprimo (v+1) 2
                                proxEs3P = esAPseudoprimo (v+1) 3

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k | k == 1 = prox (1)
                       | otherwise = prox (prev)
                         where prev = kesimo2y3Pseudoprimo (k-1)
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
