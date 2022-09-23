-- 1
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n m | m == 1 = 1
                       | mod n m == 0 = m + sumaDivisoresHasta n (m-1)
                       | otherwise = sumaDivisoresHasta n (m-1)

-- 2
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

-- 3
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n m | n == m = n
                      | mod n m == 0 = m
                      | otherwise = menorDivisorDesde n (m+1)

menorDivisor :: Int -> Int
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

-- 4
esPrimo :: Int -> Bool
esPrimo n = n == menorDivisor n

-- 5
proximoPrimo :: Int -> Int
proximoPrimo m | esPrimo m = m
               | otherwise = proximoPrimo (m+1)

nEsimoPrimoAux :: Int -> Int -> Int
nEsimoPrimoAux n m | n == 1 = m
                   | otherwise = nEsimoPrimoAux (n-1) (proximoPrimo (m+1))

nEsimoPrimo :: Int -> Int
nEsimoPrimo n = nEsimoPrimoAux n 2

-- 6
fact :: Int -> Int
fact n | n == 0 = 1
       | otherwise = n * fact (n-1)

menorFactDesdeAux :: Int -> Int -> Int
menorFactDesdeAux m k | n >= m = n 
                      | otherwise = menorFactDesdeAux m (k+1)
                        where n = fact k

menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeAux m 1

-- 7
mayorFactHastaAux :: Int -> Int -> Int
mayorFactHastaAux m k | n > m = fact (k-1)
                      | otherwise = mayorFactHastaAux m (k+1)
                        where n = fact k

mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaAux m 1

-- 8
esFact :: Int -> Bool
esFact n = menorFactDesde n == mayorFactHasta n

-- 9
fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

esFibonacciAux :: Int -> Int -> Bool
esFibonacciAux n m | n == f = True
                   | n < f = False
                   | otherwise = esFibonacciAux n (m+1)
                     where f = fib m

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciAux n 0

-- 10
sumaPrimerosPrimos :: Int -> Int -> Int
sumaPrimerosPrimos n m | n == 1 = m
                       | otherwise = m + sumaPrimerosPrimos (n-1) (proximoPrimo (m+1))

esSumaInicialDePrimosAux :: Int -> Int -> Bool
esSumaInicialDePrimosAux n m | n == s = True
                             | n < s = False
                             | otherwise = esSumaInicialDePrimosAux n (m+1)
                               where s = sumaPrimerosPrimos m 2

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosAux n 1

-- 11
tomaValorMaxAux :: Int -> Int -> Int -> Int -> Int
tomaValorMaxAux n1 n2 m max | n1 > n2 = m
                            | s > max = tomaValorMaxAux n1 (n2-1) n2 s
                            | otherwise = tomaValorMaxAux n1 (n2-1) m max
                              where s = sumaDivisores n2

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = tomaValorMaxAux n1 n2 n2 (sumaDivisores n2)

-- 11
tomaValorMinAux :: Int -> Int -> Int -> Int -> Int
tomaValorMinAux n1 n2 m min | n1 > n2 = m
                            | s < min = tomaValorMinAux n1 (n2-1) n2 s
                            | otherwise = tomaValorMinAux n1 (n2-1) m min
                              where s = sumaDivisores n2

tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 = tomaValorMinAux n1 n2 n2 (sumaDivisores n2)