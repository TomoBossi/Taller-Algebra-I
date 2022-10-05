-- Tipo [(Int, Int)] (diapo 21)

-- [1,0..(-100)] (lista decreciente de 1 a -100 inclusive, diapo 24)
-- [(-19),(-15)..20] (lista creciente entre -20 y 20 congruentes con 1 (4), diapo 24)

-- sumatoria :: [Int] -> Int
-- sumatoria l | l == [] = 0
--             | otherwise = head l + sumatoria (tail l)

-- longitud :: [Int] -> Int -- No pude con [a], requiere especificar trait Eq
-- longitud l | l == [] = 0
--            | otherwise = 1 + longitud (tail l)

-- pertenece :: Int -> [Int] -> Bool
-- pertenece v l | l == [] = False
--               | otherwise = v == head l || pertenece v (tail l)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

pertenece :: Int -> [Int] -> Bool -- (diapo 31)
pertenece v [] = False
pertenece v (x:xs) = v == x || pertenece v xs

-- Ejercicios (diapo 32) (TODO ----------)

-- Productoria
productoria :: [Int] -> Int
productoria l | l == [] = 1
              | otherwise = (head l) * productoria (tail l)

productoriaPM :: [Int] -> Int
productoriaPM [] = 1
productoriaPM (x:xs) = x * productoriaPM xs

-- sumarN
sumarN :: Int -> [Int] -> [Int]
sumarN n xs | xs == [] = []
            | otherwise = (n + head xs):sumarN n (tail xs)

sumarNPM :: Int -> [Int] -> [Int]
sumarNPM n [] = []
sumarNPM n (x:xs) = (n+x):sumarNPM n xs

-- sumarElPrimero
-- sumarElPrimero :: [Int] -> [Int] ----------

-- sumarElPrimeroPN :: [Int] -> [Int] ----------

-- sumarElUltimo
ultimo :: [Int] -> Int
ultimo xs | longitud xs == 1 = head xs
          | otherwise = ultimo (tail xs)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs | xs == [] = []
                 | otherwise = (ultimo xs + head xs):sumarElUltimo (tail xs)

sumarElUltimoPM :: [Int] -> [Int]
sumarElUltimoPM [] = []
sumarElUltimoPM (x:xs) = (ultimo (x:xs) + x):sumarElUltimoPM xs

-- pares
esPar :: Int -> Bool
esPar n = mod n 2 == 0

pares :: [Int] -> [Int]
pares l | l == [] = []
        | esPar (head l) = (head l):pares (tail l)
        | otherwise = pares (tail l)

paresPM :: [Int] -> [Int]
paresPM [] = []
paresPM (x:xs) | esPar x = x:paresPM xs
               | otherwise = paresPM xs

-- multiplosDeN
esMultM :: Int -> Int -> Bool
esMultM n m = mod n m == 0

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n xs | xs == [] = []
                  | esMultM (head xs) n = (head xs):multiplosDeN n (tail xs)
                  | otherwise = multiplosDeN n (tail xs)

multiplosDeNPM :: Int -> [Int] -> [Int]
multiplosDeNPM n [] = []
multiplosDeNPM n (x:xs) | esMultM x n = x:multiplosDeNPM n xs
                        | otherwise = multiplosDeNPM n xs

-- reverso
-- reverso :: [Int] -> [Int] ----------

-- reversoPM :: [Int] -> [Int] ----------

-- maximo
-- maximo :: [Int] -> Int ----------

-- maximoPM :: [Int] -> Int ----------

-- ordenar
-- ordenar :: [Int] -> [Int] ----------

-- ordenarPM :: [Int] -> [Int] ----------

-- quitar
-- quitar :: Int -> [Int] -> [Int] ----------

-- quitarPM :: Int -> [Int] -> [Int] ----------

-- hayRepetidos
-- hayRepetidos :: [Int] -> Bool ----------

-- hayRepetidosPM :: [Int] -> Bool ----------

-- eliminarRepetidos
-- eliminarRepetidos :: [Int] -> [Int] ----------

-- eliminarRepetidosPM :: [Int] -> [Int] ----------