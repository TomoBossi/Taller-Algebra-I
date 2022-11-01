sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

pertenece :: Int -> [Int] -> Bool -- (diapo 31)
pertenece v [] = False
pertenece v (x:xs) = v == x || pertenece v xs

-- Ejercicios (diapo 32)

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
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

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
reversoAux :: [Int] -> [Int] -> Int -> [Int]
reversoAux xs sxs n | longitud sxs == n = sxs
                    | otherwise = reversoAux (tail xs) ((head xs):sxs) n

reverso :: [Int] -> [Int]
reverso xs = reversoAux xs [] (longitud xs)

spliceTo :: [Int] -> Int -> [Int]
spliceTo xs end = reverso (reversoAux xs [] end)

spliceFrom :: [Int] -> Int -> [Int]
spliceFrom xs start = reversoAux (reverso xs) [] ((longitud xs) - start)

splice :: [Int] -> Int -> Int -> [Int]
splice xs start end = spliceFrom (spliceTo xs end) start

reversoAuxPM :: [Int] -> [Int] -> [Int]
reversoAuxPM [] sxs = sxs
reversoAuxPM (x:xs) sxs = reversoAuxPM xs (x:sxs)

reversoPM :: [Int] -> [Int]
reversoPM xs = reversoAuxPM xs []

-- maximo
maximoAux :: [Int] -> Int -> Int
maximoAux xs max | xs == [] = max
                 | max < head xs = maximoAux (tail xs) (head xs)
                 | otherwise = maximoAux (tail xs) max

maximo :: [Int] -> Int
maximo xs = maximoAux xs (head xs)

maximoAuxPM :: [Int] -> Int -> Int
maximoAuxPM [] max = max
maximoAuxPM (x:xs) max | max < x = maximoAuxPM xs x
                       | otherwise = maximoAuxPM xs max

maximoPM :: [Int] -> Int
maximoPM xs = maximoAuxPM xs (head xs)

-- quitar
inList :: [Int] -> Int -> Bool
inList xs n | xs == [] = False
            | n == head xs = True
            | otherwise = inList (tail xs) n

indexAux :: [Int] -> Int -> Int -> Int -- Indexing from 0
indexAux xs n i | xs == [] = i
                | n == head xs = i
                | otherwise = indexAux (tail xs) n (i+1)

index :: [Int] -> Int -> Int
index xs n = indexAux xs n 0 

quitarAux :: Int -> [Int] -> Int -> Int -> [Int]
quitarAux n xs i c | not (inList xs n) || xs == [] = xs
                   | i == c = quitarAux n (tail xs) i (c+1)
                   | otherwise = (head xs):quitarAux n (tail xs) i (c+1)

quitar :: Int -> [Int] -> [Int]
quitar n xs = quitarAux n xs (index xs n) 0

inListPM :: [Int] -> Int -> Bool
inListPM [] n = False
inListPM (x:xs) n | x == n = True
                  | otherwise = inListPM xs n

indexAuxPM :: [Int] -> Int -> Int -> Int
indexAuxPM [] n i = i
indexAuxPM (x:xs) n i | x == n = i
                      | otherwise = indexAuxPM xs n (i+1)

indexPM :: [Int] -> Int -> Int
indexPM xs n = indexAuxPM xs n 0 

quitarAuxPM :: Int -> [Int] -> Int -> Int -> [Int]
quitarAuxPM n [] i c = []
quitarAuxPM n (x:xs) i c | not (inListPM (x:xs) n) = (x:xs)
                         | i == c = quitarAuxPM n xs i (c+1)
                         | otherwise = x:quitarAuxPM n xs i (c+1)

quitarPM :: Int -> [Int] -> [Int]
quitarPM n xs = quitarAuxPM n xs (indexPM xs n) 0

-- ordenar
ordenarAux :: [Int] -> Int -> Int -> [Int]
ordenarAux xs len c | c == len - 1 = xs
                    | otherwise = m:ordenarAux (quitar m xs) len (c+1)
                      where m = maximo xs

ordenar :: [Int] -> [Int]
ordenar xs = reverso (ordenarAux xs (longitud xs) 0)

-- Por cómo es ordenarAux, no tiene sentido implementar una versión con PM.

-- hayRepetidos
valueInIndex :: [Int] -> Int -> Int
valueInIndex xs i | i == 0 = head xs
                  | i == (longitud xs) - 1 = ultimo xs
                  | otherwise = head (splice xs i (i+1))

contarAux :: [Int] -> Int -> Int -> Int
contarAux xs n c | xs == [] = c
                 | n == head xs = contarAux (tail xs) n (c+1)
                 | otherwise = contarAux (tail xs) n c

contar :: Int -> [Int] -> Int
contar n xs = contarAux xs n 0

hayRepetidosAux :: [Int] -> Int -> Bool
hayRepetidosAux xs c | c == (longitud xs - 1) = repetidoEnC
                     | otherwise = repetidoEnC || hayRepetidosAux xs (c+1)
                       where repetidoEnC = (contar (valueInIndex xs c) xs) > 1

hayRepetidos :: [Int] -> Bool
hayRepetidos xs = hayRepetidosAux xs 0

-- Por cómo es hayRepetidosAux, no tiene sentido implementar una versión con PM.

-- eliminarRepetidos
repetidoAux :: [Int] -> Int -> Int
repetidoAux xs c | c == (longitud xs - 1) && repetidoEnC = v
                 | repetidoEnC = v
                 | otherwise = repetidoAux xs (c+1)
                   where v = valueInIndex xs c
                         repetidoEnC = (contar v xs) > 1

repetido :: [Int] -> Int
repetido xs = repetidoAux xs 0

eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos xs | not (hayRepetidos xs) = xs
                     | otherwise = eliminarRepetidos (quitar (repetido xs) xs)

-- Por cómo es eliminarRepetidos, no tiene sentido implementar una versión con PM.
