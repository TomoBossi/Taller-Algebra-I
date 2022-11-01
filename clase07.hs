type Set a = [a]
vacio :: Set Int
vacio = []

-- Clase 6

longitud :: Set Int -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

indexAux :: Set Int -> Int -> Int -> Int
indexAux [] n i = i
indexAux (x:xs) n i | x == n = i
                    | otherwise = indexAux xs n (i+1)

index :: Set Int -> Int -> Int
index c n = indexAux c n 0

quitarAux :: Int -> Set Int -> Int -> Int -> Set Int
quitarAux n [] i c = vacio
quitarAux n (x:xs) i c | not (elem n (x:xs)) = (x:xs)
                       | i == c = quitarAux n xs i (c+1)
                       | otherwise = x:quitarAux n xs i (c+1)

quitar :: Int -> Set Int -> Set Int
quitar n c = quitarAux n c (index c n) 0

valorEnIndice :: Set a -> Int -> a
valorEnIndice c i | i == 0 = head c
                  | otherwise = valorEnIndice (tail c) (i-1)

-- Clase 7

agregar :: Int -> Set Int -> Set Int
agregar x c | not (elem x c) = x:c
            | otherwise = c

incluido :: Set Int -> Set Int -> Bool
-- incluido a b | a == vacio = True
--              | otherwise = elem (head a) b && incluido (tail a) b

-- Con Pattern Matching
-- incluido vacio b = True -- ¿Por qué no anda?
incluido [] b = True
incluido (x:xs) b = elem x b && incluido xs b

iguales :: Set Int -> Set Int -> Bool
iguales a b = incluido a b && incluido b a

-- Partes

subAux :: Set Int -> Int -> Set (Set Int)
subAux c i | i == 0 = [tail c]
           | otherwise = quitar (valorEnIndice c i) c:subAux c (i-1)

sub :: Set Int -> Set (Set Int)
sub c = subAux c ((longitud c) - 1)
