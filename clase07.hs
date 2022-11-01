type Set a = [a]
vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar x c | not (elem x c) = x:c
            | otherwise = c

incluido :: Set Int -> Set Int -> Bool
incluido [] b = True
incluido (x:xs) b = elem x b && incluido xs b

iguales :: Set Int -> Set Int -> Bool
iguales a b = incluido a b && incluido b a
