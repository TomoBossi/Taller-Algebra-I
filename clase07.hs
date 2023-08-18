type Set a = [a]
vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar x c 
  | not (elem x c) = x:c
  | otherwise = c

incluido :: Set Int -> Set Int -> Bool
incluido [] b = True
incluido (x:xs) b = elem x b && incluido xs b

iguales :: Set Int -> Set Int -> Bool
iguales a b = incluido a b && incluido b a

partes :: Int -> Set (Set Int)
partes 0 = [[]]
partes n = agregarTodos n (partes (n-1)) ++ partes (n-1)
  where 
    agregarTodos :: Int -> Set [Int] -> Set [Int]
    agregarTodos _ [] = []
    agregarTodos x (c:cs) = [x:c] ++ agregarTodos x cs

productoCartesiano :: Set Int -> Set Int -> Set (Int,Int)
productoCartesiano [] _ = []
productoCartesiano (x:xs) ys = emparejar x ys ++ productoCartesiano xs ys
  where 
    emparejar :: Int -> Set Int -> Set (Int,Int)
    emparejar x [] = []
    emparejar x (y:ys) = (x,y):emparejar x ys
