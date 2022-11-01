type Set a = [a]
vacio :: Set Int
vacio = []

combinatorio :: Int -> Int -> Int
combinatorio n m | m == 0 || n == m = 1 -- m == 1 = n , n == m = 1
                 | otherwise = combinatorio (n-1) m + combinatorio (n-1) (m-1)

agregarUnico :: Set Int -> [Int] -> Set (Set Int)
agregarUnico [] _ = []
agregarUnico (x:xs) r = [x:r] ++ agregarUnico xs r -- ++ !!!

agregarTodos :: Set Int -> [[Int]] -> [[Int]]
agregarTodos c [] = []
agregarTodos c (x:xs) = agregarUnico c x ++ agregarTodos c xs
     
var :: Set Int -> Int -> [[Int]]
var _ 0 = [[]]
var c l = agregarTodos c (var c (l-1))

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n 1 = n:l
insertarEn l n i = head l : insertarEn (tail l) n (i-1)

insertarTodos :: [Int] -> Int -> Int -> [[Int]]
insertarTodos l n 0 = []
insertarTodos l n i = [insertarEn l n i] ++ insertarTodos l n (i-1)
                      
insertarTodosTodosLados :: [[Int]] -> Int -> [[Int]]
insertarTodosTodosLados [] _ = []
insertarTodosTodosLados c n = insertarTodos (head c) n n ++ insertarProx
    where insertarProx = insertarTodosTodosLados (tail c) n
                      
per :: Int -> [[Int]]
per n | n == 0 = [[]]
      | otherwise = insertarTodosTodosLados (per (n-1)) n
      
-- 1
distriBolitas :: Int -> Int -> [[Int]]
distriBolitas 0 _ = [[]]
distriBolitas n k = agregarCaja k (distriBolitas (n-1) k)
    where agregarCaja :: Int -> [[Int]] -> [[Int]]
          agregarCaja k x = agregarCajaAux k x x
              where agregarCajaAux :: Int -> [[Int]] -> [[Int]] -> [[Int]]
                    agregarCajaAux 1 [] _ = []
                    agregarCajaAux k [] y = agregarCajaAux (k-1) y y
                    agregarCajaAux k (x:xs) y = [k:x] ++ agregarCajaAux k xs y

-- 3
listOrd :: Int -> Int -> [[Int]]
listOrd 0 _ = [[]]
listOrd k n = agregarNoRepe n (listOrd (k-1) n)
    where agregarNoRepe :: Int -> Set (Set Int) -> Set (Set Int)
          agregarNoRepe n x = agregarNoRepeAux n n x x 
              where agregarNoRepeAux :: Int -> Int -> Set (Set Int) -> Set (Set Int) -> Set (Set Int)
                    agregarNoRepeAux _ 0 _ _ = []
                    agregarNoRepeAux n m [] y = agregarNoRepeAux n (m-1) y y
                    agregarNoRepeAux n m (x:xs) y | not (elem m x) = [m:x] ++ agregarNoRepeAux n m xs y
                                                  | otherwise = agregarNoRepeAux n m xs y

-- 2 -- rehacer más recursivamente // preguntar en clase
subconjuntos :: Int -> Int -> Set (Set Int)
subconjuntos k n = filtrarRepe (listOrd k n)
    where filtrarRepe :: [[Int]] -> Set (Set Int)
          filtrarRepe x = filtrarRepeAux x []
              where filtrarRepeAux :: [[Int]] -> Set (Set Int) -> Set (Set Int)
                    filtrarRepeAux [] f = f
                    filtrarRepeAux (x:xs) f | incluidoSubconj x xs = filtrarRepeAux xs f
                                            | otherwise = filtrarRepeAux xs (x:f)
                        where incluidoSubconj :: Set Int -> [[Int]] -> Bool
                              incluidoSubconj _ [] = False
                              incluidoSubconj c (x:xs) = iguales c x || incluidoSubconj c xs
                                  where iguales :: Set Int -> Set Int -> Bool -- clase07
                                        iguales a b = incluido a b && incluido b a
                                            where incluido :: Set Int -> Set Int -> Bool -- clase07
                                                  incluido [] b = True
                                                  incluido (x:xs) b = elem x b && incluido xs b

-- 4 -- podría haberse logrado filtrando var [0,1] 6 de manera similar a -- 5, cambiando solo c1 > c0 por c1 == c0
sucesion6 :: [[Int]]
sucesion6 = sucesionesBalanceadas 6
    where sucesionesBalanceadas :: Int -> [[Int]]
          sucesionesBalanceadas 0 = [[]]
          sucesionesBalanceadas l = agregarTodos [0,1] x
              where x = sucesionesBalanceadas (l-1)

-- 5 -- rehacer más recursivamente // preguntar en clase
sucesion5masUnos :: [[Int]]
sucesion5masUnos = sucesionesConMasUnos 5
    where sucesionesConMasUnos :: Int -> [[Int]]

          --sucesionesConMasUnos 0 = [[]]
          --sucesionesConMasUnos l = agregarSiC1MayorIgualC0 (sucesionesConMasUnos (l-1))
              -- where agregarSiC1MayorIgualC0 :: [[Int]] -> [[Int]]
              --       agregarSiC1MayorIgualC0 [] = []
              --       agregarSiC1MayorIgualC0 (x:xs) | c1 > c0 + 1 = [0:x] ++ [1:x] ++ agregarSiC1MayorIgualC0 xs
              --                                      | otherwise = [1:x] ++ agregarSiC1MayorIgualC0 xs

          sucesionesConMasUnos l = filtrarMasUnos (var [0,1] l)
              where filtrarMasUnos :: [[Int]] -> [[Int]]
                    filtrarMasUnos [] = []
                    filtrarMasUnos (x:xs) | c1 > c0 = x:filtrarMasUnos xs
                                          | otherwise = filtrarMasUnos xs

                        where c1 = contarE 1 x
                              c0 = contarE 0 x
                              contarE :: Int -> [Int] -> Int
                              contarE _ [] = 0
                              contarE e (x:xs) | e == x = 1 + contarE e xs
                                               | otherwise = contarE e xs
