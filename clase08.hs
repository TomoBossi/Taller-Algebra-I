type Set a = [a]
vacio :: Set Int
vacio = []

-- Clase 6

longitud :: Set Int -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--

combinatorio :: Int -> Int -> Int
combinatorio n m | m == 0 || n == m = 1 -- m == 1 = n , n == m = 1
                 | otherwise = combinatorio (n-1) m + combinatorio (n-1) (m-1)

--

agregarUnico :: Set Int -> [Int] -> Set [Int]
agregarUnico c r | c == vacio = []
                 | otherwise = [(head c):r] ++ agregarUnico (tail c) r -- ++ !!!

agregarTodos :: Set Int -> [[Int]] -> Set [Int]
agregarTodos c cr | cr == [] = []
                  | otherwise = agregarUnico c (head cr) ++ agregarTodos c (tail cr)

-- Clase
     
var :: Set Int -> Int -> Set [Int]
var _ 0 = [[]]
var c l = agregarTodos c (var c (l-1))

--

-- Poco Haskelliano

insertarEnDesdeK :: [Int] -> Int -> Int -> Int -> [Int] -> [Int]
insertarEnDesdeK l n i k res | k == i = res ++ [n] ++ l
                             | k-1 == longitud l  = l ++ [n]
                             | otherwise = insertarEnDesdeK (tail l) n i (k+1) (res ++ [head l])

insertarEnl :: [Int] -> Int -> Int -> [Int]
insertarEnl l n i = insertarEnDesdeK l n i 1 []

-- Intento Haskelliano

insertarEnAux :: [Int] -> Int -> Int -> [Int] -> [Int]
insertarEnAux l n i res | i-1 == 0 = res ++ [n] ++ l
                        | otherwise = insertarEnAux (tail l) n (i-1) (res ++ [head l])

insertarEnl2 :: [Int] -> Int -> Int -> [Int]
insertarEnl2 l n i = insertarEnAux l n i []

-- Clase

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n 1 = n:l
insertarEn l n i = head l : insertarEn (tail l) n (i-1)

--

insertarTodos :: [Int] -> Int -> Int -> Set [Int] 
insertarTodos l n i | i == 0 = []
                    | otherwise = [insertarEn l n i] ++ insertarTodos l n (i-1)
                      
insertarTodosTodosLados :: Set [Int] -> Int -> Set [Int]
insertarTodosTodosLados c n | c == [] = []
                            | otherwise = insertarTodos (head c) n n ++ insertarProx
                              where insertarProx = insertarTodosTodosLados (tail c) n
                      
per :: Int -> Set [Int]
per n | n == 0 = [[]]
      | otherwise = insertarTodosTodosLados (per (n-1)) n
      
-- 1 ()


