type Polinomio = [Float] -- El polinomio 4X^3 + 3X − 2 se representa por [4, 0, 3, (-2)]. Nunca debe comenzar en 0.
type Monomio = (Float, Int) -- El monomio aX^n se representa con (a, n). a nunca debe ser igual a 0.
-- El polinomio 0 puede ser representado con []

limpiar :: [Float] -> Polinomio
limpiar [] = []
limpiar (c:cs) | c == 0 = limpiar cs
               | otherwise = (c:cs)

grado :: Polinomio -> Int
-- grado [_] = 0
grado [] = -1 -- Para divisiones
grado (c:cs) = 1 + grado cs

evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (c:cs) x = c*x^(grado (c:cs)) + evaluar cs x

suma :: Polinomio -> Polinomio -> Polinomio
suma f g = limpiar (sumaAux f g)
    where sumaAux :: Polinomio -> Polinomio -> Polinomio
          sumaAux f [] = f
          sumaAux [] g = g
          sumaAux f g = (sumaAux inif inig) ++ [lf + lg]
              where inif = init f
                    inig = init g
                    lf = last f
                    lg = last g

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar k f = limpiar (productoPorEscalarAux k f)
    where productoPorEscalarAux :: Float -> Polinomio -> Polinomio
          productoPorEscalarAux _ [] = []
          productoPorEscalarAux k (c:cs) = (k*c):productoPorEscalarAux k cs

resta :: Polinomio -> Polinomio -> Polinomio
resta f g = suma f (productoPorEscalar (-1) g)

productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (_, 0) [] = []
productoPorMonomio (a, n) [] = (productoPorMonomio (a, n-1) []) ++ [0]
productoPorMonomio (a, n) (c:cs) = (a*c):productoPorMonomio (a, n) cs

producto :: Polinomio -> Polinomio -> Polinomio
producto f [] = []
producto f (0:gs) = suma [] (producto f gs)
producto f (g:gs) = suma (productoPorMonomio (g, grado (g:gs)) f) (producto f gs)

hacerPolinomio :: Monomio -> Polinomio
hacerPolinomio (0, 0) = [0]
hacerPolinomio (a, 0) = [a]
hacerPolinomio (0, n) = hacerPolinomio (0, n-1) ++ [0]
hacerPolinomio (a, n) = a:hacerPolinomio (0, n-1)

derivadaMonomio :: Monomio -> Monomio
derivadaMonomio (_, 0) = (0, 0)
derivadaMonomio (a, n) = (a*(fromIntegral n), n-1)

derivada :: Polinomio -> Polinomio
derivada [] = []
derivada (c:cs) = suma (hacerPolinomio (derivadaMonomio (c, grado (c:cs)))) (derivada cs)

derivadaNEsima :: Polinomio -> Int -> Polinomio
derivadaNEsima f 0 = f
derivadaNEsima f n = derivadaNEsima (derivada f) (n-1)

primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente f d = (cpf/cpd, grf-grd)
    where grf = grado f
          grd = grado d
          cpf = head f
          cpd = head d

primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto f d = resta f (productoPorMonomio q d)
    where q = primerCociente f d

division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division p q = divisionAux p q []
    where divisionAux :: Polinomio -> Polinomio -> Polinomio -> (Polinomio, Polinomio)
          divisionAux p q res | grr >= grq = divisionAux r q (res ++ [fst c])
                              | otherwise = (res ++ [fst c], r)
              where c = primerCociente p q
                    r = primerResto p q
                    grr = grado r
                    grq = grado q

divide :: Polinomio -> Polinomio -> Bool -- ¿q divide a p?
divide p q | grp >= grq = snd (division p q) == []
           | otherwise = False
    where grp = grado p
          grq = grado q

potencia :: Int -> Polinomio -> Polinomio
potencia 0 _ = [1]
potencia n f = producto f (potencia (n-1) f)

mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP f [] = productoPorEscalar (1/(head f)) f
mcdP f g = mcdP g (snd (division f g))

multiplicidad :: Float -> Polinomio -> Int
multiplicidad x p = multiplicidadAux x p 0
    where multiplicidadAux :: Float -> Polinomio -> Int -> Int
          multiplicidadAux x p m | divide p (potencia m [1, -x]) = multiplicidadAux x p (m+1)
                                 | otherwise = m-1

raicesMultiples :: Polinomio -> Bool
raicesMultiples p = mcdP p (derivada p) /= [1]