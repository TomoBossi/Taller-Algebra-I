-- 1 No estoy seguro de entender el enunciado, pero acá va
estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y = (x <= 3 && y <= 3) || (x > 3 && x <= 7 && y > 3 && y <= 7) || (x > 7 && y > 7)

-- 2
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (vx, vy) (wx, wy) = vx*wx + vy*wy

-- 3
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (vx, vy) (wx, wy) = vx < wx && vy < wy

-- 4
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (vx, vy) (wx, wy) = sqrt((vx-wx)**2 + (vy-wy)**2)

-- 5
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (vx, vy, vz) = vx + vy + vz

-- 6 Asumo que quieren que indexemos desde 1
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (vx, vy, vz) | mod vx 2 == 0 = 1
                            | mod vy 2 == 0 = 2
                            | mod vz 2 == 0 = 3
                            | otherwise = 4

-- 7
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

-- 8
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)