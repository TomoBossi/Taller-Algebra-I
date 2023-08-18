-- division :: Int -> Int -> (Int,Int)
-- division a d | a < d = (0, a)
--              | otherwise = (fst qr' + 1, snd qr')
--     where qr' = division (a - d) d

-- Nota: GHCI incluye al comando :set +s, que permite mostrar tiempo de ejecución y memoria ocupada de cada comando.

division :: Int -> Int -> (Int,Int)
division a d 
  | (a < d && a >= 0) || (a > -d && a < 0) = (0, a)
  | a < 0 = (fst qrNeg' - 1, snd qrNeg')
  | otherwise = (fst qrPos' + 1, snd qrPos')
  where 
    qrPos' = division (a - d) d
    qrNeg' = division (a + d) d

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (snd (division a b))

mayorDivisorComun :: Int -> Int -> Int
mayorDivisorComun a b 
  | a == b = a
  | a > b = mayorDivisorComun da b
  | a < b = mayorDivisorComun a db
  where 
    da = fst (division a (menorDivisor a))
    db = fst (division b (menorDivisor b))
    menorDivisor :: Int -> Int -- clase05
    menorDivisor 1 = 1
    menorDivisor n = menorDivisorDesde n 2
      where 
        menorDivisorDesde :: Int -> Int -> Int
        menorDivisorDesde n m 
          | n == m = n
          | snd (division n m) == 0 = m
          | otherwise = menorDivisorDesde n (m+1)

emcd :: Int -> Int -> (Int,Int,Int) -- (g, s, t)
emcd a 0 = (a, 1, 0)
emcd a b = (g, t', s' - t'*q)
  where 
    g = fst3 (emcd')
    q = fst (division a b)
    s' = snd3 (emcd')
    t' = trd3 (emcd')
    emcd' = emcd b (snd (division a b))
    fst3 :: (Int,Int,Int) -> Int
    fst3 (x , _ , _ ) = x
    snd3 :: (Int,Int,Int) -> Int
    snd3 (_ , y , _ ) = y
    trd3 :: (Int,Int,Int) -> Int
    trd3 (_ , _ , z ) = z
