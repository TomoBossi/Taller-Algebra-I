division :: Integer -> Integer -> (Integer, Integer) -- clase09
division a d 
  | (a < d && a >= 0) || (a > -d && a < 0) = (0, a)           
  | a < 0 = (fst qrNeg' - 1, snd qrNeg')
  | otherwise = (fst qrPos' + 1, snd qrPos')
  where 
    qrPos' = division (a - d) d
    qrNeg' = division (a + d) d

emcd :: Integer -> Integer -> (Integer, Integer, Integer) -- clase09
emcd a 0 = (a, 1, 0)
emcd a b = (g, s, t)
  where 
    (g, s', t') = emcd b (mod a b)
    s = t'
    t = s' - t' * q
    q = div a b

tieneSolucion :: Integer -> Integer -> Integer -> Bool 
tieneSolucion a b m = mod b g == 0
  where (g, _, _) = emcd a m

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m 
  | tieneSolucion a b m = mod (b*(fst (division s' g'))) m'
  | otherwise = undefined
  where 
    (g', s', _) = emcd a' m'
    a' = fst (division a g)
    m' = fst (division m g)
    (g, _, _) = emcd a m

solucionGeneral :: Integer -> Integer -> Integer -> (Integer, Integer)
solucionGeneral a b m = (solucionParticular a b m, m')
  where 
    m' = fst (division m g)
    (g, _, _) = emcd a m
