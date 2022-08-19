-- clase 1

f x y = x * x + y * y -- f 2 3 = 13
g x y z = x + y + z * z -- g 2 3 4 = 21
h x y z = f x y + g x y z -- h 2 3 4 = 34

-- funcioncitas
doble x = 2 * x -- doble -1 tira error, doble (-1) ok (?)
suma x y = x + y
normaVectorial x1 x2 = sqrt(x1**2 + x2**2) -- se usa camelCase!!! :D
funcionConstante8 x = 8

-- funcioncitas con condicionales
condicional n | n == 0 = 1
              | n == 3 = 9
              | n > 3 = 3290482309847
              | otherwise = 5

signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1 -- otherwise = -1
        
maximo x y | x >= y = x
           | otherwise = y

-- qué hacen las siguientes funciones?
f1 n | n >= 3 = 5 -- Exception: clase1.hs:27:1-17: Non-exhaustive patterns in function f1 con n < 3
f2 n | n >= 3 = 5 
     | n <= 1 = 8 -- Exception: clase1.hs:27:1-17: Non-exhaustive patterns in function f1 con n = 2
f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8 -- Exception: Prelude.undefined CallStack (from HasCallStack): error, called at libraries/base/GHC/Err.hs:78:14 in base:GHC.Err undefined, called at clase1.hs:31:17 in main:Main con n = 2
     
-- div 13 5 = 2
-- mod 13 5 = 3

-- /= es el distinto

-- f4 y f5 muestran que las guardas se ejecutan de arriba a abajo

-- pattern matching. función que devuelve 1 si n = 0, 0 si no
f6 n | n == 0 = 1
     | n /= 0 = 0
-- puede reescribirse como
f7 0 = 1
f7 n = 0 -- ... si uso f me tira errores, por qué f7 no tira error por intento de redeclaración?
-- f7 2 = 2 -- tira warning de redundancia (porque por orden de "guardas", f7 2 = 0), pero no tira error. O sea que error redeclaración depende del orden de instrucciones y de si hay algo declarado en medio

f8 x | x<9 = 0
f8 x | x>3 = 1

-- f7 2 = 2 -- acá tirar error por redeclaración de f7

-- signo con pattern matching
signo2 0 = 0
signo2 n | n > 0 = 1
         | otherwise = -1
        
-- where (ejemplos para cuadraticas con A = 1)
cantidadDeSoluciones b c | b*2 - 4*c > 0 = 2
                         | b*2 - 4*c == 0 = 1
                         | otherwise = 0
                         
cantidadDeSoluciones2 b c | d > 0 = 2
                          | d == 0 = 1
                          | otherwise = 0
                          where d = b*2 - 4*c
                          
cantidadDeSoluciones3 b c | e == 1 = 2
                          | e == 0 = 1
                          | otherwise = 0
                          where e = (signo2 b*2 - 4*c) -- no anda, chequear
                          
-- chequear funcionRara
                          
-- logica con &&, ||, not

-- se puede especificar tipo de dato, dominio y codominio de fn
-- (SIGNATURA//FIRMA)
-- ej:
maximo2 :: Int -> Int -> Int
maximo2 x y | x >= y = x
            | otherwise = y
                          
esPar :: Int -> Bool
esPar n = mod n 2 == 0

-- ejercicios última diapo
-- 1
absoluto :: Int -> Int
absoluto n = n * signo n

-- 2
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y = maximo (absoluto x) (absoluto y)

-- 3
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = maximo (maximo x y) z 

-- 4
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = x * y == 0

algunoEs02 :: Float -> Float -> Bool
algunoEs02 0 _ = True
algunoEs02 _ 0 = True
algunoEs02 x y = False

-- 5
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = x + y == 0 && algunoEs0 x y

ambosSon02 :: Float -> Float -> Bool
ambosSon02 0 0 = True
ambosSon02 x y = False

-- 6
esMultiploDe :: Int -> Int -> Bool 
esMultiploDe x y = mod x y == 0 -- esMultiploDe (-10) 5 = True

-- 7
digitoUnidades :: Int -> Int
digitoUnidades x = y - 10 * div y 10
    where y = absoluto x -- extiende a los reales, innecesario
-- con el where empezando desde margen, problema de indentación... sad :(
-- igual, no hace falta el where porque se aclara que son solo naturales

-- 8
digitoDecenas :: Int -> Int
digitoDecenas x = digitoUnidades (div (y - digitoUnidades y) 10)
                  where y = absoluto x -- extiende a los reales, innecesario -- indentación más fachera (según yo y según uno de los ayudantes)
                  
digitoDecenas2 :: Int -> Int
digitoDecenas2 y = digitoUnidades (div (y - digitoUnidades y) 10)
    where y = absoluto y -- extiende a los reales, innecesario
-- No puedo sobreescribir a la variable y así :( curiosamente, entra en loop infinito. La ejecución se puede cortar con ctrl+C as per usual
