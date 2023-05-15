import Data.List
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
 where foldingFunction (x:y:ys) "*" = (x*y) : ys
       foldingFunction (x:y:ys) "+" = (x+y) : ys
       foldingFunction (x:y:ys) "-" = (y-x) : ys
       foldingFunction (x:y:ys) "/" = (y/x) : ys
       foldingFunction (x:y:ys) "^" = (y**x) : ys
       foldingFunction (x:xs) "ln" = log x:xs
--Funciones agregadas del prelude
       foldingFunction (x:xs) "length" = (sum[1|_<-xs]) : xs
       foldingFunction (x:y:ys) "min"  = (if x < y then x else y) : ys
       foldingFunction (x:y:ys) "max"  = (if x >= y then x else y) : ys
       foldingFunction xs "sum" = [sum xs]
       foldingFunction xs "minimum" = [minimum xs]
       foldingFunction xs "maximum" = [maximum xs]
       foldingFunction xs "segundo" = [xs !! 1]
       foldingFunction (x:xs) "sqrt" = sqrt x:xs
       foldingFunction xs "head" = [head xs]
       foldingFunction xs "last" = [last xs]
--Funciones agregadas propias
       foldingFunction xs "promedio" = [media xs]
       foldingFunction xs "mediana" = [mediana xs]
       foldingFunction (x:xs) "factorial" = factorial x:xs
       foldingFunction (x:xs) "doubleMe" = doubleMe x:xs
       foldingFunction (x:y:ys) "doubleUs" = (doubleUs x y) : ys
       foldingFunction (x:xs) "sumaLista" = [sumar xs]
       foldingFunction (x:xs) "circumference" = circumference x:xs
       foldingFunction (x:xs) "varianza" = [varianza xs]
       foldingFunction (x:xs) "Desviacion" = [desviacion xs]
       foldingFunction (x:xs) "doubleSmallNumber" = doubleSmallNumber x:xs
       
       foldingFunction xs numberString = read numberString : xs



     --Promedio
media xs = (sum xs) / (fromIntegral (length xs))

     --Mediana
mediana xs | odd n  = ys !! i
           | even n = media [ys !! (i-1), ys !! i]
    where ys = sort xs
          n  = length xs
          i  = n `div` 2 

factorial x = if x == 0 then 1 else x * factorial (x-1)

doubleMe x = x+x

doubleUs x y = x*2 + y*2

circumference r = 2 * pi * r

-- Varianza
varianza xs = media [(x-m)^2 | x <- xs]
    where m = media xs

--Desviacion
desviacion xs = sqrt (varianza xs)

--Suma de los elementos
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

doubleSmallNumber :: Float -> Float
doubleSmallNumber x = if x > 100 then x else x * 2

