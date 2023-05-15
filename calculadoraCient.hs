import Data.List

clean raw = map (\s -> read s :: Double) (lines raw)
main = do 
    rawInput <- readFile "input.txt"
    let input = clean rawInput
    print input
    putStrLn $ "1- La longitud es " ++ (show.length) input
    putStrLn $ "2- El valor minimo es " ++ (show.mini) input
    putStrLn $ "3- El primer cuartil es " ++ (show.cuartiluno) input
    putStrLn $ "4- La mediana es " ++ (show.mediana) input
    putStrLn $ "5- El tercer cuartil es " ++ (show.cuartilt) input
    putStrLn $ "6- El valor máximo es " ++ (show.maxi) input
    putStrLn $ "7- La suma total es " ++ (show.sumar) input
    putStrLn $ "8- La varianza es " ++ (show.varianza) input
    putStrLn $ "9- La desviación estándar es " ++ (show.desviacion) input
    putStrLn $ "10- El promedio es " ++ (show.media) input
--Promedio
media xs = (sum xs) / (fromIntegral (length xs))

--Mediana
mediana xs | odd n  = ys !! i
           | even n = media [ys !! (i-1), ys !! i]
    where ys = sort xs
          n  = length xs
          i  = n `div` 2
--Cuartil Uno
cuartiluno :: [Double] -> Double
cuartiluno a = head $ drop (round (fromIntegral (if (length a) `mod` 2 == 0 then ((length a) * 1 `div` 4 - 1) 
else (((length a) + 1) * 1 `div` 4 - 1)))) $ quickshort1 a
quickshort1 [] = []
quickshort1 (x:xs) = quickshort1 small ++ (x : quickshort1 large)
  where small = [y | y <- xs, y <= x]
        large = [y | y <- xs, y > x]
--Cuartil Tres
cuartilt :: [Double] -> Double
cuartilt t = head $ drop  (round (fromIntegral ( if (length t) `mod` 2 == 0 then ((length t) * 3 `div` 4 - 1) else (((length t) + 1) * 3 `div` 4 - 1))))$ quickshort3 t

quickshort3 [] = []
quickshort3 (x:xs) = quickshort3 small ++ (x : quickshort3 large)
  where small = [y | y <- xs, y <= x]
        large = [y | y <- xs, y > x]
--Minimo 
mini :: (Ord a) => [a] -> a
mini [] = error "Lista vacia"
mini [x] = x
mini (x:xs) = min x (mini xs)
--Maximo
maxi :: (Ord a) => [a] -> a
maxi [] = error  "Lista vacia"
maxi [x] = x
maxi (x:xs) = max x (maxi xs)
-- Varianza
varianza xs = media [(x-m)^2 | x <- xs]
    where m = media xs

--Desviacion
desviacion xs = sqrt (varianza xs)

--Suma de los elementos
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

