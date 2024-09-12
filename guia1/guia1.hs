sumaAlt :: (Num a) => [a] -> a
sumaAlt = foldr (-) 0

-- por que????
{-
por esto :)

sumaAlt [1,2,3,4] ~~> 1-2+3-4
x = 1
rec = sumaAlt [2,3,4] ~~> 2-3+4

f =? (-)

-}

recr :: (t1 -> [t1] -> t2 -> t2) -> t2 -> [t1] -> t2
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sumasParciales :: (Num a) => [a] -> [a]
{-
Para verla:
sumasParciales [1,2,3]
x=1
rec=[3,4]
-}

sumasParciales = foldr (\x rec -> x : sumarX x rec) []

sumarX :: (Num a) => a -> [a] -> [a]
sumarX x = map (+ x)

-- sumasParciales = foldl (\ac -> (:)) 0 :[]

sumaAltAlt :: (Num a) => [a] -> a
-- sumaAltAlt = foldl (flip (-)) 0

-- Esta mal sumaAltAlt = (\x-> sumaAlt (reverse x)) ?
-- Reverse se puede escribir como un foldr
---- No esta mal pero con foldl (flip (-)) 0 haces el calculo en "una pasada", 
---- con reverse sumaAlt primero haces una pasada para darlo vuelta y despues
---- otra con sumaAlt

{-
Reverse en terminos de foldr
[1,2,3] ~~> [3,2,1] =(3:2:1:[])
x=1
rec = rev [2,3] ~> [3,2] = (3:2:[])

f =? rec ++ x:[]
-}

sumaAltAlt = sumaAlt . foldr (\x rec -> rec ++ [x]) []

{-

permutaciones [1,2,3] ~~>  [[1,2,3],[1,3,2],
                            [2,1,3],[3,1,2]
                            [2,3,1],[3,2,1]]
x=1 rec = permutaciones [2,3] ~~> [[2,3],[3,2]]

a todos los elementos de rec les concateno 1 en cada posicion posible, como?

concatmap me deja aplicarle una funcion a cada elemento de la lista y mantenerlo todo en una
lista, tengo que hacer una funcion dado un elemento y una lista devuelva una lista de listas
ese elemento metido en cada posicion

recorriendo 0 a length(lista) con take X ++ elemento ++ drop X podria, como?

-}

permutaciones :: [a] -> [[a]]
-- permutaciones [] = [[]]
-- permutaciones (x : xs) = concatMap (intercalarElem x) (permutaciones xs)
permutaciones = foldr (concatMap . intercalarElem) [[]]

intercalarElem :: a -> [a] -> [[a]]
intercalarElem e xs = [take i xs ++ [e] ++ drop i xs | i <- [0 .. length xs]]

-- elementosEnPosicionesPares :: [a] -> [a]
-- elementosEnPosicionesPares [] = []
-- elementosEnPosicionesPares (x : xs) =
--   if null xs
    -- then [x]
    -- else x : elementosEnPosicionesPares (tail xs)



-- Ejercicio 6
sacarUna::Eq a=>a->[a]->[a]
--sacarUna _ [] = []
--sacarUna e (x:xs) = if e==x then xs else x:sacarUna e xs

-- No puedo hacerlo con foldr porque no tengo acceso a xs
--sacarUna e = foldr (\(x:xs) rec ->if e==x then xs else x:sacarUna e xs) []

sacarUna e = recr (\x xs rec ->if e==x then xs else x:sacarUna e xs) []

-- jaja despues de hacerlo me di cuenta que literalmente el ejercicio te dice que es con recr

insertarOrdenado :: Ord a=> a -> [a] -> [a]
-- insertarOrdenado e [] = [e]
-- insertarOrdenado e (x:xs) = if x<e then x:insertarOrdenado e xs else e:x:xs
insertarOrdenado e = recr (\x xs rec -> if x<e then x:insertarOrdenado e xs else e:x:xs) []

-- ejercicio 7

genLista:: a->(a->a)->Integer->[a]
genLista _ _ 0 = []
genLista e fSig n = e: genLista (fSig e) fSig (n-1)

-- Como lo hago "lindo"?

desdeHasta:: Integer->Integer->[Integer]
desdeHasta n m= genLista n (+1) (m-n+1)

-- ejercicio 9
sumarMat :: [[Integer]] -> [[Integer]] -> [[Integer]]
sumarMat _ [] = []
sumarMat [] _ = []
sumarMat (x:xs) (y:ys) = zipWith (+) x y : sumarMat xs ys

trasponer::[[a]]->[[a]]
trasponer (x:xs) = [concatMap (take 1.drop i) (x:xs)   | i<-[0..length x-1]]

-- ejercicio 10
