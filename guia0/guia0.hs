valorAbsoluto::Float->Float
valorAbsoluto x
    |   x<0=(-x)
    |   otherwise = x

bisiesto::Int->Bool
bisiesto x
    |   mod x 400==0 = True
    |   otherwise= mod x 4==0 && mod x 100 /=0

factorial::Int->Int
factorial 1=1
factorial x = x * factorial (x-1)

inverso:: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero:: Either Int Bool -> Int
aEntero x =  case x of 
                (Left n) -> n
                (Right b) -> if b then 0 else 1 

limpiar:: String -> String -> String
limpiar [] str = str 
limpiar (x:xs) str = limpiar xs (sacarCaracter x str)  

sacarCaracter:: Char -> String -> String
sacarCaracter _ [] = []
sacarCaracter c (x:xs)
    |   c == x = sacarCaracter c xs
    |   otherwise = x :  sacarCaracter c xs

promedio:: [Float] -> Float 
promedio x = suma x / fromIntegral (length x)

suma:: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

difPromedio:: [Float]->[Float]
difPromedio xs = restar xs (promedio xs)

restar:: [Float]->Float->[Float]
restar [] _ = []
restar (x:xs) r = x - r : restar xs r

todosIguales:: [Int]->Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:xs) = x==y && todosIguales (y:xs)

data AB a = Nil | Bin (AB a) a (AB a)

