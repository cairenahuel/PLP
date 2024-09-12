f::Integer->Integer
f x | x==1 = 8
    | x==4 = 131
    | x==16 = 16

g::Integer->Integer
g x | x==8 = 16
    | x==16 = 4
    | x==131 = 1

-- h = f o g
h::Integer->Integer
h x = f (g x)
-- k = g o f
k x = g (f x)

absoluto:: Integer->Integer
absoluto x  |   x>=0 = x
            |   otherwise=(-x)

maximoAbsoluto:: Integer -> Integer -> Integer
maximoAbsoluto x y  |   x>y = absoluto x
                    |   otherwise = absoluto y

maximo:: Integer -> Integer -> Integer
maximo x y  |   x>y = x
                    |   otherwise = y

maximo3:: Integer -> Integer -> Integer -> Integer
maximo3 x y z   | z>maximo x y = z
                | otherwise=maximo x y

algunoEs0:: Integer -> Integer -> Bool
algunoEs0 0 _ = True
algunoEs0 _ 0 = True
algunoEs0 _ _ = False

ambosSon0:: Integer -> Integer -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _ = False

mismoIntervalo:: (RealFloat a) => a -> a -> Bool
mismoIntervalo x y  |   x<=3 && y<=3 = True
                    |   x>3 && y>3 && x<=7 && y<=7 = True
                    |   x>7 && y>7 = True
                    |   otherwise=False
                    -- No soporta decimales

sumaDistintos:: Integer->Integer->Integer->Integer
sumaDistintos x y z |   x==y && y==z = 0
                    |   x==y = z
                    |   y==z = x
                    |   x==z = y
                    |   otherwise=x+y+z

esMultiploDe:: Integer -> Integer -> Bool
esMultiploDe x y = x `mod` y == 0

digitoUnidades:: Integer->Integer
digitoUnidades x = x `mod` 10

digitoDecenas:: Integer->Integer
digitoDecenas x = x `div` 10 `mod` 10

estanRelacionados:: Integer->Integer->Bool
estanRelacionados x y = x `mod` y == 0
--a*a +(a*b*k) <=>a+b*k = 0 <=> a=-(b*k)

prodInt:: (RealFloat a)=> (a,a)->(a,a)->a
prodInt (x,y) (z,a) = x*z+y*a
--No se como hacerlo general

todoMenor::(RealFloat a)=> (a,a)->(a,a)->Bool
todoMenor (x,y) (a,b) = x<a && y<b

distanciaPuntos::(RealFloat a)=> (a,a)->(a,a)->a
distanciaPuntos (x,y) (a,b) = sqrt ((x-a)*(x-a)+(y-b)*(y-b))

sumaTerna::  (Integer,Integer,Integer)-> Integer
sumaTerna (x,y,z)=x+y+z

sumarMultiplos:: (Integer,Integer,Integer)-> Integer ->Integer
sumarMultiplos (x,y,z) k = siEsMultiploSuma x k + siEsMultiploSuma y k + siEsMultiploSuma z k 

siEsMultiploSuma :: Integer -> Integer -> Integer
siEsMultiploSuma x y
  | x `mod` y == 0 = x
  | otherwise = 0

posPrimerPar:: (Integer,Integer,Integer)-> Integer
posPrimerPar (x,y,z)
    | x `mod` 2 == 0 = 1
    | y `mod` 2 == 0 = 2
    | z `mod` 2 == 0 = 3
    | otherwise=4

crearPar::a->b->(a,b)
crearPar x y = (x,y)

invertirPar:: (a,b)->(b,a)
invertirPar (x,y)=(y,x)

todosMenores::(Integer,Integer,Integer)-> Bool
todosMenores (x,y,z) = desigualdad x && desigualdad y && desigualdad z
    where desigualdad a = f' a > g' a

f'::Integer->Integer
f' x | x<=7 = x*x
     | x>7 = 2*x - 1

g'::Integer->Integer
g' x
    | x `mod` 2 == 0 = x `div` 2
    | otherwise = 3*x + 1

bisiesto:: Integer->Bool
bisiesto año = not ((not (año `mod` 4 == 0)) || (año `mod` 100 == 0 && not (año`mod`400==0)))

distanciaManhattan::(Float,Float,Float)->(Float,Float,Float)->Float
distanciaManhattan (x,y,z) (a,b,c)=abs (x - a)+ abs (y - b) + abs (z - c)

sumaUltimosDosDigitos:: Integer->Integer
sumaUltimosDosDigitos x = digitoUnidades x + digitoDecenas x

comparar:: Integer->Integer->Integer
comparar x y 
    | sumaUltimosDosDigitos x > sumaUltimosDosDigitos y = -1
    | sumaUltimosDosDigitos x < sumaUltimosDosDigitos y = 1
    | sumaUltimosDosDigitos x == sumaUltimosDosDigitos y = 0

