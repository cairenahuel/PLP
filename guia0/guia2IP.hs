--Recursion sobre enteros

fibonacci:: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci(x-2)

parteEntera:: Float -> Integer
parteEntera x 
    | x>(-1) && x<1 = 0
    | x>=1 = 1 + parteEntera (x-1)
    | otherwise=(-1) + parteEntera (x+1)


esDivisible::Integer->Integer->Bool
esDivisible x y = dividirEnteros (abs x) (abs y) == 0

dividirEnteros::Integer->Integer->Integer
dividirEnteros x y
    | x==0 = 0
    | x < 0 = x
    | otherwise=dividirEnteros (x-y) y

sumaImpares:: Integer-> Integer
sumaImpares x = sumaImparesAux 1 x

sumaImparesAux:: Integer-> Integer -> Integer
sumaImparesAux _ 0 = 0 
sumaImparesAux x restantes = x + sumaImparesAux (x+2) (restantes-1)

medioFact:: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact(x-2)

sumaDigitos:: Integer->Integer
sumaDigitos 0 = 0
sumaDigitos x = x `mod` 10 + sumaDigitos (x `div` 10)

todosDigitosIguales:: Integer -> Bool
todosDigitosIguales x
    | x `div` 10 == 0 && mod x 10 /= 0 = True
    | x `div` 100 == 0 = x `mod` 10 == mod (div x 10) 10
    | otherwise=x `mod` 10 == mod (div x 10) 10 && todosDigitosIguales (x `div` 10)

iesimoDigito::Integer -> Integer -> Integer
-- Esto estaria bien si fueran numeros binarios jaja
-- iesimoDigito x i
--     |   i==1 = mod x 10
--     |   otherwise=iesimoDigito (div x 10) (i-1)
iesimoDigito x i = mod (div x (10^((cantidadDeDigitos x)-i))) 10

cantidadDeDigitos:: Integer -> Integer
cantidadDeDigitos x 
    |   x == 0 = 0
    |   otherwise=1+cantidadDeDigitos (div x 10)

ultimoDigito:: Integer -> Integer
ultimoDigito x = iesimoDigito x (cantidadDeDigitos x)

esCapicua:: Integer -> Bool
esCapicua x
    | x < 10 = True
    | otherwise= iesimoDigito x 1 == ultimoDigito x && esCapicua xReducido
    where   xReducido= div sinElPrimero 10
            sinElPrimero = (x - iesimoDigito x 1 * 10^((cantidadDeDigitos x)-1))
-- dosdigitos-> 10*

sumaIesimosPoderesDeDos:: Integer -> Integer
sumaIesimosPoderesDeDos 0 = 1
sumaIesimosPoderesDeDos x = 2^x + sumaIesimosPoderesDeDos (x-1)

sumaEnesimosPoderesDeQ::Integer->Integer->Integer
sumaEnesimosPoderesDeQ 0 q = 0
sumaEnesimosPoderesDeQ n q = q^n + sumaEnesimosPoderesDeQ (n-1) q  

sumaEnesimosPoderesDeQ2::Integer->Integer->Integer
sumaEnesimosPoderesDeQ2 n q = sumaEnesimosPoderesDeQ (2*n) q 

sumaEnesimosPoderesDeQ3::Integer->Integer->Integer
sumaEnesimosPoderesDeQ3 n q = sumaEnesimosPoderesDeQ (2*n) q - sumaEnesimosPoderesDeQ (n-1) q

factorial:: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

eAprox::Integer->Float
eAprox 1 = 1
eAprox x = 1/fromIntegral (factorial (x)) + eAprox (x-1)

e::Float
e = eAprox 10

raizDe2Aprox::Integer->Float
raizDe2Aprox x = raizDe2AproxSucc x - 1

raizDe2AproxSucc::Integer->Float
raizDe2AproxSucc 1 = 2
raizDe2AproxSucc x = 2 + 1 / raizDe2AproxSucc (x-1)

dobleSum::Integer->Integer->Integer
dobleSum 1 j = dobleSumAux 1 j
dobleSum i j = dobleSumAux i j + dobleSum (i-1) j

dobleSumAux::Integer->Integer->Integer
dobleSumAux i 1 = i
dobleSumAux i j = i^j + dobleSumAux i (j-1)

sumaPotencias::Integer->Integer->Integer->Integer
sumaPotencias q n 1 = sumaPotenciasAux q n 1
sumaPotencias q n m = sumaPotencias q n (m-1) + sumaPotenciasAux q n m 

sumaPotenciasAux::Integer->Integer->Integer->Integer
sumaPotenciasAux q 1 m = q^(1+m)
sumaPotenciasAux q n m = q^(n+m)+sumaPotenciasAux q (n-1) m

sumaRacionales::Integer ->Integer->Float
sumaRacionales q 1 = sumaRacionalesAux q 1
sumaRacionales q p = sumaRacionales q (p-1) + sumaRacionalesAux q p

sumaRacionalesAux::Integer->Integer->Float
sumaRacionalesAux 1 p = fromIntegral p
sumaRacionalesAux q p = fromIntegral p / fromIntegral q + sumaRacionalesAux (q-1) p

--Estan invertidos p y q

menorDivisor:: Integer->Integer
menorDivisor x= menorDivisorAux x 2

menorDivisorAux:: Integer->Integer->Integer
menorDivisorAux a b
    |   (fromIntegral a)/fromIntegral 2 < fromIntegral b = a
    |   mod a b == 0 = b
    |otherwise=menorDivisorAux a (b+1)

esPrimo:: Integer->Bool
esPrimo x = x == menorDivisor x

sonComprimos:: Integer->Integer->Bool
sonComprimos a b
    |   mod a (menorDivisor b)==0 = False
    |   mod a (menorDivisor b)/=0 && not (esPrimo b) = sonComprimos a (round (fromIntegral b/fromIntegral (menorDivisor b)))
    |   otherwise = True 

nEsimoPrimo::Integer->Integer
nEsimoPrimo 1 = 2
nEsimoPrimo x = proximoPrimo (nEsimoPrimo (x-1)+1)

proximoPrimo::Integer->Integer
proximoPrimo p
    |   esPrimo p = p
    |   otherwise=proximoPrimo (p+1)

esFibonacci::Integer->Bool
esFibonacci x = esFibonacciAux x 1

esFibonacciAux::Integer->Integer->Bool
esFibonacciAux n i
    |   fibonacci i == n = True
    |   fibonacci i > n = False
    |   fibonacci i < n = esFibonacciAux n (i+1)

mayorDigitoPar:: Integer->Integer
mayorDigitoPar 0 = -1
mayorDigitoPar x
    |   mod x 2 == 0 = max (mod x 10) (mayorDigitoPar (div x 10))
    |   otherwise = max (-1) (mayorDigitoPar (div x 10))

sumaInicialDePrimos::Integer->Bool
sumaInicialDePrimos x = sumaInicialDePrimosAux x 1

sumaInicialDePrimosAux:: Integer -> Integer -> Bool
sumaInicialDePrimosAux 0 1 = False
sumaInicialDePrimosAux 0 _ = True
sumaInicialDePrimosAux n i
    |   n<2 = False
    |   otherwise = sumaInicialDePrimosAux (n - nEsimoPrimo i) (i+1)

sumaDivisores::Integer->Integer
sumaDivisores n = sumaDivisoresAux n n

sumaDivisoresAux:: Integer->Integer->Integer
sumaDivisoresAux n i
    |   i<1 = 0
    |   mod n i == 0 = i + sumaDivisoresAux n (i-1)
    |   otherwise = sumaDivisoresAux n (i-1)

tomaValorMaxAux::Integer->Integer->Integer->Integer
tomaValorMaxAux n1 n2 m
    |   n1==n2 && sumaDivisores n2 >= sumaDivisores m = n2
    |   n1==n2 && sumaDivisores n2 < sumaDivisores m = m
    |   (sumaDivisores n1)>(sumaDivisores m) = tomaValorMaxAux (n1+1) n2 n1
    |   otherwise = tomaValorMaxAux (n1+1) (n2) m

tomaValorMax::Integer->Integer->Integer
tomaValorMax n1 n2 = tomaValorMaxAux n1 n2 0

--Una re paja el ultimo loko 