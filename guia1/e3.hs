sumDIY::[Float] -> Float
sumDIY xs = foldr (+) 0 xs

elemDIY::Eq a => a -> [a] -> Bool
elemDIY e xs = foldr (\x acc -> (x==e) ||acc) False xs

plusplus:: [Char]->[Char]->[Char]
plusplus xs ys = foldr (:) ys xs

filtro:: (a->Bool)->[a]->[a]
filtro p xs = foldr (\x acc -> if not (p x) then x:acc else acc) [] xs

mapa:: (a->b)->[a]->[b]
mapa f xs = foldr (\y cola -> f y : cola) [] xs

mejorSegun:: (a->a->Bool)->[a]->a
mejorSegun f (x:lista) = foldr1 ()