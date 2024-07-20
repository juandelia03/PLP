--ejercicio 2
miCurry:: ((a,b)->c) -> a -> b -> c
miCurry f x y = f (x,y)

miUncurry::  (a->b->c) -> (a,b) -> c
miUncurry f (x,y) = f x y


-- foldr agarra primero el elto y despues el acumulador foldl primero el acc y despues el elto

--ejercicio 3
suma xs = foldr (+) 0 xs

elemm n (xs) = foldr (\x acc -> if x == n then True else acc) False (xs)

mas_mas xs ys = foldr (:) ys xs

filterr c xs = foldr (\x acc -> if c x then x:acc else acc) [] xs

mapp f xs = foldr (\x acc -> (f x):acc) [] xs

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun c xs = foldr1 (\x y -> if c x y then x else y) xs 

sumasParciales :: Num a => [a] -> [a]
sumasParciales (x:xs) = foldl (\acc elto -> acc ++ [last acc + elto]) [x] xs

--se van cancelando los signos 
sumaAlt:: [Int] -> Int
sumaAlt xs = foldr (-) 0 xs
sumaAltReverso:: [Int] -> Int
sumaAltReverso xs = foldr (-) 0 (reverse xs)

--ejercicio 4

--partes
agregarATodas:: a -> [[a]] -> [[a]]
agregarATodas n xs = map (\l -> n:l) xs
partes:: [a] ->[[a]]
partes xs = foldr (\x acc -> acc ++ (agregarATodas x acc)) [[]] xs

--prefijos
prefijos:: [a] -> [[a]]
prefijos xs = foldl (\acc elem ->  acc ++ [(last acc ++ [elem])]  )  [[]] xs

--Ejercicio 5
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares [x] = [x]
elementosEnPosicionesPares (x:xs) =  x : elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
then x : entrelazar xs []
else x : head ys : entrelazar xs (tail ys)



--elementos en Posiciones pares no es estructural xq no uso la cola (uso la cola de la cola),lo que si
--solo utilizo la cabeza para operar y llamo a la recursion usando la cola 

mientrelazar xs  = foldr (\x acc -> \ys -> if null ys then x:acc [] else x:head ys:acc (tail ys)) id xs 

--ejercicio 6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--si no llamo a r corta la recursion
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e xs = recr (\x lista r-> if x==e then lista else x:r) [] xs

--b)
-- con foldr si uso if else , cada vez que encuentre un elto == e lo voy a sacar 
-- Al no sabes si ya lo saque o no se hace imposible sacar unicamente la primera aparicion

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e xs =
 recr (\x lista r -> if lista == [] then (min x e):(max x e):lista  else (if (e<=x) then e:x:lista else x:r) ) [] xs


mapPares:: (a->b->c) -> [(a,b)] -> [c]
mapPares f xs = map (uncurry f) xs

armarPares :: [a] -> [b] -> [(a,b)]
armarPares xs [] = []
armarPares [] ys = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys

mapDoble :: (a->b->c) -> [a] -> [b] -> [c] 
mapDoble f xs ys =  mapPares f (armar Pares xs ys)

--ejercicio 11
foldNat:: (Int -> a -> a) -> a -> Int -> a
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n-1))


-- n^m
potencia n m = foldNat (\x acc -> acc*n) n (m-1)  


--13
--AB a es un arbol de tipo a, Puede ser Nil -> arbol vacio, o Bin donde (AB a) es el subarbol izq, a el nodo y 
--(AB a) el subarbol derecho
data AB a = Nil | Bin (AB a) a (AB a)


foldAB f z Nil = z
foldAB f z (Bin i r d) = f r (foldAB f z i) (foldAB f z d)

--es lo mismo q lo de abajox`
-- esNil Nil = True
-- esNil (Bin _ _ _) = False

esNil a =
    case a of
        Nil -> True
        Bin _ _ _ -> False

altura arbol = foldAB (\_ recI recD -> 1 +  max recI recD) 0 arbol
nodos arbol = foldAB (\_ recI recD -> 1 + recI + recD) 0 arbol


raiz (Bin i v d) = v
izq:: AB a -> AB a
izq (Bin i v d) = i
der:: AB a -> AB a
der (Bin i v d) = d

mismaEstructura:: AB a -> AB a -> Bool
mismaEstructura = foldAB (\v ri rd -> (\arbol -> if esNil arbol then False else ((ri (izq arbol))&&(rd (der arbol))))) 
                        (\arbol -> esNil arbol)


--ejercicio 15
--le pongo binn xq sino crashea
data AIH a = Hoja a | Binn (AIH a) (AIH a) 

foldAIH :: (a->b) -> (b->b->b) -> AIH a -> b
foldAIH fHoja fBin arbol = case arbol of 
    Hoja x ->  fHoja x
    (Binn i d) -> fBin (rec i) (rec d)
    where rec = foldAIH fHoja fBin

altura2 :: AIH a -> Integer
altura2 = foldAIH (const 1) (\ri rd -> 1 + max ri rd)

size = foldAIH (const 1) (\ri rd -> ri+rd)






