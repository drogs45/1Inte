import Data.List
import Data.Maybe
import System.Environment

{--
 - Razones por las cuales he llegado a la conclusion que es mejor guardar la matriz:
 - 1ero. para generar los hijos de una matriz, tuviera q aplicarle todos los cambios (construir la matriz)
 - para luego poder generar los hijos de la misma, y si guardo los hijos como tuplas, cuando me toque generar
 - de estos hijos pasara lo mismo.
 - 2do Creo y espero q haskell se la fume y no consuma tanta memoria.
 -}

{-|
 - cc = cantidad de Candidatos
 - cv = cantidad de votantes
 - c  = Tuplas mapea de numero a nombre de candidatos
 - m  = matriz
 - creo q esto se puede hacer con algo como mapAccumL para hacer el generar mas eficiente, ya que 
 - de esta manera el generar tiene q esperar expandir TODOS los hijos del nodo actual para poder seguir.
 -}
{--
bfs :: Integer -> Integer -> [(Integer,String)] -> ([(Integer,Integer)],Integer) -> ([([Integer],Integer)],Integer,Integer)
bfs cc cv c m = fil ( fCondorcet cc cv ) 0 cc $ generar cc 1 [] [(matrizToVector cc m,1)]
--}


{-| Funcion q filtra el primer elemento condorcet, y devuelve una tripleta q tiene la matriz q gano, el piso y nodo en el q se encontro
 - pred = funcion a aplicar al elemento.
 - a = acumulador de nodos contados.
 - cc = cantidad de candidatos.
 - x = matriz a revisar.
 - y = piso en el q se encuentra la matriz a revisar.
 - xs = matriz siguientes.
 -}
fil :: ([(Integer,Integer)] -> Bool) -> Integer -> Integer-> [([(Integer,Integer)],Integer)] -> ([([Integer],Integer)],Integer,Integer)
fil _pred a cc []   = ([],0,a)
fil pred a cc ((x,y):xs)
  | pred x         = (vectorToMatriz cc x ,y,a) 
  | otherwise      = fil pred (a+1) cc xs

{-|
 - tripleta: matriz, piso, numero de nodo.
 -}
fCondorcet ::  Integer -> Integer -> [(Integer,Integer)] -> Bool
fCondorcet cc cv m = foldl (\x y -> x || (f cv y)) False $ groupBy verifElem $ sort $ fst $ mapAccumL trans [] $ vectorToMatriz cc m


verifElem :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Bool
verifElem (a,_,_) (b,_,_) = a ==b
{-|Funcion que verifica que todos las sumas de un las tuplas sean > n `div` 2
 -}
f ::  Integer -> [(Integer,Integer,Integer)] -> Bool
f _ [] = True
f p ((a,b,c):xs)
  | c > p `div` 2 = f p xs
  | otherwise = False


trans :: [(Integer,Integer,Integer)] -> ([Integer],Integer) -> ([(Integer,Integer,Integer)],())
trans  a (l,p)= (add l p a,())
{-|Genera todas las tripletas de una columna completa.
 - primero: Columna.
 - segundo: peso de la columna.
 - tercero: lista de tripletas acumuladas.
 -}
add :: [Integer] -> Integer -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
add [y] p a = a
add (x:y:xs) p a = add (y:xs) p (fst $ mapAccumL (fnd x p) a (y:xs))
{-|Funcion que recibe un numero (x), una lista de numeros (ls) y el peso (p)y la lista de tripletas q se a acumulado,
 -  y lo que hace es genera todas las tuplas de x con cada uno de lso numeros de la lista ls con el peso p.
 -  Si la tupla ya se encuentra el suma el peso (p) al peso anterior.
 -  x = primer elemento de la tripleta.
 -  ls = lista de segundos elementos de las tripletas.
 -  p = peso.
 -}
fnd :: Integer -> Integer -> [(Integer,Integer,Integer)] -> Integer -> ([(Integer,Integer,Integer)],())
fnd x p [] y = ([(x,y,p)],())
fnd x p (l@(x0,y0,p0):xs) y
  | x == x0 && y == y0 = ((x0,y0,m):xs,())
  | otherwise = (l : (fst $ fnd x p xs y) ,())
  where
    !m = p+p0

{-|
 - cc = cantidad de candidatos.
 - La tripleta tiene en el primer elemento el vector que representa a la la matriz, en el segundo elemento
 - el piso en el q esta ese nodo, y en el tercer elemento la lista de tuplas, que en el primer elemento
 - tiene un numero q representa una columna y en el segundo elemento la lista de movimientos que no se le 
 - pueden hacer a esa columna para no repetir nodos.
 -}
generar :: Integer -> [([(Integer,Integer,[Integer])],Integer)] -> [([(Integer,Integer,[Integer])],Integer)]
generar cc []  = []
--generar cc (x@(m,pi,c):xs) = x:(generar cc newcn (xs ++ lista ))
generar cc (x:xs) = x:(generar cc (xs ++ (expandir x cc) ))
{--
  where
    hijo = (m,pi)
    (newcn,lista) = expandir hijo cc 0
--}

{-|
 - n = nodo.
 - m = matriz representada por lista de tuplas.
 - pisos = contador de pisos.
 - a = acumulador de cambios prohibidos.
 - cc = cantidad de candidatos.
 -}
expandir :: ( [(Integer,Integer,[Integer])] , Integer ) -> Integer -> [ ( [(Integer,Integer)] , Integer, [(Integer,Integer)] )
expandir n@(m,pisos) cc = concatMap (cambio n m) [0..cc-2]


{-|Funcion cambia el elemento que le dice ccam por el siguente en todas las filas,
 - produciendo asi una lista de matrices.
 - m = matriz.
 - l = lista de filas a las que se le va a aplicar el cambio,  ya que hay filas que estan fijas xq si las modifico genero un padre de el.
 - ccam = indice del elemento a realizar el cambio.
 - cn = contador de nodos.
 -}
cambio :: ( [(Integer,Integer,[Integer])] , Integer ) -> [(Integer,Integer,[Integer])] -> Integer -> [( [(Integer,Integer,[Integer])] , Integer )]
cambio (m,pisos) l ccam = map (genMatriz m ccam pisos) l

{-|
 - m = matriz.
 - ccam
 - pisos
 - c = valor de la columna que representa.
 - p = personas q votaron x esa preferencia.
 - a = lista de columnas prohibidas.
 - e = fila a q se le van a hacer las modificaciones.
 -}
genMatriz :: ( [(Integer,Integer,[Integer])],Integer ) -> Integer -> Integer ->(Integer,Integer,[Integer]) -> ([(Integer,Integer,[Integer]] ,Integer)
genMatriz m ccam pisos (c,p,a) = (modifVotante (verifFila m $ genFila ccam e) e,pisos+1)

{-|
 - x = elemento actual de la lista.
 - m = fila actual.
 - s = cantidad de votantes que tienen esa preferencia.
 - xs = siguientes elementos.
 - e = fila buscada.
 - Esta funcion busca la fila a la cual se le va a realizar el cambio del votante,
 - si solo hay un votante elimina esa preferencia, de lo contrario, le disminuye en 1
 - la cantidad de votantes de la preferencia.
 -}
modifVotante :: [([Integer],Integer)] -> ([Integer],Integer) -> [([Integer],Integer)]
modifVotante [] _ = []
modifVotante (x@(m,s):xs) e
  | x /= e    = x: (modifVotante xs e)
  | s == 1    = xs
  | otherwise = (m,s-1):xs

{-|Funcion que verifica si la fila creada se encuentra en la matriz.
 - m = matriz.
 - f = fila.
 -}
verifFila :: [([Integer],Integer)] -> ([Integer],Integer) -> [([Integer],Integer)]
verifFila [] f = [f]
verifFila (x@(fila,p):xs) f@(fila1,_)
  | fila == fila1 = (fila,p+1):xs
  | otherwise     = x:(verifFila xs f)
  where 

{-| Funcion que genera una fila nueva.
 - y = el inidice del elemento que queremos cambiar.
 -}
genFila :: Integer ->  ([Integer],Integer) -> ([Integer],Integer)
genFila y (fila,_) = (genFilaN 0 y fila , 1)
  where 
    genFilaN :: Integer -> Integer  -> [Integer] -> [Integer]
    genFilaN _ _ [] = []
    genFilaN x y (e0:e1:es)
      | x == y = e1:e0:es
      | otherwise = e0:genFilaN (x+1) y (e1:es)


matrizToVector :: Integer -> [([Integer],Integer)] -> [(Integer,Integer,[Integer])]
matrizToVector cc m = map (\(e,t) -> ((fst $foldl (foldAcumm (cc+1)) (0,cc) e) `div` (cc+1),t,[])) m

{-|Funcion que pasa una columna a un numero.
 - cc = cantidad de candidatos.
 - a = suma acumulada.
 - e = potencia a la q hay q elevar la cc.
 - n = el nuevo numero q se va a multiplicar por esa potencia.
 - La potencia (e) tiene q acumularse, ya que cada vez se eleva a la anterior menos uno.
 -}
foldAcumm :: Integer -> (Integer,Integer) -> Integer -> (Integer,Integer)
foldAcumm cc (a,e) n = (a+(n*(cc^e)),e-1)

vectorToMatriz :: Integer -> [(Integer,Integer,[Integer])] -> [([Integer],Integer)]
vectorToMatriz cc = map (\(e,t,_) -> (reverse (unfolAcumm (cc+1) e),t)) 


unfolAcumm :: Integer -> Integer -> [Integer]
unfolAcumm a 0 = []
unfolAcumm a x = x `mod` a : unfolAcumm a (x `div` a)

{--
bfs2 :: Integer -> Integer -> [(Integer,String)] -> [([Integer],Integer)] -> [([([Integer],Integer)],Integer,Integer)]
bfs2 cc cv c m = filter (\(s,_,_) -> fCondorcet cc cv s) $ generar cc 1 [(m,0,1)]


fCondorcet2 :: Integer -> Integer -> ([([Integer] ,Integer)],Integer,Integer) -> [Integer]
fCondorcet2 cc cv (m,_,_) = map (f2 cc) $ groupBy verifElem $ sort $ fst $ mapAccumL trans [] m
--}
f2 ::  Integer -> [(Integer,Integer,Integer)] -> Integer
f2 _ [] = -1
f2 p ((a,b,c):xs)
  | c > p `div` 2 && xs ==[] = a
  | c > p `div` 2 = f2 p xs
  | otherwise = -1

names :: Integer -> [String] -> [(Integer,String)]
names _ [] = []
names n (x:xs) = (n,x) : names (n+1) xs
{--
matriz :: Integer -> [(Integer),Integer]
matriz 

main = do
  a <-getArgs
  b <-readFile (head a)
  let c = drop 1 $ concatMap words $ lines b
  print $ names 1 $  take (read $ head c) (tail c)
  let d = drop (read $ head c) (tail c)
--  matriz (read $ head d) $ tail d
  print d
--}





