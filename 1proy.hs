import Data.List
import Data.Maybe

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
bfs :: Integer -> Integer -> [(Integer,String)] -> [([Integer],Integer)] -> ([([Integer],Integer)],Integer,Integer)
bfs cc cv c m = head $ filter (\(s,_,_) -> fCondorcet cc cv s) $ generar cc 1 [(m,0,1)]

{-|
 - 
 - tripleta: matriz, piso, numero de nodo.
 -}
fCondorcet ::  Integer -> Integer -> [([Integer],Integer)] -> Bool
fCondorcet cc cv m = foldl (\x y -> x || (f cv y)) False $ groupBy verifElem $ sort $ fst $ mapAccumL trans [] m


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
 - c = contador de nodos visitados.
 - x = Matriz
 -}
generar :: Integer -> Integer -> [([([Integer],Integer)],Integer,Integer)] -> [([([Integer],Integer)],Integer,Integer)]
generar cc cn []  = []
generar cc cn (x@(m,pi,c):xs) = x:(generar cc newcn (xs ++ lista ))
--generar cc (x@(m,pi,c):xs) = x:(generar cc (xs ++ (expandir hijo cc 0 0) ))
  where
    hijo = (m,pi,c+1)
    (newcn,lista) = expandir hijo cc cn 0


{-|
 - m =matriz
 - cc = cantidad de candidatos.
 - pisos = contador de pisos.
 - cn = contador de nodos
 - ccam = los nodos q ya han sido cambiados, este valor va desde 0 hasta cc-2.
 - Para saber el numero de nodo que soy es s
 - s = sumatoria desde i=1 hasta n-1 de i
 - donde n es la cantidad de candidatos.
 - La cantidad de hijos de una matriz son s * cantidad de filas.
 -}
expandir :: ([([Integer],Integer)],Integer,Integer) -> Integer -> Integer -> Integer ->(Integer,[([([Integer],Integer)],Integer,Integer)]) 
expandir m@(s,0,_) cc cn ccam =  (e,concat l)
  where
    (e,l) = mapAccumL (cambio m s) cn [0..cc-2]
expandir m@(r,pisos,c) cc cn ccam = (f, concat d)
  where
    (f,d) = mapAccumL (cambio m $ drop (fromIntegral pisos) r ) cn [0..cc-2]
{--  
  | ccam == (cc -1) = []
  | otherwise       = (cambio m cn ccam) ++ (expandir m cc (cn+cc) (ccam+1))
--}

{-|Funcion cambia el elemento que le dice ccam por el siguente en todas las filas,
 - produciendo asi una lista de matrices.
 - m = matriz.
 - l = lista de filas a las que se le va a aplicar el cambio,  ya que hay filas que estan fijas xq si las modifico genero un padre de el.
 - ccam = indice del elemento a realizar el cambio.
 - cn = contador de nodos.
 -}
cambio ::([([Integer],Integer)],Integer,Integer) -> [([Integer],Integer)] -> Integer -> Integer -> (Integer,[([([Integer],Integer)],Integer,Integer)])
cambio (m,pisos,_) l cn ccam = mapAccumL (genMatriz m ccam pisos) cn l

{-|
 - m
 - ccam
 - pisos
 - cn = cantidad de nodos
 - e = fila a q se le van a hacer las modificaciones.
 -}
genMatriz :: [([Integer],Integer)] -> Integer -> Integer -> Integer ->([Integer],Integer) -> (Integer,([([Integer],Integer)],Integer,Integer))
genMatriz m ccam pisos cn e = (cn+1,(modifVotante (verifFila m $ genFila ccam e) e,pisos+1,cn))

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


bfs2 :: Integer -> Integer -> [(Integer,String)] -> [([Integer],Integer)] -> [([([Integer],Integer)],Integer,Integer)]
bfs2 cc cv c m = filter (\(s,_,_) -> fCondorcet cc cv s) $ generar cc 1 [(m,0,1)]


fCondorcet2 :: Integer -> Integer -> ([([Integer] ,Integer)],Integer,Integer) -> [Integer]
fCondorcet2 cc cv (m,_,_) = map (f2 cc) $ groupBy verifElem $ sort $ fst $ mapAccumL trans [] m

f2 ::  Integer -> [(Integer,Integer,Integer)] -> Integer
f2 _ [] = -1
f2 p ((a,b,c):xs)
  | c > p `div` 2 && xs ==[] = a
  | c > p `div` 2 = f2 p xs
  | otherwise = -1

