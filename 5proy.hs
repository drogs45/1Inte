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
bfs cc cv c m = head $ filter (fCondorcet cc cv) $ generar 0 2 [(m,0,1)]

{-|
 - 
 - tripleta: matriz, piso, numero de nodo.
 -}
fCondorcet ::  Integer -> Integer -> ([([Integer],Integer)],Integer,Integer) -> Bool
fCondorcet cc cv (m,_,_) = isJust $ find (\(i,w,z) -> i > cv `div`2) $ fst $ mapAccumL (cont cc) [] m

{-|Funcion que recibe
 - cp = cantidad de personas q apoyan esa preferencia.
 - a = acumulado de tuplas de la cantidad de personas q votaron por un candidato.
 -}
cont :: Integer-> [(Integer,Integer,Integer)]  -> ([Integer],Integer) -> ([(Integer,Integer,Integer)],())
cont cc a (fila,cp) =  (aumenta a $ genTuplas cp fila [],())
  where
    genTuplas :: Integer -> [Integer] -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
    genTuplas cp (x:xs) a
      | xs == [] = a
      | otherwise = genTuplas cp xs (zip3 (repeat cp) (repeat x) (xs))++a
    aumenta :: [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
    aumenta a l
      | l == [] = a
      | otherwise = aumenta (r a (head l)) (tail l)
        where
          r :: [(Integer,Integer,Integer)] -> (Integer,Integer,Integer) -> [(Integer,Integer,Integer)]
          r [] x = [x]
          r (x@(y,t,w):xs) (p,a,w1)
            | t == a  && w == w1= (y+p,a,w):xs
            | otherwise = x: (r xs (p,a,w1))


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
expandir m cc cn ccam = (e, concat l)
  where
    (e,l) = mapAccumL (cambio m) cn [0..cc-2]
{--  
  | ccam == (cc -1) = []
  | otherwise       = (cambio m cn ccam) ++ (expandir m cc (cn+cc) (ccam+1))
--}

{-|Funcion cambia el elemento que le dice ccam por el siguente en todas las filas,
 - produciendo asi una lista de matrices.
 - m = matriz
 - ccam = indice del elemento a realizar el cambio.
 - cn = contador de nodos.
 -}
cambio :: ([([Integer],Integer)],Integer,Integer) -> Integer -> Integer -> (Integer,[([([Integer],Integer)],Integer,Integer)])
cambio (m,pisos,_) cn ccam = mapAccumL (genMatriz m ccam pisos) cn m

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
  | s == 0    = xs
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
      | otherwise = e1:genFilaN (x+1) y (e1:es)

