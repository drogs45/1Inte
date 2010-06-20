import Data.List
import System.Environment

{--
 - Razones por las cuales he llegado a la conclusion que es mejor guardar la matriz:
 - 1ero. para generar los hijos de una matriz, tuviera q aplicarle todos los cambios (construir la matriz)
 - para luego poder generar los hijos de la misma, y si guardo los hijos como tuplas, cuando me toque generar
 - de estos hijos pasara lo mismo.
 - 2do Creo y espero q haskell se la fume y no consuma tanta memoria.
 -}



{-|Funcion que verifica si una matriz tiene algun ganador condorcet.
 - cc = cantidad de candidatos.
 - cv = cantidad de votantes.
 - m = matriz. 
 -}
fCondorcet ::  Integer -> Integer -> [(Integer,Integer)] -> Bool
fCondorcet cc cv m = foldl (\x y -> x || (f cv y)) False $ groupBy verifElem $ sort $ fst $ mapAccumL trans [] $ vectorToMatriz cc m



{-|Funcion que verifica que todos las sumas de un las tuplas sean > n `div` 2
 -}
f ::  Integer -> [(Integer,Integer,Integer)] -> Bool
f _ [] = True
f p ((a,b,c):xs)
  | c > p `div` 2 = f p xs
  | otherwise = False

{-|Funcion que devuelve la lista de fcondorcet de una matriz.
 - cc = cantidad de candidatos.
 - cv = cantidad de votantes.
 - m = matriz. 
 -}
dfCondorcet ::  Integer -> Integer -> [(Integer,Integer)] -> Integer
dfCondorcet cc cv m = fr $ groupBy verifElem $ sort $ fst $ mapAccumL trans [] $ vectorToMatriz cc m
  where
    fr :: [[(Integer,Integer,Integer)]] -> Integer
    fr [] = -1
    fr (x@((y0,y1,y2):ys):xs) 
      | f y0 x = y0
      | otherwise = fr xs


verifElem :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Bool
verifElem (a,_,_) (b,_,_) = a ==b

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
 - cv = cantidad de votantes.
 - ne = nodos expandidos.
 - ng = nodos generados.
 - tu = lista de tuplas de String, Integer, que indica que String representa el Integer.
 - x = nodo a expandir.
 - xs = siguientes nodos.
 - nnv = nuevos nodos visitados al expandir x.
 - hijosx = los hijos de x.
 -}
bfs :: Integer -> Integer -> Integer -> Integer -> [(String,Integer)] -> [[(Integer,Integer)]] -> [([(Integer,Integer)],Integer)] -> (String,Integer,Integer,Integer)
bfs cc cv ne ng tu _ [] = ([],-1,-1,-1)
bfs cc cv ne ng tu nv ((x,p):xs)
  | fCondorcet cc cv x  = (camb tu $ dfCondorcet cc cv x,p,ne,ng)
  | otherwise           = bfs cc cv (ne+1) (ng+nng) tu nnv (xs++hijosx)
  where
    ((nnv,nng),hijosx) = expandir (x,p) cc nv
    camb :: [(String,Integer)] -> Integer -> String
    camb []  _ = "no se encontro la representacion del integer"
    camb ((y,y1):xs) n
      | y1 == n = y
      | otherwise = camb xs n

{-|
 - n = nodo.
 - m = matriz representada por lista de tuplas.
 - pisos = contador de pisos.
 - cc = cantidad de candidatos.
 - nv = nodos visitados.
 -}
expandir :: ([(Integer,Integer)], Integer) -> Integer -> [[(Integer,Integer)]] -> (( [[(Integer,Integer)]] ,Integer ) , [([(Integer,Integer)],Integer)] )
expandir n@(m,pisos) cc nv = man $ mapAccumL (cambio cc n m) (nv,0) [0..cc-2]
  where
    man (nnv,nm) = (nnv,w)
      where
        !w = concat nm


{-|Funcion cambia el elemento que le dice ccam por el siguente en todas las filas,
 - produciendo asi una lista de matrices.
 - cc = cantidad de candidatos.
 - m = matriz.
 - l = lista de filas a las que se le va a aplicar el cambio,  ya que hay filas que estan fijas xq si las modifico genero un padre de el.
 - ccam = indice del elemento a realizar el cambio.
 - cn = contador de nodos.
 -}
cambio :: Integer -> ([(Integer,Integer)], Integer) -> [(Integer,Integer)] -> 
  ([[(Integer,Integer)]],Integer) -> Integer -> (([[(Integer,Integer)]],Integer), [ ([(Integer,Integer)],Integer) ] )
cambio cc (m,pisos) l (nv,ng) ccam = (\(nnv,nm) -> (nnv,limp nm) ) $ mapAccumL (genMatriz cc m ccam pisos) (nv,ng) l
  where
    limp :: [([(Integer,Integer)],Integer)] -> [([(Integer,Integer)],Integer)]
    limp [] = []
    limp (([],_):xs) = limp xs
    limp ((x,p):xs) | x /= [] = (x,p) : limp xs


{-|
 - cc = cantidad de candidatos.
 - n = nodo.
 - nm = matriz representada x lista de tuplas de numeros.
 - m = matriz.
 - pisos
 - ccam
 - pisos
 - nv = nodos visitados.
 - col = columna.
 - p = personas q votaron x esa preferencia.
 -}
genMatriz :: Integer -> [(Integer,Integer)] -> Integer -> Integer -> ([[(Integer,Integer)]],Integer) -> (Integer,Integer) -> (([[(Integer,Integer)]],Integer),([(Integer,Integer)],Integer))
genMatriz cc nm ccam pisos (nv,ng) (col,p) =  fnd nv ng $ matrizToVector cc $ modifVotante (verifFila m $ genFila ccam e) (e,p)
  where
    m = vectorToMatriz cc nm
    e = reverse $ unfolAcumm (cc+1) col
    {-| Funcion que busca la ocurrencia de la nueva matriz generada en los nodos visitados.
     - De no haber visitado previamente la matriz, se mete al principio de los nodos visitados.
     - nv = nodos visitados.
     - rnm = matriz representada x listas de tuplas de numeros.
     - pisos
     -}
    fnd :: [[(Integer,Integer)]] -> Integer -> [(Integer,Integer)] -> ( ([[(Integer,Integer)]],Integer) , ([(Integer,Integer)],Integer) )
    fnd nv ng rnm 
      | elem rnm nv = ((nv,ng+1),([],5500))
      | otherwise = (((rnm:nv),ng+1),(rnm,r))
      where
        !r = pisos + 1

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
genFila :: Integer ->  [Integer] -> ([Integer],Integer)
genFila y fila = (genFilaN 0 y fila , 1)
  where 
    genFilaN :: Integer -> Integer  -> [Integer] -> [Integer]
    genFilaN _ _ [] = []
    genFilaN x y (e0:e1:es)
      | x == y = e1:e0:es
      | otherwise = e0:genFilaN (x+1) y (e1:es)


matrizToVector :: Integer -> [([Integer],Integer)] -> [(Integer,Integer)]
matrizToVector cc m = map (\(e,t) -> ((fst $foldl (foldAcumm (cc+1)) (0,cc) e) `div` (cc+1),t)) m

{-|Funcion que pasa una columna a un numero.
 - cc = cantidad de candidatos.
 - a = suma acumulada.
 - e = potencia a la q hay q elevar la cc.
 - n = el nuevo numero q se va a multiplicar por esa potencia.
 - La potencia (e) tiene q acumularse, ya que cada vez se eleva a la anterior menos uno.
 -}
foldAcumm :: Integer -> (Integer,Integer) -> Integer -> (Integer,Integer)
foldAcumm cc (a,e) n = (a+(n*(cc^e)),e-1)

vectorToMatriz :: Integer -> [(Integer,Integer)] -> [([Integer],Integer)]
vectorToMatriz cc = map (\(e,t) -> (reverse (unfolAcumm (cc+1) e),t)) 


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

names :: Integer -> [String] -> [(String,Integer)]
names _ [] = []
names n (x:xs) = (x,n) : names (n+1) xs

matriz :: [(String,Integer)] -> Int -> [String] -> [([Integer],Integer)] -> Integer -> ([([Integer],Integer)],Integer)
matriz nam _ [] a b = (a,b)
matriz nam n (x:xs) a b = matriz nam n seg ((camb nam prim,read x):a) (b+(read x))
 where
    (prim,seg) = splitAt n xs
    camb :: [(String,Integer)] -> [String] -> [Integer]
    camb _ [] = []
    camb nam (x1:xs1) = r nam x1 : camb nam xs1
      where
      r :: [(String,Integer)] -> String -> Integer
      r ((x,y):xs) z
        | x == z    = y
        | otherwise = r xs z
{--
put :: (String,Integer,Integer,Integer) -> String
put (g,p,ne,ng)
--}
{--
main = do
  a <-getArgs
  b <-readFile (last a)
  let c = drop 1 $ concatMap words $ lines b
--  let cc = read $ head c
  let nam = names 1 $  take (read $ head c) (tail c)
  let d = drop (1 + (read $ head c)) (tail c)
  let (mat,cv) = matriz nam (read $ head c) d [] 0
 -- print $ matriz nam (read $ head c) d [] 0
  case (head a) of
    "-ida" -> print "ida"
    "-bfs" -> 
      print $ bfs (read $ head c) cv 0 1 nam [matrizToVector (read $ head c) mat] [(matrizToVector (read $ head c) mat,0)]
--      let (m,ce,ne,ng) = bfs (read $ head c) cv 0 0 [matrizToVector (read $ head c) mat] [(matrizToVector (read $ head c) mat,0)]
--    print m
--    print "Num cambios elementales: "++ce
--    print "Nodos generados: "++ng
--    print "Nodos expandidos: "++ne
    otherwise -> print "Error: argumento invalido."
--}
