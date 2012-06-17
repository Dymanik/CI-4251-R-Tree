{-|
  /Hilbert R-Tree/

  Programación Funcional Avanzada (CI4251)

  0.1 2012-06-14


  Johan González	07-40979
  
  Andreina García	08-10406


  Este módulo implanta una estructura de datos que almacena y consulta 
  datos geométricos de rectángulos, dicha estructura es un R-Tree de 
  Hilbert.
 -}


module RTree (
	-- * Tipos exportados.
		-- ** Rectángulos.
	Rectangle (..),
		-- ** Árbol de almacenamiento y consulta de rectángulos.
	RTree (..),
		-- ** Punto en el espacio de coordenadas X y Y.
	Point,
	-- * Funciones exportadas.
		-- ** Permite la comparación de dos rectángulos según su número de Hilbert
	orderHV,
		-- ** Agrega un nuevo rectángulo a la estructura. Insertar un rectángulo duplicado es causa de error.
	insert,
		-- ** Elimina un rectángulo presente en la estructura. Eliminar un rectángulo inexistente es causa de error.
	delete,
		-- ** Consulta la estructura para determinar si el rectángulo suministrado como parámetro se solapa con uno o más rectángulos en la estructura. El resultado de la función es la lista de rectángulos solapados.
	search,
		-- ** Construye un RTree a partir de una lista de rectángulos.
	fromList,
		-- ** Construye un RTree a partir de un conjunto de rectángulos.
	fromList',
		-- ** Construye un Rectangle a partir de una lista de 8 enteros.
	makeRect4,
		-- * Construye un Rectangle a partir de dos Point.
	makeRect2
) 
where

import qualified Data.Set as DS
import qualified Data.Foldable as F
import qualified Data.Maybe as DM
import Data.Bits
import qualified Data.List as DL
import Data.Either
import Control.Monad.Error
import Test.QuickCheck

-- Hilbert Value.
type HV = Int

-- Punto en el espacio de coordenadas X y Y.
type Point = (Int,Int)

{-
  El tipo de datos @Rectangle@ representa rectángulos de coordenadas 
  X y Y entre 0 y 65536 mediante sus vertices.

  Se declara derivando de @Show@ para facilitar la depuración
  y la creación de rectángulos "manualmente".

  Se declara derivando de @Eq@ pues en la implantación interna
  del R-Tree es necesario comparar rectángulos.
  
  Se declara instancia de @Ord@ pues es necesario ordenar rectángulos.
  
  Se declara instancia de @Arbitrary@ pues es necesario generar 
  rectángulos arbitrarios para casos de prueba con QuickCheck.
 -}
data Rectangle = R {
	ul :: Point,	-- ^ Vertice superior izquierdo   (X0,Y1)
	ll :: Point,	-- ^ Vertice inferior izquierdo   (X0,Y0)
	lr :: Point,	-- ^ Vertice inferior derecho     (X1,Y0)
	ur :: Point		-- ^ Vertice superior derecho     (X1,Y1)
} deriving (Show)

instance Eq Rectangle where
	a == b = (ul a == ul b) && (ll a == ll b) && (lr a == lr b) && (ur a == ur b)

instance Ord Rectangle where
	compare r1 r2 = orderHV r1 r2

-- No se generan valores de todo el rango posible (0 a 65536) para aumentar
-- la posibilidad de coincidencias entre rectángulos.
instance Arbitrary Rectangle where
	arbitrary = do
		x0 <- choose (0,49)
		y0 <- choose (0,49)
		x1 <- choose (x0,50)
		y1 <- choose (y0,50)
		return $ R (x0,y1) (x0,y0) (x1,y0) (x1,y1)


{-
  El tipo de datos @RTree@ representa un R-Tree de Hilbert que 
  almacena rectángulos y permite consultarlos.

  Se declara derivando de @Show@ para facilitar la depuración
  y la creación de árboles "manualmente".

  Se declara derivando de @Eq@ pues en la implantación interna
  del R-Tree es necesario comparar árboles.
  
  Se declara instancia de @Ord@ pues es necesario ordenar árboles.
  
  Se declara instancia de @Arbitrary@ pues es necesario generar 
  árboles arbitrarios para casos de prueba con QuickCheck.
 -}
data RTree = 
	Branch {hv::HV, mbr::Rectangle, childs::(DS.Set RTree)}	-- ^ Rama del árbol
	| Leaf {hv::HV, mbr::Rectangle, rects::(DS.Set Rectangle)}	-- ^ Hoja del árbol
	| Empty														-- ^ Árbol vacio
	deriving (Show, Eq)

instance Ord RTree where
	compare t1 t2 =  compare (hv t1) (hv t2)

instance Arbitrary RTree where
	arbitrary = do
		rs <- {-suchThat (-}listOf1 $ (arbitrary :: Gen Rectangle){-) f-}
		return $ fromList rs
		{-where
		f xs = length xs >= 0 && length <= 1000-}


{- Error 
data RectError = RectangleNotFound Rectangle
				| DuplicateRectangle Rectangle
				| AnotherError String
				deriving (Show)

instance Error RectError where
	noMsg = AnotherError "Error desconocido."
	strMsg s = AnotherError s

instance Ord RectError where
	compare (RectangleNotFound r1) (RectangleNotFound r2) = orderHV r1 r2
-}


{-
  @splitPolicy@ es un valor constante que indica cuantos nodos vecinos 
  en un RTree deben estar llenos antes de hacer split.
 -}
splitPolicy = 2

{-
  @nodeCapacity@ es un valor constante que indica cuantos subárboles 
  puede contener una rama.
 -}
nodeCapacity = 3

{-
  @leafCapacity@ es un valor constante que indica cuantos rectángulos 
  puede contener una hoja.
 -}
leafCapacity = 3


{-
  @hilbval@ calcula el valor o número de Hilbert para el centro de un
  rectángulo.
 -}
hilbval:: Rectangle -> Int
hilbval (R _ (llx,lly) _ (urx,ury)) = 
	hilbertValue 17 ((llx+urx) `div` 2,(ury+lly) `div` 2)

{-
  @hilbertValue@ calcula el valor o número de Hilbert para un punto 
  cualquiera.
  http://www.serpentine.com/blog/2007/01/11/two-dimensional-spatial-hashing-with-space-filling-curves/
 -}
hilbertValue :: (Bits a, Ord a) => Int -> (a,a) -> a
hilbertValue d (x,y)
	| x < 0 || x >= 1 `shiftL` d = error "x bounds"
	| y < 0 || y >= 1 `shiftL` d = error "y bounds"
	| otherwise = dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y
		where
			dist 0 _area result _x _y = result
			dist side area result x y = case (compare x side,compare y side) of
				(LT, LT) -> step result y x
				(LT, _)  -> step (result+area) x (y-side)
				(_, LT)  -> step (result+area*3) (side-y-1) (side*2-x-1)
				(_, _)   -> step (result+area*2) (x-side) (y-side)
				where step = dist (side `shiftR` 1) (area `shiftR` 2)


{- 
  @orderHV@ establece la comparación de dos rectangulos segun su número 
  de Hilbert.
 -}
orderHV :: Rectangle -> Rectangle -> Ordering
orderHV r1 r2 = compare (hilbval r1) (hilbval r2)


{-
  @makeRect4@ permite construir un rectángulo a partir de una lista 
  de 8 enteros.
 -}
makeRect4 :: [Int] -> Rectangle
makeRect4 [xa,ya,xb,yb,xc,yc,xd,yd] = R (xa,ya) (xb,yb) (xc,yc) (xd,yd)


{-
  @makeRect2@ permite construir un rectángulo a partir de 2 puntos en
  el espacio.
 -}
makeRect2 :: Point -> Point -> Rectangle
makeRect2 l@(x0,y0) u@(x1,y1) 
	| l<=u		 = R (x0,y1) (x0,y0) (x1,y0) (x1,y1)
	| otherwise	 = R (x1,y0) (x1,y1) (x0,y1) (x0,y0)


{-
  @boundingBox@ calcula el rectángulo que envuelve a una serie de 
  rectangulos.
 -}
boundingBox :: (F.Foldable f) => f Rectangle -> Rectangle
boundingBox = F.foldr1 f
	where 
		f r1 r2 = buildR (g (ul r1) (ul r2) (lr r1) (lr r2)) 
		g (x10,y10) (x20,y20) (x11,y11) (x21,y21) = 
			(min x10 x20, max y10 y20, max x11 x21, min y11 y21)
		buildR (x0,y0,x1,y1) = R (x0,y0) (x0,y1) (x1,y1) (x1,y0)


{-
  @makeLeaf@ construye una hoja del árbol a partir de un conjunto de 
  rectángulos.
 -}
makeLeaf ::  DS.Set Rectangle -> RTree
makeLeaf ls = Leaf (hilbval br) (br) (ls)
	where
		br = boundingBox ls


{-
  @makeBranch@ cnstruye una rama del árbol a partir de un conjunto de 
  árboles, sean hojas o ramas.
 -}
makeBranch ::  DS.Set RTree -> RTree
makeBranch ls = Branch (hilbval br) (br) (ls)
	where
		br = boundingBox (DS.map mbr ls)


{-
  @raiseTree@ construye un árbol a partir de una lista de árboles.
 -}
raiseTree ::  [RTree] -> RTree
raiseTree []  = Empty
raiseTree [t] = t
raiseTree ls = raiseTree $ roots ls where
		roots = roots' []
		roots' res bs
			| null bs = reverse res
			| otherwise =
				roots' ((makeBranch (DS.fromList(child))) : res) rest where
				(child,rest) = splitAt nodeCapacity bs


{-
  @fromList@ construye un árbol a partir de una lista de rectángulos.
 -}
fromList ::  [Rectangle] -> RTree
fromList = raiseTree . leaves . DL.sortBy orderHV
	where 
		leaves  = leaves' [] 
		leaves' res [] = reverse res
		leaves' res s  = leaves' ((makeLeaf (DS.fromList rec)) : res ) rest where
			(rec,rest) = splitAt leafCapacity s

fromList' ::  F.Foldable t => t Rectangle -> RTree
fromList' = F.foldl' (f) Empty where
	f t r = either (\x ->t) (id) (insert t r)

---------------------------------------------------------------------

{- 
  @overflow@ determina si ocurre overflow en el árbol luego de insertar
  un rectángulo. 
  
  Si ocurre overflow se hace un split reordenando el árbol y lo separándolo 
  en dos subárboles con un nuevo nodo padre que tendrá su valor de Hilbert 
  igual a cero; de esta manera se reconoce que ocurrió un split. 
  
  Si no, retorna el árbol actual.
 -}
overflow :: RTree -> DS.Set RTree -> RTree
overflow (Leaf _hv _rec rs) leafs
	| exceso leafs = case (DS.findMin (childs rearmar)) of
		(Leaf _ _ _) -> rearmar
		otherwise -> rearmar{hv=0}	-- Permite reconocer que ocurrió un split
	| otherwise = makeBranch leafs
	where
		rearmar :: RTree
		rearmar = fromList $ DS.toList $ treeToSet $ makeBranch leafs
		exceso :: DS.Set RTree -> Bool
		exceso ls = DS.member True (DS.map f ls)
		f :: RTree -> Bool
		f leaf = DS.size (rects leaf) > leafCapacity
overflow (Branch _hv _rec ts) trees
	| split trees = makeBranch ajustar
	| DS.size trees > nodeCapacity = (desarmar trees)
	| otherwise = makeBranch trees
	where
		desarmar :: DS.Set RTree -> RTree
		desarmar ts = 
			makeBranch $ 
				DS.fromList [(makeBranch (DS.deleteMax ts)){hv=0},DS.findMax ts]
		ajustar :: DS.Set RTree
		ajustar = DS.fold subir DS.empty trees
		subir :: RTree -> DS.Set RTree -> DS.Set RTree
		subir arb arbs
			| hv arb == 0 = DS.union (childs arb) arbs
			| otherwise = DS.insert arb arbs
		split :: DS.Set RTree -> Bool
		split ts = DS.member True (DS.map f ts)
		f :: RTree -> Bool
		f branch = hv branch == 0


{- 
  @overflowHandling@ es el manejador de overflow al insertar un 
  rectángulo en un árbol.
  
  Utiliza @overflow@ para determinar si ocurrió un overflow y 
  manejarlo según sea el caso.
 -}
overflowHandling :: RTree -> RTree
overflowHandling (Branch _hv _rec ts) = overflow (DS.findMin ts) ts


{- 
  @insert@ inserta un rectángulo en un árbol.
  
  Si el rectángulo ya se encuentra en el árbol se reporta un error
  de rectángulo duplicado.
  
  Si no, se devulve el nuevo árbol.
 -}
insert :: RTree -> Rectangle -> Either String RTree
insert Empty r = return $ makeLeaf $ DS.singleton r
insert (Leaf _hv _rec rs) r 
	| DS.size (DS.insert r rs) == DS.size rs = throwError "DuplicateRectangle"
	| otherwise = return $ makeLeaf $ DS.insert r rs
insert (Branch _hv _rec trees) r = case (elegirTree trees) of
	Right tree -> return $ overflowHandling $ makeBranch $ reinsert tree
	otherwise -> throwError "DuplicateRectangle"
	where
		reinsert :: RTree -> DS.Set RTree
		reinsert a = DS.insert a (DS.delete (arbol trees) trees)
		elegirTree :: DS.Set RTree -> Either String RTree
		elegirTree seqt = insert (arbol seqt) r
		arbol :: DS.Set RTree -> RTree
		arbol cjto
			| DS.null (fst (DS.partition f cjto)) = DS.findMin cjto
			| otherwise = DS.findMax (fst (DS.partition f cjto))
		f :: RTree -> Bool
		f tree = hv tree < hilbval r


{-
  @delete@ elimina un rectángulo de un árbol.
  
  Si el rectángulo se encuentra en el árbol se retorne un nuevo arbol
  que no contiene al rectángulo. En caso contrario reporta un error de 
  rectángulo no encontrado.

-}
delete :: RTree -> Rectangle -> Either String RTree
delete Empty r = throwError "RectangleNotFound"
delete (Leaf _hv _rec rs) r 
	| (DS.size eliminado) == (DS.size rs) = throwError "RectangleNotFound"
	| DS.null eliminado = return Empty
	| otherwise = return $ makeLeaf eliminado
	where
		eliminado :: DS.Set Rectangle
		eliminado = DS.delete r rs

delete (Branch _hv _rec trees) r = case g of
	(null, Nothing) -> throwError "RectangleNotFound"
	([newTree], Just oldTree) -> return $ noBranch $ rearmar newTree oldTree
	where
		noBranch :: DS.Set RTree -> RTree
		noBranch seqt 
			| DS.null seqt = Empty
			| otherwise = makeBranch seqt
		rearmar :: RTree -> RTree -> DS.Set RTree
		rearmar Empty old = DS.delete old trees
		rearmar nuevo old = DS.insert nuevo (DS.delete old trees)
		g :: ([RTree], Maybe RTree)
		g = (rights (DS.toList (DS.map f trees)), elimViejo trees (DS.toList (DS.map f trees)))
		elimViejo :: DS.Set RTree -> [Either String RTree] -> Maybe RTree
		elimViejo _ [] = Nothing
		elimViejo ts ((Right _):xs) = return $ DS.findMin ts
		elimViejo ts ((Left _):xs) = elimViejo (DS.deleteMin ts) xs
		f :: RTree -> Either String RTree
		f tree = delete tree r



{-
  @search@ busca todos los rectángulos que se solapen con un 
  rectángulo dado.
  
  Si no existen solapamientos con el rectángulo suministrado no se 
  devuelve nada.
  
  Si existen se devuelve una lista de todos los rectángulos solapados. 
-}
search ::  RTree -> Rectangle -> Maybe [Rectangle]
search Empty _r = Nothing
search (Leaf _hv _rec rs) r
	| DS.null rs  = Nothing
	| otherwise = case (DS.toList $ DS.filter (intersects r) rs) of
		[] -> Nothing
		l -> return l
search (Branch _hv br rs) r
	| intersects r br = F.foldMap (flip (search) r) rs
	| otherwise = Nothing


{-
  @intersects@ determina si dos rectángulos se intersectan.
 -}
intersects ::  Rectangle -> Rectangle -> Bool
intersects a b = 
	((bx0 < ax1 && ax1 <= bx1) && 
		((by0 <= ay0 && ay0 < by1) || 
		(by0 < ay1 && ay1 <= by1))) ||
	((by0 < ay1 && ay1 <= by1) && 
		((bx0 <= ax0 && ax0 < bx1) || 
		(bx0 < ax1 && ax1 <= bx1)))
	where
		ax0 = fst $ ll a
		bx0 = fst $ ll b
		ax1 = fst $ ur a
		bx1 = fst $ ur b
		ay0 = snd $ ll a
		by0 = snd $ ll b
		ay1 = snd $ ur a
		by1 = snd $ ur b


{-
  @tall@ calcula la altura de un árbol desde su raíz hasta las hojas.
  
  Devuelve en una tupla la mínima altura del árbol y la máxima altura
  del árbol. La diferencia entre estos dos valores debe ser a lo sumo
  de una unidad.
 -}
tall :: RTree -> (Int,Int)
tall Empty = (0,0)
tall (Leaf _hv _rec _rs) = (1,1)
tall (Branch _hv _rec ts) = (fst (DS.findMin talls), snd (DS.findMax talls))
	where
		talls :: DS.Set (Int,Int)
		talls = DS.map f ts
		f :: RTree -> (Int,Int)
		f tree = ((fst (tall tree)) + 1, (snd (tall tree)) + 1)


{-
  @treeToSet@ devuelve el conjunto de rectángulos que conforman un árbol.
 -}
treeToSet :: RTree -> DS.Set Rectangle
treeToSet Empty = DS.empty
treeToSet (Leaf _hv _rec ls) = ls
treeToSet (Branch _hv _rec ts) = 
	DS.fold DS.union DS.empty (DS.map treeToSet ts)


-- Propiedades de prueba para QuickCheck

{-
  @prop_tall_insert@ representa una propiedad que debe cumplirse al insertar
  un rectángulo en un árbol.
  
  Insertar un rectángulo repetido genera error, sino la altura del árbol
  resultante es igual,o a lo sumo superior por una unidad, a la altura del 
  árbol antes de insertar el rectángulo.
 -}
prop_tall_insert :: RTree -> Rectangle -> Bool
prop_tall_insert t r = if (DS.member r (treeToSet t)) 
	then insert t r == throwError "DuplicateRectangle"
	else (snd (tall (head (rights [insert t r]))) == snd (tall t)) || 
		(snd (tall (head (rights [insert t r]))) == (snd (tall t)) + 1)


{-
  @prop_member_insert@ representa una propiedad que debe cumplirse al 
  insertar un rectángulo en un árbol.
  
  Si al insertar un rectángulo se produce un error entonces el rectángulo
  que se desea insertar ya pertenece al árbol original.
  
  Si no, el árbol original no contenía al rectángulo nuevo y el árbol 
  resultante si lo contiene.
 -}
prop_member_insert :: RTree -> Rectangle -> Bool
prop_member_insert t r = case (insert t r) of
	Right tree -> (DS.member r (treeToSet tree)) && 
					not (DS.member r (treeToSet t)) &&
					(DS.insert r (treeToSet t)) == (treeToSet tree)
	otherwise -> DS.member r $ treeToSet t


{-
  @prop_ord_insert@ representa una propiedad que debe cumplirse al 
  insertar un rectángulo en un árbol.
  
  Si al insertar un rectángulo se produce un error entonces el rectángulo
  que se desea insertar ya pertenece al árbol original.
  
  Si no, el nuevo rectángulo insertado estará detrás del rectángulo con 
  mayor valor de Hilbert menor al suyo y antes del rectángulo cuyo número 
  de Hilbert sea mayor o igual al suyo.
 -}
prop_ord_insert :: RTree -> Rectangle -> Bool
prop_ord_insert t r = case (insert t r) of
	Right tree -> if (not (DS.null (mayor tree)) && not (DS.null (menor tree)))
		then (hilbval (DS.findMin (mayor tree)) >= hilbval r) &&
			(hilbval (DS.findMax (menor tree)) < hilbval r)
		else if (not (DS.null (mayor tree)))
			then hilbval (DS.findMin (mayor tree)) >= hilbval r
			else hilbval (DS.findMax (menor tree)) < hilbval r
	otherwise -> DS.member r $ treeToSet t
	where
		menor = fst . DS.split r . treeToSet
		mayor = snd . DS.split r . treeToSet


{-
  @prop_empty_insert@ representa una propiedad que debe cumplirse al
  insertar un rectángulo en un árbol vacío.
  
  Al insertar cualquier rectángulo en un árbol vacío no debe producirse
  ningún error y el árbol resultante debe ser una hoja que contenga
  únicamente al nuevo rectángulo.
 -}
prop_empty_insert :: Rectangle -> Bool
prop_empty_insert r = case (insert Empty r) of
	Right t -> makeLeaf (DS.singleton r) == t


{-
  @prop_tall_delete@ representa una propiedad que debe cumplirse al eliminar
  un rectángulo de un árbol.
  
  Eliminar un rectángulo inexistente genera error, sino la altura del árbol 
  resultante es igual, o a lo sumo inferior por una unidad, a la altura del 
  árbol antes de eliminar el rectángulo.
 -}
prop_tall_delete :: RTree -> Rectangle -> Bool
prop_tall_delete t r = if (DS.member r (treeToSet t)) 
	then snd (tall (head (rights [delete t r]))) <= snd (tall t)
	else delete t r == throwError "RectangleNotFound"


{-
  @prop_member_delete@ representa una propiedad que debe cumplirse al 
  eliminar un rectángulo de un árbol.
  
  Si al eliminar un rectángulo se produce un error entonces el rectángulo 
  no se encuentra en el árbol.
  
  Si no, el árbol original contenía al rectángulo y el árbol resultante 
  de la eliminación no lo contiene.
 -}
prop_member_delete :: RTree -> Rectangle -> Bool
prop_member_delete t r = case (delete t r) of
	Right tree -> not (DS.member r (treeToSet tree)) && 
					(DS.member r (treeToSet t))
	otherwise -> not $ DS.member r $ treeToSet t


{-
  @prop_empty_delete@ representa una propiedad que debe cumplirse al
  eliminar un rectángulo en un árbol vacío.
  
  Siempre que se elimine un rectángulo en un árbol vacío se producirá
  un error.
 -}
prop_empty_delete :: Rectangle -> Bool
prop_empty_delete r = case (delete Empty r) of
	Left e -> e == "RectangleNotFound"


{-
  @prop_search@ representa a la propiedad que debe cumplirse al buscar
  solapamientos con un rectángulo en un árbol.
  
  Si al buscar un rectángulo se produce un Nothing quiere decir que el
  rectángulo no se intersectaba con ningúno en el árbol.
  
  Si se produce una lista de solapamientos entonces dicha lista es un 
  subconjunto del conjunto de rectángulos que conforman el árbol y cada
  rectángulo en ella se intersecta con el rectángulo de búsqueda.
 -}
prop_search :: RTree -> Rectangle -> Bool
prop_search t r = case (search t r) of
	Just rs -> (DS.isSubsetOf inside rectSet) && ((DS.fromList rs) ==  inside)
	Nothing -> (DS.null inside) && (outside == rectSet)
	where
		(inside,outside) = DS.partition (intersects r) (rectSet)
		rectSet = treeToSet t


{-
  @prop_empty_search@ representa a la propiedad que debe cumplirse al buscar
  solapamientos con un rectángulo en un árbol vacío.
  
  Siempre que se busque algún solapamiento en un árbol vacío no se obtendrán
  resultados.
 -}
prop_empty_search :: Rectangle -> Bool
prop_empty_search r = (search Empty r) == Nothing


{-
  @test@ ejecuta todas las pruebas.
 -}
test :: IO()
test = do
	putStrLn "member_insert"
	quickCheck prop_member_insert
	putStrLn "ord_insert"
	quickCheck prop_ord_insert
	putStrLn "tall insert"
	quickCheck prop_tall_insert
	putStrLn "empty insert"
	quickCheck prop_empty_insert
	putStrLn "search"
	quickCheck prop_search
	putStrLn "empty search"
	quickCheck prop_empty_search
	putStrLn "member delete"
	quickCheck prop_member_delete
	putStrLn "tall delete"
	quickCheck prop_tall_delete
	putStrLn "empty delete"
	quickCheck prop_empty_delete
	putStrLn "DONE"


--------------------------------------------------------------------------------


-- ARBOL DE PRUEBA
a = R {ll=(30,40), ul=(30,55), ur=(35,55), lr=(35,40)}
b = R {ll=(20,15), ul=(20,25), ur=(30,25), lr=(30,15)}
c = R {ll=(30,40), ul=(30,55), ur=(35,55), lr=(35,40)}
d = R {ll=(33,30), ul=(33,43), ur=(37,43), lr=(37,30)}
a2 = R {ll=(10,5), ul=(10,80), ur=(50,80), lr=(50,5)}
no = R {ll=(30,15), ul=(30,25), ur=(40,25), lr=(40,15)}
enlos2 = R (10,80) (10,30) (72,30) (72,80)
hoja1 = makeLeaf (DS.fromList [b,d,c])
papa1 = R {ll=(10,5), ul=(10,80), ur=(50,80), lr=(50,5)}
x = R {ll=(65,50), ul=(65,55), ur=(70,55), lr=(70,50)}
y = R {ll=(75,40), ul=(75,50), ur=(80,50), lr=(80,40)}
hoja2 = makeLeaf (DS.fromList [x,y])
papa2 = R {ll=(40,30), ul=(40,90), ur=(90,90), lr=(90,30)}
papa = R (0,100) (0,0) (100,0) (100,100)
arbolp = makeBranch (DS.fromList [
	makeBranch (DS.fromList [hoja1]),
	makeBranch (DS.fromList [hoja2])])



-- problemas con el fromList: me elimina una hoja de la nada :S
ainsert = Branch {hv = 653, mbr = R {ul = (2,50), ll = (2,2), lr = (49,2), ur = (49,50)}, childs = DS.fromList [
Leaf {hv = 564, mbr = R {ul = (2,41), ll = (2,2), lr = (37,2), ur = (37,41)}, rects = DS.fromList [R {ul = (5,30), ll = (5,15), lr = (37,15), ur = (37,30)},R {ul = (2,41), ll = (2,2), lr = (33,2), ur = (33,41)},R {ul = (8,29), ll = (8,23), lr = (32,23), ur = (32,29)}]},
Leaf {hv = 2105, mbr = R {ul = (17,50), ll = (17,23), lr = (49,23), ur = (49,50)}, rects = DS.fromList [R {ul = (44,36), ll = (44,23), lr = (46,23), ur = (46,36)},R {ul = (30,50), ll = (30,28), lr = (41,28), ur = (41,50)},R {ul = (17,50), ll = (17,36), lr = (49,36), ur = (49,50)}]},
Leaf {hv = 3404, mbr = R {ul = (20,44), ll = (20,29), lr = (41,29), ur = (41,44)}, rects = DS.fromList [R {ul = (39,43), ll = (39,42), lr = (41,42), ur = (41,43)},R {ul = (20,44), ll = (20,29), lr = (35,29), ur = (35,44)}]}]}

rinsert = R {ul = (34,37), ll = (34,30), lr = (47,30), ur = (47,37)}

a2insert = Branch {hv = 653, mbr = R {ul = (2,50), ll = (2,2), lr = (49,2), ur = (49,50)}, childs = DS.fromList [
Leaf {hv = 564, mbr = R {ul = (2,41), ll = (2,2), lr = (37,2), ur = (37,41)}, rects = DS.fromList [R {ul = (5,30), ll = (5,15), lr = (37,15), ur = (37,30)},R {ul = (2,41), ll = (2,2), lr = (33,2), ur = (33,41)},R {ul = (8,29), ll = (8,23), lr = (32,23), ur = (32,29)}]},
Leaf {hv = 2105, mbr = R {ul = (17,50), ll = (17,23), lr = (49,23), ur = (49,50)}, rects = DS.fromList [R {ul = (44,36), ll = (44,23), lr = (46,23), ur = (46,36)},R {ul = (30,50), ll = (30,28), lr = (41,28), ur = (41,50)},R {ul = (17,50), ll = (17,36), lr = (49,36), ur = (49,50)},R {ul = (34,37), ll = (34,30), lr = (47,30), ur = (47,37)}]},
Leaf {hv = 3404, mbr = R {ul = (20,44), ll = (20,29), lr = (41,29), ur = (41,44)}, rects = DS.fromList [R {ul = (39,43), ll = (39,42), lr = (41,42), ur = (41,43)},R {ul = (20,44), ll = (20,29), lr = (35,29), ur = (35,44)}]}]}

maphilbval = DS.map hilbval (treeToSet ainsert)
unir2 (a,b) = DS.union a b


-- problemas al intentar eliminar un rect q no existe pero tiene el mismo HV q uno q si existe
adelete = Branch {hv = 652, mbr = R {ul = (0,50), ll = (0,4), lr = (50,4), ur = (50,50)}, childs = DS.fromList [Branch {hv = 651, mbr = R {ul = (2,50), ll = (2,4), lr = (50,4), ur = (50,50)}, childs = DS.fromList [Branch {hv = 651, mbr = R {ul = (6,50), ll = (6,5), lr = (47,5), ur = (47,50)}, childs = DS.fromList [Leaf {hv = 610, mbr = R {ul = (6,44), ll = (6,15), lr = (37,15), ur = (37,44)}, rects = DS.fromList [R {ul = (6,44), ll = (6,16), lr = (32,16), ur = (32,44)},R {ul = (18,44), ll = (18,15), lr = (19,15), ur = (19,44)},R {ul = (8,37), ll = (8,26), lr = (37,26), ur = (37,37)}]},Leaf {hv = 629, mbr = R {ul = (6,50), ll = (6,5), lr = (35,5), ur = (35,50)}, rects = DS.fromList [R {ul = (6,9), ll = (6,7), lr = (19,7), ur = (19,9)},R {ul = (8,50), ll = (8,5), lr = (23,5), ur = (23,50)},R {ul = (7,39), ll = (7,6), lr = (35,6), ur = (35,39)}]},Leaf {hv = 691, mbr = R {ul = (14,44), ll = (14,11), lr = (47,11), ur = (47,44)}, rects = DS.fromList [R {ul = (22,42), ll = (22,11), lr = (22,11), ur = (22,42)},R {ul = (16,44), ll = (16,15), lr = (47,15), ur = (47,44)},R {ul = (14,31), ll = (14,13), lr = (41,13), ur = (41,31)}]}]},Branch {hv = 723, mbr = R {ul = (2,43), ll = (2,4), lr = (50,4), ur = (50,43)}, childs = DS.fromList [Leaf {hv = 727, mbr = R {ul = (2,40), ll = (2,4), lr = (49,4), ur = (49,40)}, rects = DS.fromList [R {ul = (12,14), ll = (12,8), lr = (44,8), ur = (44,14)},R {ul = (2,16), ll = (2,14), lr = (37,14), ur = (37,16)},R {ul = (49,40), ll = (49,4), lr = (49,4), ur = (49,40)}]},Leaf {hv = 1845, mbr = R {ul = (38,43), ll = (38,12), lr = (50,12), ur = (50,43)}, rects = DS.fromList [R {ul = (38,43), ll = (38,17), lr = (46,17), ur = (46,43)},R {ul = (47,35), ll = (47,12), lr = (47,12), ur = (47,35)},R {ul = (44,30), ll = (44,15), lr = (50,15), ur = (50,30)}]},Leaf {hv = 1929, mbr = R {ul = (30,37), ll = (30,5), lr = (42,5), ur = (42,37)}, rects = DS.fromList [R {ul = (38,37), ll = (38,7), lr = (42,7), ur = (42,37)},R {ul = (37,36), ll = (37,5), lr = (39,5), ur = (39,36)},R {ul = (30,23), ll = (30,10), lr = (35,10), ur = (35,23)}]}]},Branch {hv = 2028, mbr = R {ul = (24,47), ll = (24,16), lr = (50,16), ur = (50,47)}, childs = DS.fromList [Leaf {hv = 2013, mbr = R {ul = (24,34), ll = (24,18), lr = (50,18), ur = (50,34)}, rects = DS.fromList [R {ul = (25,27), ll = (25,18), lr = (43,18), ur = (43,27)},R {ul = (24,28), ll = (24,19), lr = (42,19), ur = (42,28)},R {ul = (27,34), ll = (27,22), lr = (50,22), ur = (50,34)}]},Leaf {hv = 2020, mbr = R {ul = (33,41), ll = (33,16), lr = (43,16), ur = (43,41)}, rects = DS.fromList [R {ul = (36,41), ll = (36,16), lr = (43,16), ur = (43,41)},R {ul = (33,32), ll = (33,29), lr = (39,29), ur = (39,32)},R {ul = (36,38), ll = (36,28), lr = (37,28), ur = (37,38)}]},Leaf {hv = 2117, mbr = R {ul = (31,47), ll = (31,33), lr = (39,33), ur = (39,47)}, rects = DS.fromList [R {ul = (32,41), ll = (32,33), lr = (38,33), ur = (38,41)},R {ul = (31,46), ll = (31,39), lr = (39,39), ur = (39,46)},R {ul = (37,47), ll = (37,47), lr = (38,47), ur = (38,47)}]}]}]},Branch {hv = 3437, mbr = R {ul = (0,50), ll = (0,19), lr = (50,19), ur = (50,50)}, childs = DS.fromList [Branch {hv = 3383, mbr = R {ul = (9,50), ll = (9,34), lr = (50,34), ur = (50,50)}, childs = DS.fromList [Leaf {hv = 2190, mbr = R {ul = (30,48), ll = (30,36), lr = (50,36), ur = (50,48)}, rects = DS.fromList [R {ul = (30,45), ll = (30,36), lr = (45,36), ur = (45,45)},R {ul = (32,48), ll = (32,36), lr = (50,36), ur = (50,48)},R {ul = (40,46), ll = (40,45), lr = (46,45), ur = (46,46)}]},Leaf {hv = 2539, mbr = R {ul = (46,49), ll = (46,34), lr = (50,34), ur = (50,49)}, rects = DS.fromList [R {ul = (46,34), ll = (46,34), lr = (46,34), ur = (46,34)},R {ul = (47,45), ll = (47,45), lr = (50,45), ur = (50,45)},R {ul = (48,49), ll = (48,46), lr = (48,46), ur = (48,49)}]},Leaf {hv = 3382, mbr = R {ul = (9,50), ll = (9,37), lr = (49,37), ur = (49,50)}, rects = DS.fromList [R {ul = (46,50), ll = (46,46), lr = (47,46), ur = (47,50)},R {ul = (39,50), ll = (39,49), lr = (40,49), ur = (40,50)},R {ul = (9,50), ll = (9,37), lr = (49,37), ur = (49,50)}]}]},Branch {hv = 3436, mbr = R {ul = (0,49), ll = (0,19), lr = (49,19), ur = (49,49)}, childs = DS.fromList [Leaf {hv = 3395, mbr = R {ul = (11,43), ll = (11,35), lr = (49,35), ur = (49,43)}, rects = DS.fromList [R {ul = (13,43), ll = (13,41), lr = (46,41), ur = (46,43)},R {ul = (24,42), ll = (24,35), lr = (36,35), ur = (36,42)},R {ul = (11,43), ll = (11,36), lr = (49,36), ur = (49,43)}]},Leaf {hv = 3404, mbr = R {ul = (15,49), ll = (15,23), lr = (46,23), ur = (46,49)}, rects = DS.fromList [R {ul = (24,49), ll = (24,23), lr = (33,23), ur = (33,49)},R {ul = (15,40), ll = (15,33), lr = (46,33), ur = (46,40)},R {ul = (28,43), ll = (28,25), lr = (30,25), ur = (30,43)}]},Leaf {hv = 3684, mbr = R {ul = (0,47), ll = (0,19), lr = (22,19), ur = (22,47)}, rects = DS.fromList [R {ul = (18,45), ll = (18,19), lr = (20,19), ur = (20,45)},R {ul = (0,42), ll = (0,22), lr = (22,22), ur = (22,42)},R {ul = (9,47), ll = (9,25), lr = (9,25), ur = (9,47)}]}]}]}]}
rdelete = R {ul = (31,50), ll = (31,48), lr = (48,48), ur = (48,50)}

a2delete = Branch {hv = 642, mbr = R {ul = (0,50), ll = (0,0), lr = (50,0), ur = (50,50)}, childs = DS.fromList [Branch {hv = 642, mbr = R {ul = (0,50), ll = (0,0), lr = (50,0), ur = (50,50)}, childs = DS.fromList [Branch {hv = 626, mbr = R {ul = (0,50), ll = (0,2), lr = (44,2), ur = (44,50)}, childs = DS.fromList [Leaf {hv = 579, mbr = R {ul = (3,45), ll = (3,3), lr = (32,3), ur = (32,45)}, rects = DS.fromList [R {ul = (3,45), ll = (3,4), lr = (20,4), ur = (20,45)},R {ul = (8,31), ll = (8,3), lr = (18,3), ur = (18,31)},R {ul = (15,26), ll = (15,15), lr = (32,15), ur = (32,26)}]},Leaf {hv = 611, mbr = R {ul = (0,50), ll = (0,9), lr = (40,9), ur = (40,50)}, rects = DS.fromList [R {ul = (11,32), ll = (11,11), lr = (33,11), ur = (33,32)},R {ul = (4,28), ll = (4,16), lr = (40,16), ur = (40,28)},R {ul = (0,50), ll = (0,9), lr = (37,9), ur = (37,50)}]},Leaf {hv = 695, mbr = R {ul = (15,50), ll = (15,2), lr = (44,2), ur = (44,50)}, rects = DS.fromList [R {ul = (15,47), ll = (15,2), lr = (28,2), ur = (28,47)},R {ul = (22,50), ll = (22,12), lr = (29,12), ur = (29,50)},R {ul = (17,32), ll = (17,30), lr = (44,30), ur = (44,32)}]}]},Branch {hv = 756, mbr = R {ul = (8,37), ll = (8,0), lr = (48,0), ur = (48,37)}, childs = DS.fromList [Leaf {hv = 717, mbr = R {ul = (22,37), ll = (22,7), lr = (37,7), ur = (37,37)}, rects = DS.fromList [R {ul = (22,37), ll = (22,16), lr = (37,16), ur = (37,37)},R {ul = (27,36), ll = (27,14), lr = (33,14), ur = (33,36)},R {ul = (22,31), ll = (22,7), lr = (33,7), ur = (33,31)}]},Leaf {hv = 772, mbr = R {ul = (11,31), ll = (11,0), lr = (48,0), ur = (48,31)}, rects = DS.fromList [R {ul = (11,27), ll = (11,5), lr = (44,5), ur = (44,27)},R {ul = (16,29), ll = (16,8), lr = (47,8), ur = (47,29)},R {ul = (13,31), ll = (13,0), lr = (48,0), ur = (48,31)}]},Leaf {hv = 830, mbr = R {ul = (8,27), ll = (8,1), lr = (40,1), ur = (40,27)}, rects = DS.fromList [R {ul = (12,13), ll = (12,5), lr = (40,5), ur = (40,13)},R {ul = (10,22), ll = (10,4), lr = (35,4), ur = (35,22)},R {ul = (8,27), ll = (8,1), lr = (33,1), ur = (33,27)}]}]},Branch {hv = 761, mbr = R {ul = (10,39), ll = (10,0), lr = (50,0), ur = (50,39)}, childs = DS.fromList [Leaf {hv = 897, mbr = R {ul = (10,15), ll = (10,0), lr = (35,0), ur = (35,15)}, rects = DS.fromList [R {ul = (10,15), ll = (10,4), lr = (35,4), ur = (35,15)},R {ul = (11,9), ll = (11,6), lr = (29,6), ur = (29,9)},R {ul = (28,15), ll = (28,0), lr = (35,0), ur = (35,15)}]},Leaf {hv = 1182, mbr = R {ul = (35,24), ll = (35,1), lr = (50,1), ur = (50,24)}, rects = DS.fromList [R {ul = (38,20), ll = (38,11), lr = (45,11), ur = (45,20)},R {ul = (35,24), ll = (35,3), lr = (50,3), ur = (50,24)},R {ul = (45,24), ll = (45,1), lr = (48,1), ur = (48,24)}]},Leaf {hv = 1819, mbr = R {ul = (34,39), ll = (34,17), lr = (49,17), ur = (49,39)}, rects = DS.fromList [R {ul = (47,37), ll = (47,21), lr = (49,21), ur = (49,37)},R {ul = (49,39), ll = (49,20), lr = (49,20), ur = (49,39)},R {ul = (34,35), ll = (34,17), lr = (47,17), ur = (47,35)}]}]}]},Branch {hv = 651, mbr = R {ul = (2,50), ll = (2,5), lr = (50,5), ur = (50,50)}, childs = DS.fromList [Branch {hv = 2008, mbr = R {ul = (28,48), ll = (28,5), lr = (49,5), ur = (49,48)}, childs = DS.fromList [Leaf {hv = 1853, mbr = R {ul = (43,46), ll = (43,5), lr = (49,5), ur = (49,46)}, rects = DS.fromList [R {ul = (43,46), ll = (43,5), lr = (46,5), ur = (46,46)},R {ul = (44,33), ll = (44,22), lr = (44,22), ur = (44,33)},R {ul = (44,39), ll = (44,16), lr = (49,16), ur = (49,39)}]},Leaf {hv = 2019, mbr = R {ul = (28,40), ll = (28,16), lr = (47,16), ur = (47,40)}, rects = DS.fromList [R {ul = (28,35), ll = (28,16), lr = (41,16), ur = (41,35)},R {ul = (37,40), ll = (37,20), lr = (39,20), ur = (39,40)},R {ul = (31,39), ll = (31,26), lr = (47,26), ur = (47,39)}]},Leaf {hv = 2173, mbr = R {ul = (36,48), ll = (36,34), lr = (40,34), ur = (40,48)}, rects = DS.fromList [R {ul = (36,37), ll = (36,34), lr = (36,34), ur = (36,37)},R {ul = (37,48), ll = (37,41), lr = (40,41), ur = (40,48)},R {ul = (37,42), ll = (37,42), lr = (38,42), ur = (38,42)}]}]},Branch {hv = 2267, mbr = R {ul = (32,50), ll = (32,22), lr = (50,22), ur = (50,50)}, childs = DS.fromList [Leaf {hv = 2175, mbr = R {ul = (32,50), ll = (32,30), lr = (47,30), ur = (47,50)}, rects = DS.fromList [R {ul = (32,50), ll = (32,30), lr = (43,30), ur = (43,50)},R {ul = (40,47), ll = (40,46), lr = (47,46), ur = (47,47)},R {ul = (43,47), ll = (43,47), lr = (45,47), ur = (45,47)}]},Leaf {hv = 2226, mbr = R {ul = (42,50), ll = (42,34), lr = (50,34), ur = (50,50)}, rects = DS.fromList [R {ul = (45,50), ll = (45,45), lr = (50,45), ur = (50,50)},R {ul = (42,48), ll = (42,39), lr = (46,39), ur = (46,48)},R {ul = (45,40), ll = (45,34), lr = (45,34), ur = (45,40)}]},Leaf {hv = 2275, mbr = R {ul = (35,48), ll = (35,22), lr = (49,22), ur = (49,48)}, rects = DS.fromList [R {ul = (35,48), ll = (35,22), lr = (49,22), ur = (49,48)},R {ul = (36,48), ll = (36,23), lr = (44,23), ur = (44,48)},R {ul = (41,35), ll = (41,31), lr = (45,31), ur = (45,35)}]}]},Branch {hv = 3431, mbr = R {ul = (2,50), ll = (2,16), lr = (50,16), ur = (50,50)}, childs = DS.fromList [Leaf {hv = 2240, mbr = R {ul = (45,49), ll = (45,30), lr = (50,30), ur = (50,49)}, rects = DS.fromList [R {ul = (48,41), ll = (48,30), lr = (50,30), ur = (50,41)},R {ul = (48,37), ll = (48,33), lr = (49,33), ur = (49,37)},R {ul = (45,49), ll = (45,47), lr = (48,47), ur = (48,49)}]},Leaf {hv = 2289, mbr = R {ul = (42,48), ll = (42,16), lr = (49,16), ur = (49,48)}, rects = DS.fromList [R {ul = (42,35), ll = (42,34), lr = (49,34), ur = (49,35)},R {ul = (44,38), ll = (44,29), lr = (49,29), ur = (49,38)},R {ul = (49,48), ll = (49,16), lr = (49,16), ur = (49,48)}]},Leaf {hv = 3268, mbr = R {ul = (2,50), ll = (2,47), lr = (50,47), ur = (50,50)}, rects = DS.fromList [R {ul = (40,49), ll = (40,48), lr = (43,48), ur = (43,49)},R {ul = (2,50), ll = (2,49), lr = (50,49), ur = (50,50)},R {ul = (21,50), ll = (21,47), lr = (37,47), ur = (37,50)}]}]}]},Branch {hv = 3437, mbr = R {ul = (3,49), ll = (3,19), lr = (47,19), ur = (47,49)}, childs = DS.fromList [Branch {hv = 3437, mbr = R {ul = (4,49), ll = (4,19), lr = (47,19), ur = (47,49)}, childs = DS.fromList [Leaf {hv = 3351, mbr = R {ul = (10,49), ll = (10,43), lr = (40,43), ur = (40,49)}, rects = DS.fromList [R {ul = (28,45), ll = (28,43), lr = (35,43), ur = (35,45)},R {ul = (10,49), ll = (10,45), lr = (40,45), ur = (40,49)},R {ul = (22,47), ll = (22,43), lr = (32,43), ur = (32,47)}]},Leaf {hv = 3437, mbr = R {ul = (18,49), ll = (18,19), lr = (33,19), ur = (33,49)}, rects = DS.fromList [R {ul = (20,38), ll = (20,31), lr = (33,31), ur = (33,38)},R {ul = (26,49), ll = (26,19), lr = (28,19), ur = (28,49)},R {ul = (18,45), ll = (18,19), lr = (25,19), ur = (25,45)}]},Leaf {hv = 3442, mbr = R {ul = (4,49), ll = (4,25), lr = (47,25), ur = (47,49)}, rects = DS.fromList [R {ul = (4,49), ll = (4,38), lr = (47,38), ur = (47,49)},R {ul = (27,36), ll = (27,28), lr = (34,28), ur = (34,36)},R {ul = (14,46), ll = (14,25), lr = (40,25), ur = (40,46)}]}]},Branch {hv = 3655, mbr = R {ul = (3,49), ll = (3,27), lr = (24,27), ur = (24,49)}, childs = DS.fromList [Leaf {hv = 3650, mbr = R {ul = (4,49), ll = (4,27), lr = (24,27), ur = (24,49)}, rects = DS.fromList [R {ul = (7,42), ll = (7,40), lr = (18,40), ur = (18,42)},R {ul = (4,49), ll = (4,27), lr = (24,27), ur = (24,49)},R {ul = (12,39), ll = (12,38), lr = (15,38), ur = (15,39)}]},Leaf {hv = 3706, mbr = R {ul = (3,47), ll = (3,31), lr = (19,31), ur = (19,47)}, rects = DS.fromList [R {ul = (3,47), ll = (3,31), lr = (19,31), ur = (19,47)}]}]}]}]}
r2delete = R {ul = (31,39), ll = (31,13), lr = (50,13), ur = (50,39)}





-- problemas al buscar esto... el resultado de la busqueda a mano tiene un rectangulo mas q lo obtenido en el partition con intersects...
asearch = Branch {hv = 642, mbr = R {ul = (1,50), ll = (1,0), lr = (50,0), ur = (50,50)}, childs = DS.fromList [
	Branch {hv = 642, mbr = R {ul = (1,50), ll = (1,0), lr = (50,0), ur = (50,50)}, childs = DS.fromList [
		Branch {hv = 585, mbr = R {ul = (1,50), ll = (1,2), lr = (38,2), ur = (38,50)}, childs = DS.fromList [
			Leaf {hv = 459, mbr = R {ul = (1,40), ll = (1,3), lr = (23,3), ur = (23,40)}, 
				rects = DS.fromList [R {ul = (1,4), ll = (1,3), lr = (23,3), ur = (23,4)},R {ul = (2,40), ll = (2,17), lr = (2,17), ur = (2,40)},R {ul = (1,35), ll = (1,23), lr = (22,23), ur = (22,35)}]},
			Leaf {hv = 584, mbr = R {ul = (4,50), ll = (4,2), lr = (33,2), ur = (33,50)}, 
				rects = DS.fromList [R {ul = (4,20), ll = (4,17), lr = (29,17), ur = (29,20)},R {ul = (17,50), ll = (17,4), lr = (30,4), ur = (30,50)},R {ul = (9,47), ll = (9,2), lr = (33,2), ur = (33,47)}]},
			Leaf {hv = 654, mbr = R {ul = (10,48), ll = (10,4), lr = (38,4), ur = (38,48)}, 
				rects = DS.fromList [R {ul = (18,39), ll = (18,15), lr = (34,15), ur = (34,39)},R {ul = (10,48), ll = (10,4), lr = (38,4), ur = (38,48)},R {ul = (25,29), ll = (25,29), lr = (31,29), ur = (31,29)}]}]},
		Branch {hv = 650, mbr = R {ul = (4,50), ll = (4,4), lr = (50,4), ur = (50,50)}, childs = DS.fromList [
			Leaf {hv = 686, mbr = R {ul = (10,50), ll = (10,6), lr = (50,6), ur = (50,50)}, 
				rects = DS.fromList [R {ul = (10,37), ll = (10,25), lr = (50,25), ur = (50,37)},R {ul = (21,50), ll = (21,6), lr = (39,6), ur = (39,50)},R {ul = (14,25), ll = (14,20), lr = (47,20), ur = (47,25)}]},
			Leaf {hv = 820, mbr = R {ul = (4,20), ll = (4,5), lr = (49,5), ur = (49,20)}, 
				rects = DS.fromList [R {ul = (9,19), ll = (9,5), lr = (33,5), ur = (33,19)},R {ul = (4,20), ll = (4,10), lr = (33,10), ur = (33,20)},R {ul = (47,15), ll = (47,11), lr = (49,11), ur = (49,15)}]},
			Leaf {hv = 1845, mbr = R {ul = (40,50), ll = (40,4), lr = (49,4), ur = (49,50)}, 
				rects = DS.fromList [R {ul = (43,50), ll = (43,8), lr = (49,8), ur = (49,50)},R {ul = (40,50), ll = (40,4), lr = (47,4), ur = (47,50)}]}]},
		Branch {hv = 2002, mbr = R {ul = (25,50), ll = (25,0), lr = (50,0), ur = (50,50)}, childs = DS.fromList [
			Leaf {hv = 1843, mbr = R {ul = (39,50), ll = (39,0), lr = (50,0), ur = (50,50)}, 
				rects = DS.fromList [R {ul = (41,50), ll = (41,0), lr = (44,0), ur = (44,50)},R {ul = (43,31), ll = (43,24), lr = (50,24), ur = (50,31)},R {ul = (39,23), ll = (39,17), lr = (44,17), ur = (44,23)}]},
			Leaf {hv = 1970, mbr = R {ul = (25,35), ll = (25,8), lr = (41,8), ur = (41,35)}, 
				rects = DS.fromList [R {ul = (25,24), ll = (25,8), lr = (39,8), ur = (39,24)},R {ul = (26,19), ll = (26,19), lr = (41,19), ur = (41,19)},R {ul = (29,35), ll = (29,17), lr = (35,17), ur = (35,35)}]},
			Leaf {hv = 2028, mbr = R {ul = (29,46), ll = (29,16), lr = (45,16), ur = (45,46)}, 
				rects = DS.fromList [R {ul = (34,37), ll = (34,18), lr = (41,18), ur = (41,37)},R {ul = (32,37), ll = (32,20), lr = (45,20), ur = (45,37)},R {ul = (29,46), ll = (29,16), lr = (35,16), ur = (35,46)}]}]}]},
	Branch {hv = 3430, mbr = R {ul = (3,50), ll = (3,15), lr = (50,15), ur = (50,50)}, childs = DS.fromList [
		Branch {hv = 2049, mbr = R {ul = (16,50), ll = (16,15), lr = (50,15), ur = (50,50)}, childs = DS.fromList [
			Leaf {hv = 2064, mbr = R {ul = (22,49), ll = (22,15), lr = (50,15), ur = (50,49)}, 
				rects = DS.fromList [R {ul = (22,48), ll = (22,37), lr = (49,37), ur = (49,48)},R {ul = (32,48), ll = (32,32), lr = (48,32), ur = (48,48)},R {ul = (34,49), ll = (34,15), lr = (50,15), ur = (50,49)}]},
			Leaf {hv = 2105, mbr = R {ul = (20,46), ll = (20,26), lr = (47,26), ur = (47,46)}, 
				rects = DS.fromList [R {ul = (22,38), ll = (22,26), lr = (42,26), ur = (42,38)},R {ul = (36,37), ll = (36,34), lr = (37,34), ur = (37,37)},R {ul = (20,46), ll = (20,26), lr = (47,26), ur = (47,46)}]},
			Leaf {hv = 3407, mbr = R {ul = (16,50), ll = (16,22), lr = (47,22), ur = (47,50)}, 
				rects = DS.fromList [R {ul = (38,44), ll = (38,22), lr = (47,22), ur = (47,44)},R {ul = (19,50), ll = (19,48), lr = (20,48), ur = (20,50)},R {ul = (16,48), ll = (16,48), lr = (38,48), ur = (38,48)}]}]},
		Branch {hv = 3433, mbr = R {ul = (3,50), ll = (3,15), lr = (47,15), ur = (47,50)}, childs = DS.fromList [
			Leaf {hv = 3390, mbr = R {ul = (13,48), ll = (13,32), lr = (47,32), ur = (47,48)}, 
				rects = DS.fromList [R {ul = (22,47), ll = (22,39), lr = (40,39), ur = (40,47)},R {ul = (13,48), ll = (13,33), lr = (47,33), ur = (47,48)},R {ul = (21,32), ll = (21,32), lr = (40,32), ur = (40,32)}]},
			Leaf {hv = 3429, mbr = R {ul = (15,50), ll = (15,15), lr = (40,15), ur = (40,50)}, 
				rects = DS.fromList [R {ul = (21,50), ll = (21,15), lr = (40,15), ur = (40,50)},R {ul = (15,42), ll = (15,33), lr = (40,33), ur = (40,42)},R {ul = (18,47), ll = (18,18), lr = (21,18), ur = (21,47)}]},
			Leaf {hv = 3533, mbr = R {ul = (3,45), ll = (3,38), lr = (33,38), ur = (33,45)}, 
				rects = DS.fromList [R {ul = (17,39), ll = (17,39), lr = (21,39), ur = (21,39)},R {ul = (3,44), ll = (3,42), lr = (33,42), ur = (33,44)},R {ul = (10,45), ll = (10,38), lr = (29,38), ur = (29,45)}]}]},
		Branch {hv = 3522, mbr = R {ul = (6,50), ll = (6,33), lr = (28,33), ur = (28,50)}, childs = DS.fromList [
			Leaf {hv = 3522, mbr = R {ul = (7,50), ll = (7,33), lr = (28,33), ur = (28,50)}, 
				rects = DS.fromList [R {ul = (12,42), ll = (12,39), lr = (28,39), ur = (28,42)},R {ul = (22,48), ll = (22,33), lr = (22,33), ur = (22,48)},R {ul = (7,50), ll = (7,45), lr = (28,45), ur = (28,50)}]},
			Leaf {hv = 3627, mbr = R {ul = (6,48), ll = (6,33), lr = (12,33), ur = (12,48)}, 
				rects = DS.fromList [R {ul = (6,48), ll = (6,33), lr = (12,33), ur = (12,48)}]}]}]}]}
rsearch = R {ul = (3,32), ll = (3,21), lr = (26,21), ur = (26,32)}
lrsearch = [R {ul = (1,35), ll = (1,23), lr = (22,23), ur = (22,35)},R {ul = (17,50), ll = (17,4), lr = (30,4), ur = (30,50)},R {ul = (9,47), ll = (9,2), lr = (33,2), ur = (33,47)},R {ul = (18,39), ll = (18,15), lr = (34,15), ur = (34,39)},R {ul = (10,48), ll = (10,4), lr = (38,4), ur = (38,48)},R {ul = (10,37), ll = (10,25), lr = (50,25), ur = (50,37)},R {ul = (21,50), ll = (21,6), lr = (39,6), ur = (39,50)},R {ul = (14,25), ll = (14,20), lr = (47,20), ur = (47,25)},R {ul = (25,24), ll = (25,8), lr = (39,8), ur = (39,24)},R {ul = (22,38), ll = (22,26), lr = (42,26), ur = (42,38)},R {ul = (20,46), ll = (20,26), lr = (47,26), ur = (47,46)},R {ul = (21,50), ll = (21,15), lr = (40,15), ur = (40,50)}]