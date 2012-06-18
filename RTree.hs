{-|
  /Hilbert R-Tree/

  Programación Funcional Avanzada (CI4251)

  0.1 2012-06-17


  Johan González	07-40979
  
  Andreina García	08-10406


  Este módulo implanta una estructura de datos que almacena y consulta 
  datos geométricos de rectángulos, dicha estructura es un R-Tree de 
  Hilbert.
 -}


module RTree (
	-- * Tipos exportados.
	Rectangle (..),
	RTree (..),
	Point (..),
	-- * Funciones sobre Rectangulos
		-- ** Constructores
		makeRect4,
		makeRect2,
		-- ** Comparacion
		intersects,
		orderHV,
	-- * Funciones sobre el Arbol
		-- ** Constructores.
	fromList,
	fromList',
		-- ** funciones basicas
	insert,
	delete,
	search,
		-- ** Pruebas
	test
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

{-
  Número de Hilbert para el centro de un rectángulo.
 -}
type HV = Int

{-
  Punto en el espacio de coordenadas X y Y.
 -}
type Point = (Int,Int)	-- ^ (X,Y)

{-
  El tipo de datos @Rectangle@ representa rectángulos de coordenadas 
  X y Y entre 0 y 65536 mediante sus vertices.

  Se declara derivando de @Show@ para facilitar la depuración
  y la creación de rectángulos "manualmente".

  Se declara instancia de @Eq@ pues en la implantación interna
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
	a == b = 
		(ul a == ul b) && (ll a == ll b) && (lr a == lr b) && (ur a == ur b)

instance Ord Rectangle where
	compare r1 r2
		| (hilbval r1) == (hilbval r2) = orderPoint r1 r2
		| otherwise = orderHV r1 r2

-- No se generan valores dentro de todo el rango posible (0 a 65536) 
-- para aumentar la posibilidad de coincidencias entre rectángulos.
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
	compare t1 t2
		| (hv t1) == (hv t2) = orderPoint (mbr t1) (mbr t2)
		| otherwise = compare (hv t1) (hv t2)

instance Arbitrary RTree where
	arbitrary = do
		rs <- suchThat (listOf1 $ (arbitrary :: Gen Rectangle)) f
		return $ fromList rs
		where
			f xs = length xs >= 0 && length xs <= 1000


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


{-|
  @orderHV@ establece la comparación de dos rectangulos segun su número 
  de Hilbert.
 -}
orderHV :: Rectangle -> Rectangle -> Ordering
orderHV r1 r2 = compare (hilbval r1) (hilbval r2)

{- 
  @orderPoint@ establece la comparación de dos rectangulos segun su posición
  en el espacio.
 -}
orderPoint :: Rectangle -> Rectangle -> Ordering
orderPoint r1 r2 = compare (ax0,ay0,ax0,ax1) (bx0,by0,bx0,bx1)
	where
		ax0 = fst (ll r1)
		bx0 = fst (ll r2)
		ay0 = snd (ll r1)
		by0 = snd (ll r2)
		ax1 = fst (ur r1)
		bx1 = fst (ur r2)
		ay1 = snd (ur r1)
		by1 = snd (ur r2)


{-|
  @makeRect4@ permite construir un rectángulo a partir de una lista 
  de 8 enteros.
 -}
makeRect4 :: [Int] -> Rectangle
makeRect4 [xa,ya,xb,yb,xc,yc,xd,yd] = R (xa,ya) (xb,yb) (xc,yc) (xd,yd)


{-|
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
				roots' ((makeBranch (DS.fromList(child))):res) rest where
				(child,rest) = splitAt nodeCapacity bs


{-|
  @fromList@ construye un árbol a partir de una lista de rectángulos.
 -}
fromList ::  [Rectangle] -> RTree
fromList = raiseTree . leaves . DL.sortBy orderHV
	where 
		leaves  = leaves' [] 
		leaves' res [] = reverse res
		leaves' res s  = leaves' ((makeLeaf (DS.fromList rec)):res ) rest where
			(rec,rest) = splitAt leafCapacity s

{-|
  @fromList'@  contruye un árbol a partir de un conjunto de rectángulos
  utilizando la operación de @insert@.
 -}
fromList' ::  F.Foldable t => t Rectangle -> RTree
fromList' = F.foldl' (f) Empty where
	f t r = either (\x ->t) (id) (insert t r)


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


{-|
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


{-|
  @delete@ elimina un rectángulo de un árbol.
  
  Si el rectángulo se encuentra en el árbol se retorne un nuevo arbol
  que no contiene al rectángulo. 
  
  En caso contrario reporta un error de rectángulo no encontrado.

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



{-|
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


{- |
  @intersects@ determina si dos rectángulos se intersectan.
 -}
intersects ::  Rectangle -> Rectangle -> Bool
intersects r1 r2 = ins r1 r2 || ins r2 r1
	where
		ins b a =((bx0 < ax1 && ax1 <= bx1) && 
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
			(hilbval (DS.findMax (menor tree)) <= hilbval r)
		else if (not (DS.null (mayor tree)))
			then hilbval (DS.findMin (mayor tree)) >= hilbval r
			else hilbval (DS.findMax (menor tree)) <= hilbval r
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


{- |
  @test@ ejecuta todas las pruebas de quickcheck.
 -}
test :: IO()
test = do
	putStrLn "member insert"
	quickCheck prop_member_insert
	putStrLn "ord insert"
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
	putStrLn "Done!"


