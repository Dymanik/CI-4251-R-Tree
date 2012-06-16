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
	-- * Funciones exportadas.
		-- ** Permite la comparación de dos rectángulos según su número 
		-- de Hilbert
	orderHV,
		-- ** Agrega un nuevo rectángulo a la estructura. Insertar un
		-- rectángulo duplicado es causa de error.
	insert,
		-- ** Elimina un rectángulo presente en la estructura. Eliminar 
		-- un rectángulo inexistente es causa de error.
	delete,
		-- ** Consulta la estructura para determinar si el rectángulo 
		-- suministrado como parámetro se solapa con uno o más
		-- rectángulos en la estructura. El resultado de la función es 
		-- la lista de rectángulos solapados.
	search,
		-- ** Construye un RTree a partir de una lista de rectángulos.
	Point,
	fromList,
	fromList',
		-- ** Construye un Rectangle a partir de una lista de 8 enteros.
	makeRect4,
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

-- Hilbert Value
type HV = Int

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
	ul :: (Int, Int),	-- ^ Vertice superior izquierdo   (X0,Y0)
	ll :: (Int, Int),	-- ^ Vertice inferior izquierdo   (X0,Y1)
	lr :: (Int, Int),	-- ^ Vertice inferior derecho     (X1,Y1)
	ur :: (Int, Int)	-- ^ Vertice superior derecho     (X1,Y0)
} deriving (Show, Eq)

instance Ord Rectangle where
	compare r1 r2 = orderHV r1 r2


instance Arbitrary Rectangle where
	arbitrary = do
		x0 <- choose (0,65535)
		y0 <- choose (0,65535)
		x1 <- choose (x0,65536)
		y1 <- choose (y0,65536)
		return $ R (x0,y0) (x0,y1) (x1,y1) (x1,y0)


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
	compare t1 t2 =  case compare (hv t1) (hv t2) of
		

instance Arbitrary RTree where
	arbitrary = do
		rs <- suchThat (listOf1 $ (arbitrary :: Gen Rectangle)) f
		return $ fromList rs
		where
			f xs = length xs > 100


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
  @createRect@ permite construir un rectángulo a partir de una lista 
  de 8 enteros
 -}
makeRect4 :: [Int] -> Rectangle
makeRect4 [xa,ya,xb,yb,xc,yc,xd,yd] = R (xa,ya) (xb,yb) (xc,yc) (xd,yd)


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
			(min x10 x20, min y10 y20, max x11 x21, max y11 y21)
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
		roots' res bs = case null bs of
			True	  -> reverse res
			otherwise -> 
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
		leaves' res s  = leaves' ((makeLeaf $ DS.fromList rec) : res ) rest where
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
		l@(Leaf _ _ _) -> rearmar
		otherwise -> rearmar{hv=0}	-- Permite reconocer que ocurrió un split
	| otherwise = makeBranch leafs
	where
		rearmar :: RTree
		rearmar = fromList $ DS.toList $ DS.fold romper DS.empty leafs
		romper :: RTree -> DS.Set Rectangle -> DS.Set Rectangle
		romper hoja recs = DS.union (rects hoja) recs
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
insert Empty r = Right $ makeLeaf $ DS.singleton r
insert (Leaf _hv _rec rs) r 
	| DS.size (DS.insert r rs) == DS.size rs = throwError "DuplicateRectangle"
	| otherwise = Right $ makeLeaf $ DS.insert r rs
insert (Branch _hv _rec trees) r = case (elegirTree trees) of
	Right tree -> Right $ overflowHandling $ makeBranch $ reinsert tree
	otherwise -> throwError "DuplicateRectangle"
	where
		reinsert :: RTree -> DS.Set RTree
		reinsert a = DS.insert a (DS.delete (arbol trees) trees)
		elegirTree :: DS.Set RTree -> Either String RTree
		elegirTree seqt = insert (arbol seqt) r
		arbol :: DS.Set RTree -> RTree
		arbol cjto
			| DS.null (snd (DS.partition f cjto)) = DS.findMax cjto
			| otherwise = DS.findMin (snd (DS.partition f cjto))
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
	| DS.null eliminado = Right Empty
	| otherwise = Right (makeLeaf eliminado)
	where
		eliminado :: DS.Set Rectangle
		eliminado = DS.delete r rs

delete (Branch _hv _rec trees) r = case g of
	(null, Nothing) -> throwError "RectangleNotFound"
	([newTree], Just oldTree) -> Right $ noBranch $ rearmar newTree oldTree
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
		elimViejo ts ((Right _):xs) = Just (DS.findMin ts)
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
search Empty _ = Nothing
search (Leaf{rects=rs}) r
	| DS.null rs  = Nothing
	| otherwise = case (DS.toList $ DS.filter (intersects r) rs) of
		[] -> Nothing
		l -> return l
search (Branch{childs=rs,mbr=br}) r
	| intersects br r = F.foldMap (flip (search) r) rs 
	| otherwise = Nothing


intersects ::  Rectangle -> Rectangle -> Bool
intersects r2 r1 = ( ll r1 > ll r2 && ll r1 < ur r2) ||
				( lr r1 > ll r2 && lr r1 < ur r2) ||
				( ul r1 > ll r2 && ul r1 < ur r2) ||
				( ur r1 > ll r2 && ur r1 < ur r2) 

---------------------------------------------------------------------

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
					not (DS.member r (treeToSet t))
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
   -- q el eliminado ya no este
	
{-
  @prop_search@ representa a la propiedad que debe cumplirse al buscar
  solapamientos con un rectángulo en un árbol.
  
  Si al buscar un rectángulo se produce un Nothing quiere decir que el
  rectángulo no se encontraba en el árbol.
  
  Si se produce una lista de solapamientos entonces dicha lista es un 
  subconjunto del conjunto de rectángulos que conforman el árbol.
 -}
prop_search :: RTree -> Rectangle -> Bool
prop_search t r = case (search t r) of
	Just rs -> DS.isSubsetOf (DS.fromList rs) (treeToSet t)
	Nothing -> not $ DS.isSubsetOf (DS.singleton r) (treeToSet t)




--------------------------------------------------------------------------------


-- ARBOL DE PRUEBA
a = R {ul=(30,40), ll=(30,55), lr=(35,55), ur=(35,40)}
b = R {ul=(20,15), ll=(20,25), lr=(30,25), ur=(30,15)}
c = R {ul=(30,40), ll=(30,55), lr=(35,55), ur=(35,40)}
d = R {ul=(33,30), ll=(33,43), lr=(37,43), ur=(37,30)}
a2 = R {ul=(10,5), ll=(10,80), lr=(50,80), ur=(50,5)}
no = R {ul=(30,15), ll=(30,25), lr=(40,25), ur=(40,15)}
enlos2 = R (10,30) (10,80) (72,80) (72,30)
hoja1 = makeLeaf (DS.fromList [b,d,c])
papa1 = R {ul=(10,5), ll=(10,80), lr=(50,80), ur=(50,5)}
x = R {ul=(65,50), ll=(65,55), lr=(70,55), ur=(70,50)}
y = R {ul=(75,40), ll=(75,50), lr=(80,50), ur=(80,40)}
hoja2 = makeLeaf (DS.fromList [x,y])
papa2 = R {ul=(40,30), ll=(40,90), lr=(90,90), ur=(90,30)}
papa = R (0,0) (0,100) (100,100) (100,0)
arbolp = makeBranch (DS.fromList [
	makeBranch (DS.fromList [hoja1]),
	makeBranch (DS.fromList [hoja2])])

-- PRUEBAS PARA EL INSERT
e = R {ul=(21,16), ll=(21,26), lr=(31,26), ur=(31,16)}
f = R {ul=(22,17), ll=(22,27), lr=(32,27), ur=(32,17)}
g = R {ul=(23,18), ll=(23,28), lr=(33,28), ur=(33,18)}
h = R {ul=(24,19), ll=(24,29), lr=(34,29), ur=(34,19)}
i = R {ul=(25,20), ll=(25,30), lr=(35,30), ur=(35,20)}
j = R {ul=(26,21), ll=(26,31), lr=(36,31), ur=(36,21)}
k = R {ul=(27,22), ll=(27,32), lr=(37,32), ur=(37,22)}
l = R {ul=(28,23), ll=(28,33), lr=(38,33), ur=(38,23)}
m = R {ul=(29,24), ll=(29,34), lr=(39,34), ur=(39,24)}
n = R {ul=(30,25), ll=(30,35), lr=(40,35), ur=(40,25)}
o = R {ul=(31,26), ll=(31,36), lr=(41,36), ur=(41,26)}
p = R {ul=(32,27), ll=(32,37), lr=(42,37), ur=(42,27)}
q = R {ul=(33,28), ll=(33,38), lr=(43,38), ur=(43,28)}
r = R {ul=(34,29), ll=(34,39), lr=(44,39), ur=(44,29)}
s = R {ul=(35,30), ll=(35,40), lr=(45,40), ur=(45,30)}
t = R {ul=(36,31), ll=(36,41), lr=(46,41), ur=(46,31)}
h1 = makeLeaf $ DS.fromList [j,h,i,g]
h2 = makeLeaf $ DS.fromList [f,b,e,d]
h3 = makeLeaf $ DS.fromList [k,o,n,m]
h4 = makeLeaf $ DS.fromList [l,p,t,q]
h5 = makeLeaf $ DS.fromList [r,s,x,y]
overleaf = makeBranch $ DS.fromList [h1,h2,h3,h4,h5]






-- ERROR QUICKCHECK

arbolquick = (Branch {hv = 2270853427, 
						mbr = R {ul = (8570,24581), ll = (8570,63848), lr = (65356,63848), ur = (65356,24581)}, 
						childs = DS.fromList [
							Leaf {hv = 2373404487, 
									mbr = R {ul = (19335,24581), ll = (19335,54687), lr = (65356,54687), ur = (65356,24581)}, 
									rects = DS.fromList [R {ul = (27160,29187), ll = (27160,34173), lr = (27783,34173), ur = (27783,29187)},R {ul = (28549,24581), ll = (28549,52342), lr = (49843,52342), ur = (49843,24581)},R {ul = (19335,37180), ll = (19335,45843), lr = (65356,45843), ur = (65356,37180)},R {ul = (56762,51914), ll = (56762,54687), lr = (58172,54687), ur = (58172,51914)}]},
							Leaf {hv = 3124535267, 
									mbr = R {ul = (8570,39185), ll = (8570,63848), lr = (64784,63848), ur = (64784,39185)}, 
									rects = DS.fromList [R {ul = (63185,56100), ll = (63185,63848), lr = (64784,63848), ur = (64784,56100)},R {ul = (8570,39185), ll = (8570,46901), lr = (24846,46901), ur = (24846,39185)}]}]})


recquick = (R {ul = (22475,42128), ll = (22475,59797), lr = (30568,59797), ur = (30568,42128)})

   
arbdelete = (Leaf {hv = 1724648175, 
					mbr = R {ul = (62824,9643), ll = (62824,37579), lr = (63224,37579), ur = (63224,9643)}, 
					rects = DS.fromList [R {ul = (62824,9643), ll = (62824,37579), lr = (63224,37579), ur = (63224,9643)}]})
recdelete = (R {ul = (34083,4600), ll = (34083,42536), lr = (52021,42536), ur = (52021,4600)})
   
   
   
   
   --------------------------------------------------------------
