{-|
  /Hilbert R-Tree/

  Programación Funcional Avanzada (CI4251)

  0.1 2012-06-13


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
	--insert,
		-- ** Elimina un rectángulo presente en la estructura. Eliminar 
		-- un rectángulo inexistente es causa de error.
	delete,
		-- ** Consulta la estructura para determinar si el rectángulo 
		-- suministrado como parámetro se solapa con uno o más
		-- rectángulos en la estructura. El resultado de la función es 
		-- la lista de rectángulos solapados
	search,

	fromList,
	createRect	
) 
where

import qualified Data.Set as DS
import qualified Data.Foldable as F
import qualified Data.Maybe as DM
import Data.Bits
import qualified Data.List as DL
import Data.Either
import Control.Monad.Error

type Point = (Int,Int)

-- Hilbert Value
type HV = Int

{-
  El tipo de datos @Rectangle@ representa rectangulos de coordenadas 
  X y Y entre 0 y 65536 mediante sus vertices.

  Se declara derivando de @Show@ para facilitar la depuración
  y la creación de rectángulos "manualmente".

  Se declara derivando de @Eq@ pues en la implantación interna
  del R-Tree es necesario comparar rectángulos.
  
  Se declara derivando de @Ord@ pues es necesario ordenar rectángulos.
-}
data Rectangle = R {
	ul :: (Int, Int),	-- ^ Vertice superior izquierdo   (X0,Y0)
	ll :: (Int, Int),	-- ^ Vertice inferior izquierdo   (X0,Y1)
	lr :: (Int, Int),	-- ^ Vertice inferior derecho     (X1,Y1)
	ur :: (Int, Int)	-- ^ Vertice superior derecho     (X1,Y0)
} deriving (Show, Eq)

instance Ord Rectangle where
	compare r1 r2 = orderHV r1 r2

-- permite comparar dos rectangulos segun su numero de hilbert
orderHV :: Rectangle -> Rectangle -> Ordering
orderHV r1 r2 = compare (hilbval r1) (hilbval r2)


splitPolicy = 2		--cuantos nodos vecinos deben estar llenos antes de hacer split
nodeCapacity = 5--cuantos hijos puede tener un nodo
leafCapacity = 4	--cuantos rectangulos puede guardar una hoja


{-
  El tipo de datos @RTree@ representa un R-Tree de Hilbert que almacena
  rectángulos y permite consultarlos.

  Se declara derivando de @Show@ para facilitar la depuración
  y la creación de árboles "manualmente".

  Se declara derivando de @Eq@ pues en la implantación interna
  del R-Tree es necesario comparar árboles.
-}
data RTree = 
	Branch {hv::HV, mbr::Rectangle, childs::(DS.Set RTree)}	-- ^ Rama del árbol
	| Leaf {hv::HV, mbr::Rectangle, rects::(DS.Set Rectangle)}	-- ^ Hoja del árbol
	| Empty														-- ^ Árbol vacio
	deriving (Show, Eq)

instance Ord RTree where
	compare t1 t2 = compare (hv t1) (hv t2)


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

--Constructores
createRect :: [Int] -> Rectangle
createRect [xa,ya,xb,yb,xc,yc,xd,yd] = R (xa,ya) (xb,yb) (xc,yc) (xd,yd)

makeLeaf ::  DS.Set Rectangle -> RTree
makeLeaf ls = Leaf (hilbval	br) (br) (ls)
	where
		br = boundingBox ls

makeBranch ::  DS.Set RTree -> RTree
makeBranch ls = Branch (hilbval br) (br) (ls)
	where
		br = boundingBox (DS.map mbr ls)

raiseTree ::  [RTree] -> RTree
raiseTree ls  = case length ls of
	0	-> Empty
	1 	-> head ls
	otherwise -> raiseTree $ roots ls where
		roots = roots' []
		roots' res bs = case null bs of
			True	  -> reverse res
			otherwise -> roots' ((makeBranch (DS.fromList(child))) : res) rest where
				(child,rest) = splitAt nodeCapacity bs


--fromList ::  [Rectangle] -> RTree
fromList = raiseTree . leaves . DL.sortBy orderHV
	where 
		leaves  = leaves' [] 
		leaves' res [] = reverse res
		leaves' res s  = leaves' ((makeLeaf $ DS.fromList rec) : res ) rest where
			(rec,rest) = splitAt leafCapacity s





--el valor de Hilbert para el centro de un rectangulo
hilbval:: Rectangle -> Int
hilbval (R _ (llx,lly) _ (urx,ury)) = 
	hilbertValue 17 ((llx+urx) `div` 2,(ury+lly) `div` 2)


--el valor de Hilbert para un punto cualquiera
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



boundingBox :: (F.Foldable f) => f Rectangle -> Rectangle
boundingBox = F.foldr1 f
	where 
		f r1 r2 = buildR (g (ul r1) (ul r2) (lr r1) (lr r2)) 
		g (x10,y10) (x20,y20) (x11,y11) (x21,y21) = 
			(min x10 x20, min y10 y20, max x11 x21, max y11 y21)
		buildR (x0,y0,x1,y1) = R (x0,y0) (x0,y1) (x1,y1) (x1,y0)


------------------------------------------------------------

-- Auxiliar para overflowHandling
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


overflowHandling :: RTree -> RTree
overflowHandling (Branch _hv _rec ts) = overflow (DS.findMin ts) ts


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
		arbol cjto = DS.findMin (snd (DS.partition f cjto))
		f :: RTree -> Bool
		f tree = hv tree < hilbval r


--------------------------------------------------------

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

----------------------------------------------------------
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





--------------------------------------------------------------


search :: RTree -> Rectangle -> Maybe [Rectangle]
search (Leaf _ _ rs) r =  if (null f)
	then Nothing
	else Just f
	where
		f :: [Rectangle]
		f = F.toList $ DS.filter overlapped rs
		overlapped :: Rectangle -> Bool
		overlapped rec = (((fst (ul rec) < fst (ur r)) && 
						(fst (ur r) <= fst (ur rec)) ||
						(fst (ul rec) <= fst (ul r)) && 
						(fst (ul r) < fst (ur rec))) &&
						(((snd (ul rec) <= snd (ul r)) && 
							(snd (ul r) < snd (ll rec))) || 
							((snd (ul rec) < snd (ll r)) && 
							(snd (ll r) <= snd (ll rec))))) ||
						(((fst (ul r) < fst (ur rec)) && 
						(fst (ur rec) <= fst (ur r)) ||
						(fst (ul r) <= fst (ul rec)) && 
						(fst (ul rec) < fst (ur r))) &&
						(((snd (ul r) <= snd (ul rec)) && 
							(snd (ul rec) < snd (ll r))) || 
							((snd (ul r) < snd (ll rec)) && 
							(snd (ll rec) <= snd (ll r)))))
search (Branch _hv _rec trees) r = if (null g)
	then Nothing
	else Just g
	where
		g :: [Rectangle]
		g = concat $ DM.catMaybes $ F.toList $ DS.map h trees
		h :: RTree -> Maybe [Rectangle]
		h tree = search tree r