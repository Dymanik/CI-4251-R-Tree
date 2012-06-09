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
	insert,
		-- ** Elimina un rectángulo presente en la estructura. Eliminar 
		-- un rectángulo inexistente es causa de error.
	delete,
		-- ** Consulta la estructura para determinar si el rectángulo 
		-- suministrado como parámetro se solapa con uno o más
		-- rectángulos en la estructura. El resultado de la función es 
		-- la lista de rectángulos solapados
	search
	
) 
where

import qualified Data.Sequence as DS
import qualified Data.Foldable as F
import qualified Data.Maybe as DM
import Data.Bits

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
} deriving (Show, Eq, Ord)

-- permite comparar dos rectangulos segun su numero de hilbert
orderHV :: Rectangle -> Rectangle -> Ordering
orderHV r1 r2 = compare (hilbval r1) (hilbval r2)


splitPolicy = 2		--cuantos nodos vecinos deben estar llenos antes de hacer split
nodeCapacity = 4	--cuantos hijos puede tener un nodo
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
	Branch {hv::HV, rect::Rectangle, childs::(DS.Seq RTree)}	-- ^ Rama del árbol
	| Leaf (DS.Seq Rectangle)									-- ^ Hoja del árbol
	deriving (Show, Eq)

	
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


--insert :: RTree -> Rectangle -> Either e RTree
insert t r = growTree  

growTree :: [RTree] -> RTree
growTree [r]	= r 
growTree [r,x]	= 
	Branch (max (hv r) (hv x)) (boundingBox [rect r,rect x]) (DS.empty DS.|> r DS.|> x)


boundingBox :: (F.Foldable f) => f Rectangle -> Rectangle
boundingBox = F.foldr1 f
	where 
		f r1 r2 = buildR (g (ul r1) (ul r2) (lr r1) (lr r2)) 
		g (x10,y10) (x20,y20) (x11,y11) (x21,y21) = 
			(min x10 x20, max y10 y20, max x11 x21, min y11 y21)
		buildR (x0,y0,x1,y1) = R (x0,y0) (x0,y1) (x1,y0) (x1,y1)


--delete :: RTree -> Rectangle -> Either e RTree
delete = 0


----------------------------------------------------------
-- ARBOL DE PRUEBA
a = R {ul=(30,40), ll=(30,55), lr=(35,55), ur=(35,40)}
b = R {ul=(20,15), ll=(20,25), lr=(30,25), ur=(30,15)}
c = R {ul=(30,40), ll=(30,35), lr=(35,55), ur=(35,40)}
d = R {ul=(33,30), ll=(33,43), lr=(37,43), ur=(37,30)}
a2 = R {ul=(10,5), ll=(10,80), lr=(50,80), ur=(50,5)}
no = R {ul=(30,15), ll=(30,25), lr=(40,25), ur=(40,15)}
enlos2 = R (10,30) (10,80) (72,80) (72,30)
hoja1 = Leaf (DS.fromList [b,c,d])
papa1 = R {ul=(10,5), ll=(10,80), lr=(50,80), ur=(50,5)}
x = R {ul=(65,50), ll=(65,55), lr=(70,55), ur=(70,50)}
y = R {ul=(75,40), ll=(75,50), lr=(80,50), ur=(80,40)}
hoja2 = Leaf (DS.fromList [x,y])
papa2 = R {ul=(40,30), ll=(40,90), lr=(90,90), ur=(90,30)}
papa = R (0,0) (0,100) (100,100) (100,0)
arbol = Branch (hilbval papa) papa (DS.fromList [
	Branch (hilbval papa1) papa1 (DS.fromList [hoja1]),
	Branch (hilbval papa2) papa2 (DS.fromList [hoja2])])
--------------------------------------------------------------


search :: RTree -> Rectangle -> Maybe [Rectangle]
search (Leaf rs) r =  if (null f)
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
		g = concat $ DM.catMaybes $ F.toList $ fmap h trees
		h :: RTree -> Maybe [Rectangle]
		h tree = search tree r

-- Func internas
--chooseLeaf :: RTree-> Rectangle -> HV -> RTree
--chooseLeaf n@(Leaf seq) = n
--chooseLeaf (Branch hv seq) = case (:< seq) of
--	n@(Leaf _) -> chooseLeaf n
--	otherwise  -> chooseLeaf (:< (DS.dropWhileL )

--adjustTree
--overflowHandling
--
--
--





--
--
--
