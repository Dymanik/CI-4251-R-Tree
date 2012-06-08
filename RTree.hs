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


module RTrees (
	-- * Tipos exportados.
		-- ** Rectángulos.
	Rectangle (..),
		-- ** Árbol de almacenamiento y consulta de rectángulos.
	RTree (..),
	-- * Funciones exportadas.
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
import Data.Bits

type Point = (Int,Int)
type HV = Int

{-
  El tipo de datos @Rectangle@ representa rectangulos de coordenadas 
  X y Y entre 0 y 65536 mediante sus vertices.

  Se declara derivando de @Show@ para facilitar la depuración
  y la creación de rectángulos "manualmente".

  Se declara derivando de @Eq@ pues en la implantación interna
  del R-Tree es necesario comparar rectángulos.
-}
data Rectangle = R {
	ul :: (Int, Int),	-- ^ Vertice superior izquierdo   (X0,Y0)
	ll :: (Int, Int),	-- ^ Vertice inferior izquierdo   (X0,Y1)
	lr :: (Int, Int),	-- ^ Vertice inferior derecho     (X1,Y1)
	ur :: (Int, Int)	-- ^ Vertice superior derecho     (X1,Y0)
} deriving (Show, Eq)


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
data RTree = Branch {hv::HV, rect::Rectangle, childs::(DS.Seq RTree)}	-- ^ Rama del árbol
			| Leaf (DS.Seq Rectangle)				-- ^ Hoja del árbol
			deriving (Show, Eq)


--insert :: RTree -> Rectangle -> Either e RTree
insert t r = growTree  

growTree :: [RTree] -> RTree
growTree [r]	=	r 
growTree [r,x]	=	Branch (max (hv r) (hv x)) (boundingBox [rect r,rect x]) (DS.empty DS.|> r DS.|> x)


boundingBox :: (F.Foldable f) => f Rectangle -> Rectangle
boundingBox s = F.foldr1 f s
		where 
			f r1 r2 = buildR (g (ul r1) (ul r2) (lr r1) (lr r2)) 
			g (x10,y10) (x20,y20) (x11,y11) (x21,y21) = (min x10 x20,max y10 y20,max x11 x21, min y11 y21)
			buildR (x0,y0,x1,y1) = R (x0,y0) (x0,y1) (x1,y0) (x1,y1)


--delete :: RTree -> Rectangle -> Either e RTree
delete = 0

--search :: RTree -> Rectangle -> Maybe [Rectangle]
search = 0

-- Func internas
--chooseLeaf :: RTree-> Rectangle -> HV -> RTree
--chooseLeaf n@(Leaf seq) = n
--chooseLeaf (Branch hv seq) = case (:<seq) of
--			n@(Leaf _) -> chooseLeaf n
--			otherwise  -> chooseLeaf (:< (DS.dropWhileL )

--adjustTree
--overflowHandling
--
--
--



--el valor de Hilbert para el centro de un rectangulo
hilbval:: Rectangle -> Int
hilbval (R _ (llx,lly) _ (urx,ury))  = hilbertValue 17 ((llx+urx) `div` 2,(ury+lly) `div` 2)


--el valor de Hilbert para un punto cualquiera
hilbertValue :: (Bits a, Ord a) => Int -> (a,a) -> a
hilbertValue d (x,y)
	| x < 0 || x >= 1 `shiftL` d = error "x bounds"
	| y < 0 || y >= 1 `shiftL` d = error "y bounds"
	| otherwise = dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y
		where
			dist 0 _ result _ _ = result
			dist side area result x y = case (compare x side, compare y side) of
				(LT, LT) -> step result y x
				(LT, _)  -> step (result + area) x (y - side)
				(_, LT)  -> step (result + area * 3) (side - y - 1) (side * 2 - x - 1)
				(_, _)   -> step (result + area * 2) (x - side) (y - side)
				where step = dist (side `shiftR` 1) (area `shiftR` 2)

--
--
--
