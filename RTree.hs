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
import Data.Bits

type Point = (Int,Int);

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
data RTree = Branch Int Rectangle (DS.Seq RTree)	-- ^ Rama del árbol
			| Leaf (DS.Seq Rectangle)				-- ^ Hoja del árbol
			deriving (Show, Eq)


--insert :: RTree -> Rectangle -> Either e RTree


--delete :: RTree -> Rectangle -> Either e RTree


--search :: RTree -> Rectangle -> Maybe [Rectangle]

-- Func internas
--chooseLeaf :: Rectangle -> Int -> RTree
--adjustTree
--overflowHandling
--
--
--



--el valor de Hilbert para el centro de un rectangulo
hv:: Rectangle -> Int
hv (R _ (llx,lly) _ (urx,ury))  = hilbertDistance 17 ((llx+urx) `div` 2,(ury+lly) `div` 2)


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
