{-|
  /Tarea 4/

  Programación Funcional Avanzada (CI4251)

  0.1 2012-06-13


  Johan González	07-40979
  Andreina García	08-10406


  Este módulo .
-}

--module Main where

import RTree
import qualified Data.Sequence as DS
import Data.List.Split
import qualified Data.List as DL



numericMatrix :: [[String]] -> [[Int]]
numericMatrix [] = []
numericMatrix (x:xs) = (map read x):(numericMatrix xs)

goodR :: [Int] -> Bool
goodR [xa,ya,xb,yb,xc,yc,xd,yd] =	xa==xb && xc==xd && ya==yd && yb==yc &&
									xa>=0 && xa<=65536 && xc>=0 && xc<=65536 &&
									ya>=0 && ya<=65536 && yb>=0 && yb<=65536

createRect :: [Int] -> Rectangle
createRect [xa,ya,xb,yb,xc,yc,xd,yd] = R (xa,ya) (xb,yb) (xc,yc) (xd,yd)

--create :: Rectangle -> RTree -> RTree
--create r t = insert t r

rtree :: String -> IO ()
rtree arch
	| null arch = do
		putStrLn "No se indicó base de datos inicial."
		--return $ Leaf $ DS.empty
	| otherwise = do
		content <- readFile arch
		let badRects = numericMatrix $ map (splitOn ",") (lines content)
		let goodRects = filter goodR badRects
		putStrLn $ "Leídos " ++ show (length goodRects) ++
					" rectángulos desde el archivo " ++ arch
		-- nub es n cuadrado... mejorar si se puede
		let rects = DL.nub (DL.sortBy orderHV (map createRect goodRects))
		putStrLn $ show rects
		--return $ foldl create (Leaf $ DS.empty) rects

