{-|
  /Tarea 4/

  Programación Funcional Avanzada (CI4251)

  0.1 2012-06-13


  Johan González	07-40979
  Andreina García	08-10406


  Este módulo .
-}

module Main where

import RTrees


rtree :: String -> IO()
rtree s
	| null s = putStrLn "No se indicó base de datos inicial."
	| otherwise = do