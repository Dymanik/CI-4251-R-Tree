{-|
  /Tarea 4/

  Programación Funcional Avanzada (CI4251)

  0.1 2012-06-17


  Johan González	07-40979
  
  Andreina García	08-10406


  Este archivo contiene la aplicación de interacción con el usuario.
  
  Recibe como argumento de línea de comando el nombre de un archivo
  de texto que contenga una colección inicial de rectángulos a cargar
  en la base de datos. Si no se especifica ningún archivo se tendrá
  una base de datos inicial vacía.
  
  Luego el usuario podrá indicar operaciones a efectuar sobre la base
  de datos: agregar un rectángulo, eliminar un rectángulo, buscar
  rectángulos que se solpane con uno dado y terminar la ejecuión de la
  aplicación.
  
  Al finalizar la aplicación se mostrará un resumen de su ejecución, 
  indicando operaciones exitosas y erróneas.
-}

module Main (
	-- * Funciones exportadas.
		-- ** Comienza la ejecución de la aplicación.
	main
) where

import RTree
--import Data.List.Split
--import qualified Data.List.Ordered as DLO
import Text.ParserCombinators.Parsec
import Control.Applicative((<$>),(<*),(*>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Environment
import System.IO(hFlush,stdout)


{-
  El tipo de datos @Cmd@ representa los posibles comandos indicados
  por el usuario.
 -}
data Cmd = 
	Insert Rectangle		-- ^ Agregar un rectángulo.
	| Delete Rectangle		-- ^ Eliminar un rectángulo.
	| Search Rectangle Int	-- ^ Buscar N solapamientos con un rectángulo.
	| BadCmd				-- ^ Comando inválido.
	| Exit					-- ^ Salir de la aplicación.


{-
  El tipo de datos @Stats@ representa el estado actual de la base
  de datos en un momento dado durante la ejecución de la aplicación.

  Se declara instancia de @Show@ para informar correctamente al usuario
  el reporte final de la ejecución de la aplicación sobre la base de datos.
 -}
data Stats = Stats {
	modified::Bool,		-- ^ La base de datos fue modificada.
	goodSearches::Int,	-- ^ Total de búsquedas exitosas sobre la base de datos inicial.
	badSearches::Int,	-- ^ Total de búsquedas erróneas sobre la base de datos inicial.
	modTree::RTree,		-- ^ Base de datos modificada.
	mgoodSearches::Int,	-- ^ Total de búsquedas exitosas sobre la base de datos modificada.
	mbadSearches::Int,	-- ^ Total de búsquedas erróneas sobre la base de datos modificada.
	mgoodInserts::Int,	-- ^ Total de inserciones exitosas sobre la base de datos modificada.
	mbadInserts::Int,	-- ^ Total de inserciones erróneas sobre la base de datos modificada.
	mgoodDeletes::Int,	-- ^ Total de eliminaciones exitosas sobre la base de datos modificada.
	mbadDeletes::Int	-- ^ Total de eliminaciones erróneas sobre la base de datos modificada.
}

instance Show Stats where
	show s@(Stats {modified=False}) = 
		"Busquedas exitosas: " ++ (show $ goodSearches s) ++
		" Busquedas sin solapamientos: " ++ (show $ badSearches s)
	show s@(Stats {modified=True}) =  
		"\nBase de datos original:\n\tBusquedas exitosas: " ++
		(show $ goodSearches s) ++ "\n\tBusquedas sin solapamientos: " ++
		(show $ badSearches s) ++ "\n\nBase de datos modificada:\n" ++
		"\tBusquedas exitosas: " ++ (show $ mgoodSearches s) ++ 
		"\n\tBusquedas sin solapamientos: " ++ (show $ mbadSearches s) ++ 
		"\n\tInserciones exitosas: " ++ (show $ mgoodInserts s) ++ 
		"\n\tInserciones duplicadas: " ++ (show $ mbadInserts s) ++ 
		"\n\tEliminaciones exitosas: " ++ (show $ mgoodDeletes s) ++ 
		"\n\tEliminaciones inexistentes: " ++ (show $ mbadDeletes s)


{-
  @newStats@ crea un nuevo Stats inicial.
 -}
newStats ::  Stats
newStats = Stats False 0 0 Empty 0 0 0 0 0 0


{-
  @parseRects@   ???
 -}
parseRects :: String -> Either ParseError [Rectangle]
parseRects s = 
	either (Left) (return . f)	(parse (many filerects) "" s) where
			f = foldr ((++) . g) []
			g r = case goodR ( h r) of
				True -> [makeRect4 (h r)]
				otherwise -> []
			h = map read

{-
  @filerects@   ???
 -}
 -- filerects ::   ???
filerects = (sepBy1 (many1 digit) (comma)) <* newline


{-
  @comma@ reconoce una coma.
 -}
-- comma ::   ???
comma = (char ',')


{-
  @numericMatrix@ convierte una matriz de números en su forma de
  String a una matriz de números enteros.
 -}
numericMatrix :: [[String]] -> [[Int]]
numericMatrix [] = []
numericMatrix (x:xs) = (map read x):(numericMatrix xs)


{-
  @goodR@ verifica que una lista de 8 enteros puedan formar un 
  rectángulo correctamente.
 -}
goodR :: [Int] -> Bool
goodR [xa, ya, xb, yb, xc, yc, xd, yd] =	
	(xa == xb) && (xc == xd) && (ya == yd) && (yb == yc) && xa >= 0 && 
	xa <= 65536 && xc >= 0 && xc <= 65536 && ya >= 0 && ya <= 65536 && 
	yb >= 0 && yb <= 65536


{-
  @parseCmd@ reconoce comandos.
 -}
parseCmd ::  String -> Either ParseError Cmd
parseCmd s = parse (cmdInst) "" s


{-
  @cmdInst@   ???
 -}
-- cmdInst ::   ???
cmdInst = choice [cmdDelete, cmdInsert, cmdSearch, cmdExit]

{-
  @cmdDelete@   ???
 -}
-- cmdDelete ::   ???
cmdDelete = liftM Delete (string "delete " *> cmdRect <* spaces)

{-
  @cmdInsert@   ???
 -}
-- cmdInsert ::   ???
cmdInsert = liftM Insert (string "insert " *> cmdRect <* spaces)

{-
  @cmdSearch@   ???
 -}
-- cmdSearch ::   ???
cmdSearch = 
	liftM2 Search (string "search " *> cmdRect) (read <$> cmdNum <* spaces)

{-
  @cmdExit@   ???
 -}
-- cmdExit ::   ???
cmdExit = string "kthxbye" >> spaces >> return Exit

{-
  @cmdNum@   ???
 -}
-- cmdNum ::   ???
cmdNum = spaces *> many1 digit

{-
  @cmdRect@   ???
 -}
-- cmdRect ::   ???
cmdRect = liftM2 makeRect2 (cmdPoint) (cmdPoint)

{-
  @cmdPoint@   ???
 -}
-- cmdPoint ::   ???
cmdPoint = liftM2 (,) (read <$> cmdNum) (read <$> cmdNum)


{-
  @printHelp@   ???
 -}
-- printHelp :: IO ()
printHelp = do
	putStrLn "\nComandos disponibles: "
	putStrLn "\tsearch x0 y0 x1 y1 n"
	putStrLn "\tinsert x0 y0 x1 y1"
	putStrLn "\tdelete x0 y0 x1 y1"
	putStrLn "\tkthxbye\n"


{-
  @mainLoop@   ???
 -}
mainLoop :: ReaderT RTree (StateT Stats IO) ()
mainLoop = do
	liftIO $ putStr "\n>" *> hFlush stdout
	l <- liftIO getLine
	st <- lift get
	if modified st 
		then 
			case parseCmd l of
				Right (Search r n) -> do
					liftIO $ putStrLn "Busqueda:"
					res <- asks (flip search r)
					case res of 
						Nothing -> do
							liftIO $ putStrLn "sin resultados"
							(lift $ put st{badSearches=badSearches(st)+1})
						Just x -> do
							liftIO $ putStrLn $ "se encontraron "++show (length x)++" rectangulos"
							liftIO $ print $ take n x
							(lift $ put st{goodSearches=goodSearches(st)+1})
					stm <- lift $ get 
					case search (modTree(stm)) r of
						Nothing -> do
							liftIO $ putStrLn "Base de datos modificada: sin resultados"
							(lift $ put stm{mbadSearches=mbadSearches(stm)+1})
						Just x -> do
							liftIO $ putStrLn $ "Base de datos modificada: se encontraron "++show (length x)++" rectangulos"
							liftIO $ print $ take n x
							(lift $ put stm{mgoodSearches=mgoodSearches(stm)+1})
					mainLoop
				Right (Insert r) -> do
					case insert (modTree st) r of 
						Right x -> do
							liftIO $ putStrLn "Insercion exitosa"
							(lift $ put st{modTree=x, mgoodInserts=mgoodInserts(st)+1,modified=True})
						Left x -> do
							liftIO $ print x
							lift $ put st{mbadInserts=mbadInserts(st)+1}
					mainLoop
				Right (Delete r) ->do
					case delete (modTree st) r of 
						Right x -> do
							liftIO $ putStrLn "Eliminacion exitosa"
							lift $ put st{modTree=x, mgoodDeletes=mgoodDeletes(st)+1,modified=True}
						Left x -> do
							liftIO $ print x
							lift $ put st{mbadDeletes=mbadDeletes(st)+1}
					mainLoop
				Right Exit -> do 
					st <- lift get 
					liftIO $ putStrLn $ show st 
					liftIO $ putStrLn "La aplicacion se ha cerrado.\n"
				Left x	-> do
					liftIO $ print x
					liftIO $ printHelp
					mainLoop
		else  
			case parseCmd l of
				Right (Search r n) -> do
					liftIO $ putStrLn "Busqueda:"
					res <- asks (flip search r)
					case res of 
						Nothing -> do
							liftIO $ putStrLn "sin resultados"
							(lift $ put st{badSearches=badSearches(st)+1})
						Just x -> do
							liftIO $ putStrLn $ "se encontraron "++show (length x)++" rectangulos"
							liftIO $ print $ take n x
							(lift $ put st{goodSearches=goodSearches(st)+1})
					mainLoop
				Right (Insert r) -> do
					res <- asks (flip insert r)
					case res of 
						Right x -> do
							liftIO $ putStrLn "Insercion exitosa"
							(lift $ put st{modTree=x, mgoodInserts=mgoodInserts(st)+1,modified=True})
						Left x -> do
							liftIO $ print x
							lift $ put st{mbadInserts=mbadInserts(st)+1}
					mainLoop
				Right (Delete r) ->do
					res <- asks (flip delete r)
					case res of 
						Right x -> do
							liftIO $ putStrLn "Eliminacion exitosa"
							lift $ put st{modTree=x, mgoodDeletes=mgoodDeletes(st)+1,modified=True}
						Left x -> do
							liftIO $ print x
							lift $ put st{mbadDeletes=mbadDeletes(st)+1}
					mainLoop
				Right Exit -> do 
					st <- lift get 
					liftIO $ putStrLn $ show st 
					liftIO $ putStrLn "La aplicacion se ha cerrado.\n"
				Left x	-> do
					liftIO $ print x
					liftIO $ printHelp
					mainLoop

{-
  @main@   ???
 -}
-- main :: IO ()
main = do
	a <- getArgs
	sTree <- loadFile a
	printHelp
	runStateT (runReaderT mainLoop sTree) newStats


{-
  @loadFile@ carga la base de datos inicial desde un archivo.
  
  Si no se especifica un archivo u ocurre un error al intentar leer el
  archivo especificado, la base de datos inicial estará vacía.
  
  Si no, la base de datos contendrá los rectángulos especificados 
  correctamente en el archivo.
 -}
loadFile:: [String] -> IO RTree
loadFile [] = 
	putStrLn "No se especifico la base de datos inicial\n" >> return Empty
loadFile a = do
	f <- readFile (head a)
	either (e) (ld) (parseRects f) where
		ld rs = do
			putStrLn ("Leidos "++show (length rs) ++" rectangulos desde el archivo "++head a++"\n")
			return (fromList rs)
		e err = do
			putStrLn ("Error leyendo el archivo\n")
			putStrLn $ show err
			return Empty

