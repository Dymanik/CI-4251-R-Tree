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
import qualified Data.List.Ordered as DLO
import Text.ParserCombinators.Parsec
import Control.Applicative((<$>),(<*),(*>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Environment
import System.IO(hFlush,stdout)

data Cmd = Insert Rectangle
		| Delete Rectangle
		| Search Rectangle Int
		| BadCmd
		| Exit

data Stats = Stats {
						modified::Bool,
						goodSearches::Int,
						badSearches::Int,
						modTree::RTree,
						mgoodSearches::Int,
						mbadSearches::Int,
						mgoodInserts::Int,
						mbadInserts::Int,
						mgoodDeletes::Int,
						mbadDeletes::Int
				}

instance Show Stats where
	show s@(Stats { modified=False}) = "Succesful Searches: "++(show $ goodSearches s )
									++ " Unsuccesful Searches: "++(show $ badSearches s )
	show s@(Stats { modified=True}) =  "Original Database:\n"
									++ "Succesful Searches: "++(show $ goodSearches s )
									++ " Unsuccesful Searches: "++(show $ badSearches s )
									++ "\nModified Database:\n"
									++ " Succesful Searches: "++(show $ mgoodSearches s )
									++ " Unsuccesful Searches: "++(show $ mbadSearches s )
									++ " Succesful Inserts: "++(show $ mgoodInserts s )
									++ " Unsuccesful Inserts: "++(show $ mbadInserts s )
									++ " Succesful Deletes: "++(show $ mgoodDeletes s )
									++ " Unsuccesful Deletes: "++(show $ mbadDeletes s )

newStats ::  Stats
newStats = Stats False 0 0 Empty 0 0 0 0 0 0

--parseRect
parseRects :: String -> Either ParseError [Rectangle]
parseRects s = either (Left) (return . f)	(parse (many filerects) "" s) where
			f = foldr ((++) . g) []
			g r = case goodR ( h r) of
				True -> [makeRect4 (h r)]
				otherwise -> []
			h = map read

filerects = (sepBy1 (many1 digit) (comma)) <* newline

readInt ::  String -> Int
readInt s = read s::Int

comma = (char ',')



numericMatrix :: [[String]] -> [[Int]]
numericMatrix [] = []
numericMatrix (x:xs) = (map read x):(numericMatrix xs)



goodR :: [Int] -> Bool
goodR [xa,ya,xb,yb,xc,yc,xd,yd] =	xa==xb && xc==xd && ya==yd && yb==yc &&
									xa>=0 && xa<=65536 && xc>=0 && xc<=65536 &&
									ya>=0 && ya<=65536 && yb>=0 && yb<=65536







parseCmd ::  String -> Either ParseError Cmd
parseCmd s = parse (cmdInst) "" s

cmdInst = choice [cmdDelete,
				cmdInsert,
				cmdSearch,
				cmdExit
				]


cmdDelete = liftM Delete (string "delete " *> cmdRect <* spaces)
cmdInsert = liftM Insert (string "insert " *> cmdRect <* spaces) 
cmdSearch = liftM2 Search (string "search " *> cmdRect) (read <$> cmdNum <* spaces)
cmdExit = string "kthxbye">>spaces >> return Exit

cmdNum = spaces *> many1 digit

cmdRect = liftM2 makeRect2 (cmdPoint) (cmdPoint)
cmdPoint = liftM2 (,)  (read <$> cmdNum) (read <$> cmdNum)


printHelp = do
			putStrLn "Commands Available: "
			putStrLn "\tsearch x0 y0 x1 y1 n"
			putStrLn "\tinsert x0 y0 x1 y1"
			putStrLn "\tdelete x0 y0 x1 y1"
			putStrLn "\tkthxbye\n"


mainLoop ::  ReaderT RTree (StateT Stats IO) ()
mainLoop = do
		liftIO $ putStr ">" *> hFlush stdout
		l <- liftIO getLine
		st <- lift get
		if modified st 
			then 
				case parseCmd l of
					Right (Search r n) -> do
						liftIO $ putStrLn "Search"
						res <- asks (flip search r)
						case res of 
							Nothing -> do
								liftIO $ putStrLn "Nothing found"
								(lift $ put st{badSearches=badSearches(st)+1})
							Just x -> do
								liftIO $ putStrLn $ "found "++show (length x)++" rectangles"
								liftIO $ print $ take n x
								(lift $ put st{goodSearches=goodSearches(st)+1})
						stm<- lift $ get 
						case search (modTree(stm)) r of
							Nothing -> do
								liftIO $ putStrLn "Modified database: Nothing found"
								(lift $ put stm{mbadSearches=mbadSearches(stm)+1})
							Just x -> do
								liftIO $ putStrLn $ "Modified database: found "++show (length x)++" rectangles"
								liftIO $ print $ take n x
								(lift $ put stm{mgoodSearches=mgoodSearches(stm)+1})
						mainLoop
					Right (Insert r) -> do
						case insert (modTree st) r of 
							Right x -> do
								liftIO $ putStrLn "insert succesful"
								(lift $ put st{modTree=x, mgoodInserts=mgoodInserts(st)+1,modified=True})
							Left x -> do
								liftIO $ print x
								lift $ put st{mbadInserts=mbadInserts(st)+1}
						mainLoop
					Right (Delete r) ->do
						case delete (modTree st) r of 
							Right x -> do
								liftIO $ putStrLn "delete succesful"
								lift $ put st{modTree=x, mgoodDeletes=mgoodDeletes(st)+1,modified=True}
							Left x -> do
								liftIO $ print x
								lift $ put st{mbadDeletes=mbadDeletes(st)+1}
						mainLoop
					Right Exit -> do 
						st <- lift get 
						liftIO $ putStrLn $ show st 
						liftIO $ putStrLn "Exit"
					Left x	-> do
						liftIO $ print x
						liftIO $ printHelp
						mainLoop
			else  
				case parseCmd l of
					Right (Search r n) -> do
						liftIO $ putStrLn "Search"
						res <- asks (flip search r)
						case res of 
							Nothing -> do
								liftIO $ putStrLn "Nothing found"
								(lift $ put st{badSearches=badSearches(st)+1})
							Just x -> do
								liftIO $ putStrLn $ "found "++show (length x)++" rectangles"
								liftIO $ print $ take n x
								(lift $ put st{goodSearches=goodSearches(st)+1})
						mainLoop
					Right (Insert r) -> do
						res <- asks (flip insert r)
						case res of 
							Right x -> do
								liftIO $ putStrLn "insert succesful"
								(lift $ put st{modTree=x, mgoodInserts=mgoodInserts(st)+1,modified=True})
							Left x -> do
								liftIO $ print x
								lift $ put st{mbadInserts=mbadInserts(st)+1}
						mainLoop
					Right (Delete r) ->do
						res <- asks (flip delete r)
						case res of 
							Right x -> do
								liftIO $ putStrLn "delete succesful"
								lift $ put st{modTree=x, mgoodDeletes=mgoodDeletes(st)+1,modified=True}
							Left x -> do
								liftIO $ print x
								lift $ put st{mbadDeletes=mbadDeletes(st)+1}
						mainLoop
					Right Exit -> do 
						st <- lift get 
						liftIO $ putStrLn $ show st 
						liftIO $ putStrLn "Exit"
					Left x	-> do
						liftIO $ print x
						liftIO $ printHelp
						mainLoop

{-
							then case search (modTree(stm)) r of
								Nothing -> do
									liftIO $ putStrLn "Modified database: Nothing found"
									(lift $ put stm{mbadSearches=mbadSearches(stm)+1})
									mainLoop
								Just x -> do
									liftIO $ putStrLn $ "Modified database: found "++show (length x)++" rectangles"
									liftIO $ print $ take n x
									(lift $ put stm{mgoodSearches=mgoodSearches(stm)+1})
									mainLoop
							else mainLoop
-}

main = do
		a<-getArgs
		sTree <- loadFile a
		printHelp
		runStateT (runReaderT mainLoop sTree) newStats


loadFile:: [String] -> IO RTree
loadFile [] = putStrLn "No se especifico la base de datos inicial" >>return Empty
loadFile a = do
		f <- readFile (head a)
		either (e) (ld) (parseRects f) where
			ld rs = do
				putStrLn ("Leidos "++show (length rs) ++" rectangulos desde el archivo "++head a)
				return (fromList' rs)
			e err =do
				putStrLn ("Error leyendo el archivo")
				putStrLn $ show err
				return Empty



--create :: Rectangle -> RTree -> RTree
--create r t = insert t r
{-
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
		let rects = DLO.nubSortOn orderHV (map createRect goodRects)
		putStrLn $ show rects
		--return $ foldl create (Leaf $ DS.empty) rects
-}
