module Main(main) where

import System( getArgs )
import System.IO

import Galaxy.Parser
import Galaxy.Show

import System.Exit


main = do
	fileNames<- getArgs
	files 	 <-	(sequence . map read) fileNames	
	toString $ map (uncurry doParse) $ zip fileNames files
		where
			read "-s" 	= getContents 
			read arg 	= readFile arg
			toString programs = case partitionEithers programs of
				([], o) -> do
				  mapM (putStr . renderDoc) o
				(err, _)-> do
					hPutStr stderr $ "parse errors: " ++ show err ++ "\n"
					exitWith $ ExitFailure 1
				
-- I thought this was supposed to be in Data.Either. It isn't for me :(
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([],[])
 where
  left  a (l, r) = (a:l, r)
  right a (l, r) = (l, a:r)

