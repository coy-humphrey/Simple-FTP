import System.Directory
import Data.List (sort)

main = do 
	content <- (getCurrentDirectory >>= getDirectoryContents)
	mapM_ putStrLn (sort content)