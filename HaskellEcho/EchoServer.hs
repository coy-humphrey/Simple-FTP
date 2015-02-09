import Network
import Control.Concurrent
import System.IO
import System.Environment (getArgs)
import Control.Monad (unless)

-- See: http://www.catonmat.net/blog/simple-haskell-tcp-server/

main = withSocketsDo $ do 
	args <- getArgs
	unless (length args /= 1) $ do
	    let port = fromIntegral (read $ head args :: Int)
	    sock <- listenOn $ PortNumber port
	    loop sock

loop :: Socket -> IO ()
loop sock = do 
	(h,_,_) <- accept sock
	forkIO $ handler h
	loop sock

handler :: Handle -> IO ()
handler h = do
	line <- hGetLine h :: IO String
	let cmd = words line
	if (length cmd == 1) && (head cmd == "exit")
		then hClose h 
	else do
	    hPutStrLn h line
	    handler h