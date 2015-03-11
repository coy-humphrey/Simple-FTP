import Network
import Control.Concurrent
import System.IO
import System.Environment (getArgs)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as B 
import System.Directory
import Data.List (sort)

-- See: http://www.catonmat.net/blog/simple-haskell-tcp-server/

main = withSocketsDo $ do 
    args <- getArgs
    unless (length args /= 1) $ do
        let port = fromIntegral (read $ head args :: Int)
        sock <- listenOn $ PortNumber port
        loop sock

loop :: Socket -> IO ()
loop sock = do 
    (h,host,_) <- accept sock
    forkIO $ handler h host ""
    loop sock


handler :: Handle -> HostName -> String -> IO ()
handler h host port = do
    line <- hGetLine h :: IO String
    let cmd = if (length.words) line > 0
        then words line
        else [""]
    case (head cmd) of 
        "exit"   -> hClose h
        "port"   -> if (length cmd == 2) 
                        then
                            handler h host (cmd !! 1)
                        else do
                            hPutStrLn h "Invalid arguments to port"
                            handler h host port
        "get"    -> if length cmd == 2 && port /= []
                        then do 
                            forkIO $ doGet host port (cmd !! 1)
                            handler h host ""
                        else do
                            hPutStrLn h "get command failed"
                            handler h host port
        "put"    -> if length cmd == 2
                        then do 
                            forkIO $ doPut h (cmd !! 1)
                            handler h host port
                        else do
                            hPutStrLn h "put command failed"
                            handler h host port
        "dir"    -> if length cmd == 1
                        then do
                            doDir h
                            handler h host port
                        else do
                            hPutStrLn h "dir command failed"
                            handler h host port
        ""       -> handler h host port
        _        -> do
                        hPutStrLn h "Unrecognized command"
                        handler h host port

doGet :: HostName -> String -> String -> IO ()
doGet host port file = do
    let portNum = fromIntegral (read port :: Int)
    sock <- connectTo host (PortNumber portNum)
    withFile file ReadMode (\handle -> do
        contents <- B.hGetContents handle
        B.hPut sock contents)
    hClose sock

doPut :: Handle -> String -> IO ()
doPut h file = do
    sock <- listenOn $ PortNumber 0
    (PortNumber port) <- socketPort sock
    hPutStrLn h $ show port
    (client,_,_) <- accept sock
    withFile file WriteMode (\handle -> do
        contents <- B.hGetContents client
        B.hPut handle contents)
    hClose client
    sClose sock

doDir :: Handle -> IO ()
doDir h = do
    content <- (getCurrentDirectory >>= getDirectoryContents)
    mapM_ (hPutStrLn h) (sort content)
