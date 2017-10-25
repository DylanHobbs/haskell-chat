module Main where

import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent


main :: IO()
main = do
    args <- getArgs
    let port = getPort args
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    mainLoop sock

getPort :: [String] -> PortNumber
getPort a = read (head a) :: PortNumber


mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    runConn conn
    mainLoop sock

--runConn :: (Socket, SockAddr) -> IO ()
                --runConn (sock, _) = do
                --    send sock "Hello World!\n"
                --    close sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    handle1 <- socketToHandle sock ReadWriteMode
    hSetBuffering handle1 NoBuffering
    hPutStr handle1 "Hello!"
    hClose handle1




