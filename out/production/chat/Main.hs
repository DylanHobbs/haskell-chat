module Main where

import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception
import Control.Monad (when)
import Text.Printf (printf)

type Msg = (Int, String)

main :: IO()
main = do
    args <- getArgs
    let port = getPort args
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    chan <- newChan

    _ <- forkIO $ fix $ \loop -> do
      (_,_) <- readChan chan
      loop

    mainLoop sock chan 0

getPort :: [String] -> PortNumber
getPort a = read (head a) :: PortNumber


mainLoop :: Socket -> Chan Msg ->  Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum)
    mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, sockAd) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    handle1 <- socketToHandle sock ReadWriteMode
    hSetBuffering handle1 NoBuffering

    hPutStr handle1 "Enter username: "
    name <- fmap init (hGetLine handle1)
    broadcast ("---> " ++ name ++ " joined the channel")
    hPutStrLn handle1 ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn handle1 line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <-fmap init (hGetLine handle1)
        port <- socketPort sock
        case line of
          "quit" -> hPutStrLn handle1 "Bye!"
          "Hello text" -> hPutStrLn handle1   ("HELO text\nIP: " ++ show sockAd ++ "\nPort: " ++ show port  ++ "\nStudentID: 12301730\n") >> loop
          _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader
    broadcast ("<--- " ++ name ++ "left the channel")
    hClose handle1

















