{-# LANGUAGE RecordWildCards #-}

module Chat.Server where

import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception
import Data.List.Split
import Control.Monad (when,forever)
import Text.Printf (printf)
import Debug.Trace

import qualified Data.Map as Map

import Chat.Protocol
import Chat.Types
import Chat.Client


type Msg = (Int, String)

gogoServer :: IO()
gogoServer = do
    args <- getArgs
    let port = getPort args
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    print ("Service Started on port: " ++ show port)
    server <- newServer
    listen sock 10
    mainLoop server sock 0

getPort :: [String] -> PortNumber
getPort a = read (head a) :: PortNumber

mainLoop :: Server -> Socket -> Int  -> IO ()
mainLoop server sock clientID = do
    (acceptedSocket, sockAd) <- accept sock
    handle <- socketToHandle acceptedSocket ReadWriteMode
    hSetBuffering handle NoBuffering
    forkIO $ addUser server handle clientID `finally` hClose handle
    mainLoop server sock (clientID + 1)

addUser :: Server -> Handle -> Int -> IO()
addUser server@Server{..} handle clientID =
    modifyMVar_ serverUsers $ \cur -> do
              client <- newClient clientID handle
              gogoClient server client `finally` removeUser server clientID
              return (Map.insert clientID client cur)

removeUser :: Server -> Int -> IO ()
removeUser Server{..} userID =
  modifyMVar_ serverUsers $ return . Map.delete userID

--runConn :: (Socket, SockAddr) -> Server -> Chan Msg -> Int -> Int -> IO ()
--runConn (sock, sockAd) server chan msgNum clientID = do
--    let broadcast msg = writeChan chan (msgNum, msg)
--    handle1 <- socketToHandle sock ReadWriteMode
--
--    hSetBuffering handle1 NoBuffering
--
----    hPutStr handle1 "Enter username: "
----    name <- fmap init (hGetLine handle1)
----    broadcast ("---> " ++ name ++ " joined the global channel")
----    hPutStrLn handle1 ("Welcome, " ++ name ++ "!")
--
--    commLine <- dupChan chan
--
--    reader <- forkIO $ fix $ \loop -> do
--        (nextNum, line) <- readChan commLine
--        when (msgNum /= nextNum) $ hPutStrLn handle1 line
--        loop
--
--    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
--        port <- socketPort sock
--        command <- fmap parseCommand (hGetLine handle1)
--        case command of
--          Just (HelloText "text") -> do
--                            hPutStrLn handle1 ("HELO text\nIP: " ++ show sockAd ++ "\nPort: " ++ show port  ++ "\nStudentID: 12301730\n")
--                            loop
--          Just (JoinRequest body) -> do
--                            let x = parseRequest 4 body
--                            let [chatroom_name, client_ip, port, client_name] = x
--                            hPutStrLn handle1 ("Chatroom: " ++ chatroom_name ++ " Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name) >> loop
--          Just (LeaveRequest body) -> do
--                            let x = parseRequest 3 body
--                            let [room_ref, join_id, client_name] = x
--                            hPutStrLn handle1 ("Room: " ++ room_ref ++ " Join_id: " ++ join_id ++ " Client Name: " ++ client_name) >> loop
--          Just (Disconnect body) -> do
--                            let x = parseRequest 3 body
--                            let [client_ip, port, client_name] = x
--                            hPutStrLn handle1 ("Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name) >> loop
--          Just (MessageSend body) -> do
--                            let x = parseRequest 4 body
--                            let [room_ref, join_id, client_name, message] = x
--                            hPutStrLn handle1 ("Room Ref: " ++ room_ref ++ " Join ID: " ++ join_id ++ " Client Name: " ++ client_name ++ " Message: " ++ message) >> loop
--          Just Terminate ->  hPutStrLn handle1 "Terminating Server"
--          _      -> hPutStrLn handle1 "Command not recongnised" >> loop
--
--    killThread reader
--    broadcast ("<--- user left the channel")
--    hClose handle1

















