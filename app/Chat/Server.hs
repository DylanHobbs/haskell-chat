{-# LANGUAGE RecordWildCards #-}

module Chat.Server where

import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception
import Data.List.Split
import Control.Monad (when)
import Text.Printf (printf)
import Debug.Trace
import qualified Data.Map as Map

import Chat.Protocol
import Chat.Types


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
    listen sock 2
    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
      (_,_) <- readChan chan
      loop

    mainLoop server sock chan 0 0

getPort :: [String] -> PortNumber
getPort a = read (head a) :: PortNumber

deepParse :: String -> String
deepParse a = splitOn ":" a !! 1

parseRequest :: Int -> String -> [String]
parseRequest n body = map deepParse (take n (splitOn "\\n" ("first: " ++ body)))

mainLoop :: Server -> Socket -> Chan Msg -> Int -> Int -> IO ()
mainLoop server sock chan msgNum clientNum = do
    conn <- accept sock
    --handle <- socketToHandle sock ReadWriteMode
    --client <- newClient clientNum handle
    -- TODO: Check user already entered, using modiifyMVar and Maybe types
    --addUser server clientNum client
    forkIO (runConn conn server chan msgNum clientNum)
    mainLoop server sock chan (msgNum + 1) $! clientNum + 1

addUser :: Server -> Int -> Client -> IO ()
addUser Server {..} clientID client =
  modifyMVar_ serverUsers $ \cur -> do
        let a = sMap cur
        return (Map.insert clientID client (sMap cur))

printCurrentUsers Server{..} = do
    v <- readMVar serverUsers
    x <- Map.elemAt 0 v
    print v


runConn :: (Socket, SockAddr) -> Server -> Chan Msg -> Int -> Int -> IO ()
runConn (sock, sockAd) server chan msgNum clientID = do
    let broadcast msg = writeChan chan (msgNum, msg)
    handle1 <- socketToHandle sock ReadWriteMode
    client <- newClient clientID handle1
    addUser server clientID client

    hSetBuffering handle1 NoBuffering

--    hPutStr handle1 "Enter username: "
--    name <- fmap init (hGetLine handle1)
--    broadcast ("---> " ++ name ++ " joined the global channel")
--    hPutStrLn handle1 ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn handle1 line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        port <- socketPort sock
        command <- fmap parseCommand (hGetLine handle1)
        case command of
          Just (HelloText "text") -> do
                            hPutStrLn handle1 ("HELO text\nIP: " ++ show sockAd ++ "\nPort: " ++ show port  ++ "\nStudentID: 12301730\n")
                            loop
          Just (JoinRequest body) -> do
                            let x = parseRequest 4 body
                            let [chatroom_name, client_ip, port, client_name] = x
                            hPutStrLn handle1 ("Chatroom: " ++ chatroom_name ++ " Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name) >> loop
          Just (LeaveRequest body) -> do
                            let x = parseRequest 3 body
                            let [room_ref, join_id, client_name] = x
                            hPutStrLn handle1 ("Room: " ++ room_ref ++ " Join_id: " ++ join_id ++ " Client Name: " ++ client_name) >> loop
          Just (Disconnect body) -> do
                            let x = parseRequest 3 body
                            let [client_ip, port, client_name] = x
                            hPutStrLn handle1 ("Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name) >> loop
          Just (MessageSend body) -> do
                            let x = parseRequest 4 body
                            let [room_ref, join_id, client_name, message] = x
                            hPutStrLn handle1 ("Room Ref: " ++ room_ref ++ " Join ID: " ++ join_id ++ " Client Name: " ++ client_name ++ " Message: " ++ message) >> loop
          Just Terminate ->  hPutStrLn handle1 "Terminating Server"
          _      -> hPutStrLn handle1 "Command not recongnised" >> loop

    killThread reader
    broadcast ("<--- user left the channel")
    hClose handle1

















