{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chat.Client where

import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception
import Data.List.Split
import Control.Monad (when,forever,forM_)
import Text.Printf (printf,hPrintf)
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Set as Set

import Chat.Protocol
import Chat.Types
import Control.Concurrent.STM


printToHandle :: Handle -> String -> IO ()
printToHandle handle = hPrintf handle "%s\n"

--sendResponse :: Client -> Message -> IO ()
--sendResponse Client {..} = printToHandle clientHandle

gogoClient :: Server -> Client -> IO ()
gogoClient Server{..} client@Client{..} = do
    -- launch command reader
    commandReader <- forkIO readCommands
    run `finally` killThread commandReader
--      chans <- readTVarIO connectedChannels
--      forM_ (Map.keys chans) $ \name ->
--          handleMessage (Disconnect body)
    where
    readCommands = forever $ do
        command <- hGetLine clientHandle
        command <- fmap parseCommand (hGetLine clientHandle)
        case command of
          Just (HelloText "text") ->
                            hPutStrLn clientHandle ("HELO text\nIP: 0" ++ "\nPort: 0"  ++ "\nStudentID: 12301730\n")
          Just (JoinRequest body) -> atomically $ do
                            let x = parseRequest 4 body
                            let [chatroom_name, client_ip, port, client_name] = x
                            channelMap <- readTVar connectedChannels
                            channel <- newChannel chatroom_name $ Set.singleton clientId
                            modifyTVar' serverChannels $ Map.insert chatroom_name channel
                            --hPutStrLn clientHandle ("Chatroom: " ++ chatroom_name ++ " Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name)
          Just (LeaveRequest body) -> do
                            let x = parseRequest 3 body
                            let [room_ref, join_id, client_name] = x
                            hPutStrLn clientHandle ("Room: " ++ room_ref ++ " Join_id: " ++ join_id ++ " Client Name: " ++ client_name)
          Just (Disconnect body) -> do
                            let x = parseRequest 3 body
                            let [client_ip, port, client_name] = x
                            hPutStrLn clientHandle ("Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name)
          Just (MessageSend body) -> do
                            let x = parseRequest 4 body
                            let [room_ref, join_id, client_name, message] = x
                            hPutStrLn clientHandle ("Room Ref: " ++ room_ref ++ " Join ID: " ++ join_id ++ " Client Name: " ++ client_name ++ " Message: " ++ message)
          Just Terminate ->  hPutStrLn clientHandle "Terminating Server"
          _      -> hPutStrLn clientHandle "Command not recongnised"

    run :: IO ()
    run = forever $ do
      r <- try . atomically $ do
        chans <- readTVar connectedChannels
        foldr (orElse . readTChan) retry
         $ Map.elems chans
      case r of
        Left (e :: SomeException) -> print "derp"
        Right message -> deliverMessage client message

    deliverMessage :: Client -> Message -> IO ()
    deliverMessage Client {..} message =
         case message of
           MessageSend body -> do
            let x = parseRequest 4 body
            let [room_ref, join_id, client_name, message] = x
            printToHandle clientHandle message
           _ -> printToHandle clientHandle "Could not read message"


    deepParse :: String -> String
    deepParse a = splitOn ":" a !! 1

    parseRequest :: Int -> String -> [String]
    parseRequest n body = map deepParse (take n (splitOn "\\n" ("first: " ++ body)))

