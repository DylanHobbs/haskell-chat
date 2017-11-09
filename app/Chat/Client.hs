{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chat.Client where

import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception
import Data.List.Split
import Control.Monad (when,forever,forM_,unless)
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

gogoClient :: Server -> Client -> Int -> IO ()
gogoClient Server{..} client@Client{..} client_ID = do
    -- launch command reader
    print ("Client: " ++ show client_ID ++ " has the field")
    commandReader <- forkIO readCommands
    run clientId `finally` killThread commandReader
--      chans <- readTVarIO connectedChannels
--      forM_ (Map.keys chans) $ \name ->
--          handleMessage (Disconnect body)
    where
      readCommands = forever $ do
          print ("Client: " ++ show client_ID ++ " is waiting for commands")
          command <- fmap parseCommand (hGetLine clientHandle)
          print command
          case command of
            Just (HelloText "BASE_TEST") ->
                              hPutStrLn clientHandle ("HELO text\nIP: 0" ++ "\nPort: 0"  ++ "\nStudentID: 12301730\n")
            Just (JoinRequest body) -> do
                              let x = parseRequest 4 body
                              let [chatroom_name, client_ip, port, client_name] = x
                              joinChatroom chatroom_name clientName
                              print ("JOINING CHANNEL: " ++ client_name ++ "joined room: " ++ chatroom_name)
                              --hPutStrLn clientHandle ("Chatroom: " ++ chatroom_name ++ " Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name)
            Just (LeaveRequest body) -> do
                              let x = parseRequest 3 body
                              let [room_ref, join_id, client_name] = x
                              leaveChatroom room_ref client_name
                              print ("LEAVING CHANNEL: " ++ client_name ++ " left room: " ++ room_ref)
                              --hPutStrLn clientHandle ("Room: " ++ room_ref ++ " Join_id: " ++ join_id ++ " Client Name: " ++ client_name)
            Just (Disconnect body) -> do
                              let x = parseRequest 3 body
                              let [client_ip, port, client_name] = x
                              print ("DISCONNECT: " ++ client_name)
                              --disconnect client_ip client_name
                              --hPutStrLn clientHandle ("Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name)
            Just (MessageSend body) -> do
                              let x = parseRequest 4 body
                              let [room_ref, join_id, client_name, message] = x
                              print ("MESSAGE: " ++ client_name ++ "-> " ++ room_ref)
                              sendMessage room_ref client_name message
                              --hPutStrLn clientHandle ("Room Ref: " ++ room_ref ++ " Join ID: " ++ join_id ++ " Client Name: " ++ client_name ++ " Message: " ++ message)
            Just Terminate ->  hPutStrLn clientHandle "Terminating Server"
            _      -> hPutStrLn clientHandle "Command not recongnised"

      run :: Int -> IO ()
      run id = forever $ do
        --print (show id ++ " is running away with it")
        r <- try . atomically $ do
          chans <- readTVar connectedChannels
          foldr (orElse . readTChan) retry
           $ Map.elems chans
        case r of
          Left (e :: SomeException) -> print "derp"
          Right message -> deliverMessage client message

      leaveChatroom room client = atomically $ do
          serverChannelMap <- readTVar serverChannels
          clientChannelMap <- readTVar connectedChannels
          case Map.lookup room serverChannelMap of
              Just (channel@Channel{..}) -> do
                userSet <- readTVar channelUsers
                modifyTVar' channelUsers $ Set.delete clientId
                return channel
          modifyTVar' connectedChannels $ Map.delete room

      sendMessage room name message = atomically $ do
          channelMap <- readTVar serverChannels
          case Map.lookup room channelMap of
              Just chan -> writeTChan (channelChan chan) (Text message)
              Nothing -> return ()


      deliverMessage :: Client -> Message -> IO ()
      deliverMessage Client {..} message = do
           print "Message delivaer"
           case message of
             Text body -> printToHandle clientHandle body
             _ -> printToHandle clientHandle "Could not read message"

      joinChatroom room name = atomically $ do
           clientChannelMap <- readTVar connectedChannels
           unless (Map.member room clientChannelMap) $ do
              channelMap <- readTVar serverChannels
              channel@Channel{channelChan} <-
                case Map.lookup room channelMap of
                    Just (channel@Channel{channelUsers}) -> do
                      modifyTVar' channelUsers $ Set.insert clientId
                      return channel
                    Nothing                             -> do
                      channel <- newChannel room $ Set.singleton clientId
                      modifyTVar' serverChannels $ Map.insert room channel
                      return channel
              client_chans <- dupTChan channelChan
              modifyTVar' connectedChannels $ Map.insert room client_chans

--      leaveChatroom room name = do
--           clientChannelMap <- readTVar connectedChannels
--           print room
--           --modifyTVar' connectedChannels $ Map.remove room clientChannelMap

      deepParse :: String -> String
      deepParse a = splitOn ":" a !! 1

      parseRequest :: Int -> String -> [String]
      parseRequest n body = map deepParse (take n (splitOn "\\n" ("first: " ++ body)))


