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
          --command <- hGetLine clientHandle
          line <- hGetLine clientHandle
          let [command, first] = splitOn " " line

          case [command, first] of
            ["HELO","BASE_TEST"] -> hPutStr clientHandle ("HELO text\nIP: 0" ++ "\nPort: 0"  ++ "\nStudentID: 12301730\n")
            ["JOIN_CHATROOM:", chatroom_name]-> do
                              client_ip <- hGetLine clientHandle
                              port <- hGetLine clientHandle
                              client_name <- hGetLine clientHandle
                              joinChatroom chatroom_name clientName
                              hPutStr clientHandle ("JOINED_CHATROOM:" ++ chatroom_name)
                              hPutStr clientHandle "SERVER_IP:0"
                              hPutStr clientHandle "PORT:0"
                              hPutStr clientHandle ("ROOM_REF:" ++ chatroom_name)
                              hPutStr clientHandle "JOIN_ID:0"
                              print ("JOINING CHANNEL: " ++ client_name ++ "joined room: " ++ chatroom_name)
                              --hPutStrLn clientHandle ("Chatroom: " ++ chatroom_name ++ " Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name)
            ["LEAVE_CHATROOM:", room_ref] -> do
                              join_id <- hGetLine clientHandle
                              client_name <- hGetLine clientHandle
                              leaveChatroom room_ref client_name
                              hPutStr clientHandle ("LEFT_CHATROOM:" ++ room_ref)
                              hPutStr clientHandle ("JOIN_ID:" ++ room_ref)
                              print ("LEAVING CHANNEL: " ++ client_name ++ " left room: " ++ room_ref)
                              --hPutStrLn clientHandle ("Room: " ++ room_ref ++ " Join_id: " ++ join_id ++ " Client Name: " ++ client_name)
            ["DISCONNECT:", client_ip] -> do
                              port <- hGetLine clientHandle
                              client_name <- hGetLine clientHandle
                              print ("DISCONNECT: " ++ client_name)
                              --disconnect client_ip client_name
                              --hPutStrLn clientHandle ("Client IP: " ++ client_ip ++ " Port: " ++ port ++ " Client Name: " ++ client_name)
            ["CHAT:", room_ref] -> do
                              join_id <- hGetLine clientHandle
                              client_name <- hGetLine clientHandle
                              message <- hGetLine clientHandle
                              print ("MESSAGE: " ++ client_name ++ "-> " ++ room_ref)
                              sendMessage room_ref client_name message
                              --hPutStrLn clientHandle ("Room Ref: " ++ room_ref ++ " Join ID: " ++ join_id ++ " Client Name: " ++ client_name ++ " Message: " ++ message)
            ["KILL_SERVICE", _] ->  hPutStrLn clientHandle "Terminating Server"
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


