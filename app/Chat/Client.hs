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
import qualified Data.ByteString.Char8 (hPutStrLn)
import Control.Monad (when,forever,forM_,unless)
import Text.Printf (printf,hPrintf)
import Debug.Trace
import Data.String
import Data.Char (isSpace)
import System.Exit
import Data.Hashable

import qualified Data.Map as Map
import qualified Data.Set as Set

import Chat.Protocol
import Chat.Types
import Control.Concurrent.STM

type ChannelName = String

data KillCommand = KillCommand
    deriving Show

instance Exception KillCommand


gogoClient :: Server -> Client -> Int -> IO ()
gogoClient Server{..} client@Client{..} client_ID = do
    -- launch command reader
    print ("Client: " ++ show client_ID ++ " has the field")
    runner <- forkIO $ run clientId
    readCommands client `finally` do
      print "Running finally"
      killThread runner
      clientChannelMap <- readTVarIO connectedChannels
      forM_ (Map.keys clientChannelMap) $ \channelName ->
        leaveChatroom channelName clientName
      print "Finished Finally!"
    where
      readCommands client@Client{..} = forever $ do
          print ("Client: " ++ show client_ID ++ " is waiting for commands")
          line <- hGetLine clientHandle
          print $ "LINE: " ++ line

          case parseCommand line of
            Just c -> do
              a <- handleMessage c client
              print "Doing a thing"
            Nothing -> do
              print $ "Nothing Pattern: " ++ line
              hPutStrLn clientHandle "ERROR_CODE:0"
              hPutStrLn clientHandle "ERROR_DESCRIPTION: Command not recognised"

      handleMessage (HelloText rest) Client{..} = do
          let t = filter (/= '\r') rest
          let message = "HELO " ++ t ++ "\\nIP:10.62.0.58\\nPort:9999\\nStudentID:12301730\\n"
          hPutStrLn clientHandle message

      handleMessage (JoinRequest crn) x@Client{..} = do
          print clientHandle
          -- parse rest of request line by line
          ip <- hGetLine clientHandle
          p <- hGetLine clientHandle
          cn <- hGetLine clientHandle
          let chatroom_name = filter (/= '\r') crn
          let client_ip = parseFilter ip
          let port = parseFilter p
          let client_name = parseFilter cn

          -- TODO: Maybe move ths below response
          -- Initate join
          joinChatroom chatroom_name clientName

          -- Generate joinID
          let joinID = hash client_name

          -- Return ref
          ref <- getRefFromRoom chatroom_name

          -- Send response
          hPutStrLn clientHandle ("JOINED_CHATROOM:" ++ chatroom_name)
          hPutStrLn clientHandle "SERVER_IP: 10.62.0.58"
          hPutStrLn clientHandle "PORT: 9999"
          hPutStrLn clientHandle ("ROOM_REF:" ++ show ref)
          hPutStrLn clientHandle ("JOIN_ID:" ++  show joinID)

          -- Alert channel and message to server
          print ("JOINING CHANNEL: " ++ client_name ++ " joined room: " ++ chatroom_name)
          let message = client_name ++ " has joined the room"
          sendMessage ref chatroom_name client_name message 0

      handleMessage (LeaveRequest rr) Client{..} = do
          -- Parse rest of input
          ji <- hGetLine clientHandle
          cn <- hGetLine clientHandle
          let join_id = parseFilter ji
          let room_ref = filter (/= '\r') rr
          let room_ref_int = read room_ref :: Int
          let client_name = parseFilter cn

          -- Get room name
          room <- getRoomFromRef (read room_ref :: Int)

          -- Perform leave
          leaveChatroom room client_name

          -- Response
          hPutStrLn clientHandle ("LEFT_CHATROOM:" ++ show room_ref_int)
          hPutStrLn clientHandle ("JOIN_ID:" ++ join_id)

          -- Alert channel and message server
          let message = client_name ++ " has left the room"
          sendMessage room_ref room client_name message 0
          print "finish alert"
          print ("LEAVING CHANNEL: " ++ client_name ++ " left room: " ++ room_ref)

      handleMessage(Disconnect ip) Client{..} = do
          po <- hGetLine clientHandle
          cn <- hGetLine clientHandle
          let client_ip = filter (/= '\r') ip
          let port = parseFilter po
          let client_name = parseFilter cn

          print ("DISCONNECT: " ++ client_name)
          throw KillCommand

      handleMessage (MessageSend rr) Client{..} = do
          -- parse request line by line
          ji <- hGetLine clientHandle
          cn <- hGetLine clientHandle
          m <- hGetLine clientHandle
          let room_ref = filter (/= '\r') rr
          let join_id = parseFilter ji
          let client_name = parseFilter cn
          let message = parse m

          -- Perform message
          let ref_int = read room_ref :: Int
          chanName <- getRoomFromRef ref_int
          sendMessage ref_int chanName client_name message 0
          print ("MESSAGE: " ++ client_name ++ "-> " ++ room_ref)

      run :: Int -> IO ()
      run id = forever $ do
        --print (show id ++ " is running away with it")
        r <- try . atomically $ do
          chans <- readTVar connectedChannels
          foldr (orElse . readTChan) retry
           $ Map.elems chans
        case r of
          Left (e :: KillCommand) -> print $ "KillCommand: " ++ show e
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

      sendMessage room_ref room name message simpleFlag = atomically $ do
          channelMap <- readTVar serverChannels
          case Map.lookup room channelMap of
              Just chan -> writeTChan (channelChan chan) (Text (show room_ref) name message simpleFlag)
              Nothing -> return ()


      deliverMessage :: Client -> Message -> IO ()
      deliverMessage Client {..} message = do
           print "Message delivaer"
           case message of
             Text room_ref name message 0 -> do
                hPutStrLn clientHandle ("CHAT:" ++ room_ref)
                print room_ref
                hPutStrLn clientHandle ("CLIENT_NAME:" ++ name)
                hPutStrLn clientHandle ("MESSAGE:" ++ message)
             Text room_ref name message 1 -> hPutStrLn clientHandle message
             _ -> hPutStrLn clientHandle "Could not read message"

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
                      channelNums <- readTVar maxChannels
                      room2name <- readTVar roomToName
                      modifyTVar' roomToName $ Map.insert channelNums room
                      modifyTVar' maxChannels (+ 1)
                      channel <- newChannel room channelNums $ Set.singleton clientId
                      modifyTVar' serverChannels $ Map.insert room channel
                      return channel
              client_chans <- dupTChan channelChan
              modifyTVar' connectedChannels $ Map.insert room client_chans

      getRoomFromRef ref = atomically $ do
          room2name <- readTVar roomToName
          case Map.lookup ref room2name of
              Just name -> return name

      getRefFromRoom room = atomically $ do
          channelMap <- readTVar serverChannels
          case Map.lookup room channelMap of
              Just (channel@Channel{..}) -> return channelRef
              Nothing -> return 50

      parseFilter :: String -> String
      parseFilter a = filter (/= ' ') $ filter (/= '\r') $ splitOn ":" a !! 1

      parseFilter' :: String -> String
      parseFilter' a = filter (/= '\r') $ splitOn ":" a !! 1

      parse :: String -> String
      parse a = splitOn ":" a !! 1




