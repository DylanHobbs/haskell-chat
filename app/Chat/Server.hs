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
    hSetBuffering handle LineBuffering
    print ("Client: " ++ show clientID ++ " connected to the server")
    forkIO $ addUser server handle clientID `finally` do
                                                        print "Closing client handle"
                                                        hClose handle
    mainLoop server sock (clientID + 1)

addUser :: Server -> Handle -> Int -> IO()
addUser server@Server{..} handle clientID = do
              client <- newClient clientID handle
              print ("Client: " ++ show clientID ++ " added to server list")
              gogoClient server client clientID `finally` do
                                                            print "Made it back to server"
                                                            removeUser server clientID

removeUser :: Server -> Int -> IO ()
removeUser Server{..} userID = do
  print "Removing user"
  modifyMVar_ serverUsers $ return . Map.delete userID


















