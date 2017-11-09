module Chat.Types where
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

type UserName = String
type ChannelName = String
type RoomRef = Int

data User = User { userID :: Int }
            deriving (Show, Ord, Eq)

data Server = Server {
                serverUsers    :: MVar (Map.Map Int Client)
              , serverChannels :: TVar (Map.Map ChannelName Channel)
              , roomToName     :: TVar (Map.Map RoomRef ChannelName)
              , maxChannels    :: TVar Int
              }
newServer :: IO Server
newServer = do
  serverUsers    <- newMVar Map.empty
  serverChannels <- newTVarIO Map.empty
  roomToName <- newTVarIO Map.empty
  maxChannels    <- newTVarIO 0
  return $ Server serverUsers serverChannels roomToName maxChannels

data Client = Client {
                clientId           :: Int
              , clientName         :: String
              , clientHandle       :: Handle
              , connectedChannels :: TVar (Map.Map ChannelName (TChan Message))
              }
newClient :: Int -> Handle -> IO Client
newClient clientID handle = do
              connectedChannels <- newTVarIO Map.empty
              let name = "[No Name Yet]"
              return $ Client clientID name handle connectedChannels

data Channel = Channel {
              channelName  :: String
            , channelUsers :: TVar (Set.Set Int)
            , channelChan  :: TChan Message
            }
newChannel :: ChannelName -> Set.Set Int -> STM Channel
newChannel channelName users = do
  channelUsers <- newTVar users
  channelChan  <- newBroadcastTChan
  return $ Channel channelName channelUsers channelChan



data Message = JoinRequest String
             | Text String
             | JoinResponse
             | LeaveRequest String
             | LeaveResponse
             | Disconnect String
             | ChatError
             | MessageSend String
             | MessageReceive
             | HelloText String
             | Terminate
             | Login String
             | Quit
              deriving (Show, Eq)
--
--data JoinRequest = JoinRequest {
--                  join_resquest_name :: String,
--                  client_IP :: String,
--                  port_join_request :: PortNumber,
--                  c_name :: String
--                }
--
--data JoinResponse = JoinResponse {
--                  join_response_name :: String,
--                  server_IP :: String,
--                  port_join_response :: PortNumber,
--                  room_ref :: Channel,
--                  join_id :: Int
--                }
--
--data LeaveRequest = LeaveRequest {
--                  chatroom_name :: String,
--                  join_id :: Int,
--                  c_name :: String
--                }
--
--data LeaveResponse = LeaveResponse {
--                  chatroom_name :: String,
--                  join_id :: Int
--                }
--
--data Terminate = Terminate {
--                  disconnect_IP :: String,
--                  port :: PortNumber,
--                  c_name :: String
--                }
--
--data ChatError = ChatError {
--                  error_code :: Int,
--                  error_desc :: String
--                }
--
--data MessageSend = MessageSend {
--                  error_code :: Int,
--                  error_desc :: String
--                }


--data JoinResponse = JoinResponse {
--                chatRoomName :: String
--              , serverIP :: AF_INET
--              , port :: PortNumber
--              , roomRef :: Integer
--              , joinID :: Integer
--              }
--
--data JoinRequest = JoinRequest {
--                chatRoomName :: String
--              , serverIP :: AF_INET
--              , port :: PortNumber
--              , clinetName :: String
--              }

