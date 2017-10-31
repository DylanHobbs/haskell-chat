module Chat.Types where
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

data User = User {
    userName :: String
}

data Server = Server {
                serverUsers    :: MVar (Map.Map User Client)
              , serverChannels :: TVar (Map.Map String Channel)
              }

data Client = Client {
                clientUser         :: User
              , clientHandle       :: Handle
              , clientChan         :: TChan Message
              , clientChannelChans :: TVar (Map.Map String (TChan String))
              }

data Channel = Channel {
              channelName  :: String
            , channelUsers :: TVar (Set.Set User)
            , channelChan  :: TChan Message
            }



data Message = JoinRequest String
             | JoinResponse
             | LeaveRequest String String String
             | LeaveResponse
             | Disconnect String String String
             | ChatError
             | MessageSend String String String String
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

