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

type Message = (Int, String)


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

