module Chat.Protocol where

import Chat.Types

parseCommand :: String -> Maybe Message
parseCommand command =
  case words command of
    "JOIN_CHATROOM:" : body      -> Just $ JoinRequest (unwords body)
    "LEAVE_CHATROOM:" : body     -> Just $ LeaveRequest (unwords body)
    "DISCONNECT:" : body         -> Just $ Disconnect (unwords body)
    "CHAT:" : body               -> Just $ MessageSend (unwords body)
    "HELO" : key                 -> Just $ HelloText (unwords key)
    "KILL_SERVICE\n" : service   -> Just Terminate
    _                            -> Nothing