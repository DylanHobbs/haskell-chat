module Chat.Protocol where

import Chat.Types

parseCommand :: String -> Maybe Message
parseCommand command = case words command of
--  "JOIN_CHATROOM: " : chatroom_name : "\n" : "CLIENT_IP" : client_ip : "\n" :  "PORT" : port : "\n" : "CLIENT_NAME: " : client_name : ["\n\n"]   -> Just $ JoinRequest (unwords chatroom_name) (client_ip) (port) (client_name)
  "JOIN_CHATROOM:" : body -> Just $ JoinRequest (unwords body)
  "LEAVE_CHATROOM: " : room_ref : "\n" : "JOIN_ID: " : join_id : "\n" :  "CLIENT_NAME" : client_name : ["\n\n"]                                  -> Just $ LeaveRequest room_ref join_id client_name
  "DISCONNECT: " : client_ip : "\n" : "PORT: " : port : "\n" :  "CLIENT_NAME" : client_name : ["\n\n"]                                           -> Just $ Disconnect client_ip port client_name
  "CHAT: " : room_ref : "\n" : "JOIN_ID: " : join_id : "\n" :  "CLIENT_NAME" : client_name : "\n" : "MESSAGE: " : message : ["\n\n"]             -> Just $ MessageSend room_ref join_id client_name message
  "HELO" : key                                                                                                                                   -> Just $ HelloText (unwords key)
  "KILL_SERVICE\n" : service                                                                                                                     -> Just Terminate
--  "LOGIN" : userName         -> Just $ Login (unwords userName)
  _                                                                                                                                              -> Nothing