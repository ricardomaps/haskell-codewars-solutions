module TCP (traverseStates) where
import Control.Monad (foldM)

-- import Preloaded (TCPEvent(..), TCPState(..))

data TCPEvent
  = APP_PASSIVE_OPEN
  | APP_ACTIVE_OPEN
  | APP_SEND
  | APP_CLOSE
  | APP_TIMEOUT
  | RCV_SYN
  | RCV_ACK
  | RCV_SYN_ACK
  | RCV_FIN
  | RCV_FIN_ACK
  deriving (Show, Eq, Ord, Enum)
  
data TCPState
  = CLOSED
  | LISTEN
  | SYN_SENT
  | SYN_RCVD
  | ESTABLISHED
  | CLOSE_WAIT
  | LAST_ACK
  | FIN_WAIT_1
  | FIN_WAIT_2
  | CLOSING
  | TIME_WAIT
  deriving (Show, Eq, Ord, Enum)


-- CLOSED: APP_PASSIVE_OPEN -> LISTEN
-- CLOSED: APP_ACTIVE_OPEN  -> SYN_SENT
-- LISTEN: RCV_SYN          -> SYN_RCVD
-- LISTEN: APP_SEND         -> SYN_SENT
-- LISTEN: APP_CLOSE        -> CLOSED
-- SYN_RCVD: APP_CLOSE      -> FIN_WAIT_1
-- SYN_RCVD: RCV_ACK        -> ESTABLISHED
-- SYN_SENT: RCV_SYN        -> SYN_RCVD
-- SYN_SENT: RCV_SYN_ACK    -> ESTABLISHED
-- SYN_SENT: APP_CLOSE      -> CLOSED
-- ESTABLISHED: APP_CLOSE   -> FIN_WAIT_1
-- ESTABLISHED: RCV_FIN     -> CLOSE_WAIT
-- FIN_WAIT_1: RCV_FIN      -> CLOSING
-- FIN_WAIT_1: RCV_FIN_ACK  -> TIME_WAIT
-- FIN_WAIT_1: RCV_ACK      -> FIN_WAIT_2
-- CLOSING: RCV_ACK         -> TIME_WAIT
-- FIN_WAIT_2: RCV_FIN      -> TIME_WAIT
-- TIME_WAIT: APP_TIMEOUT   -> CLOSED
-- CLOSE_WAIT: APP_CLOSE    -> LAST_ACK
-- LAST_ACK: RCV_ACK        -> CLOSED

traverseStates :: [TCPEvent] -> Maybe TCPState
traverseStates = foldM handleEvent CLOSED
      
  where
  handleEvent state event =
    case (state, event) of
      (CLOSED, APP_PASSIVE_OPEN) -> Just LISTEN
      (CLOSED, APP_ACTIVE_OPEN)  -> Just SYN_SENT
      (LISTEN, RCV_SYN)          -> Just SYN_RCVD
      (LISTEN, APP_SEND)         -> Just SYN_SENT
      (LISTEN, APP_CLOSE)        -> Just CLOSED
      (SYN_RCVD, APP_CLOSE)      -> Just FIN_WAIT_1
      (SYN_RCVD, RCV_ACK)        -> Just ESTABLISHED
      (SYN_SENT, RCV_SYN)        -> Just SYN_RCVD
      (SYN_SENT, RCV_SYN_ACK)    -> Just ESTABLISHED
      (SYN_SENT, APP_CLOSE)      -> Just CLOSED
      (ESTABLISHED, APP_CLOSE)   -> Just FIN_WAIT_1
      (ESTABLISHED, RCV_FIN)     -> Just CLOSE_WAIT
      (FIN_WAIT_1, RCV_FIN)      -> Just CLOSING
      (FIN_WAIT_1, RCV_FIN_ACK)  -> Just TIME_WAIT
      (FIN_WAIT_1, RCV_ACK)      -> Just FIN_WAIT_2
      (CLOSING, RCV_ACK)         -> Just TIME_WAIT
      (FIN_WAIT_2, RCV_FIN)      -> Just TIME_WAIT
      (TIME_WAIT, APP_TIMEOUT)   -> Just CLOSED
      (CLOSE_WAIT, APP_CLOSE)    -> Just LAST_ACK
      (LAST_ACK, RCV_ACK)        -> Just CLOSED
      _                          -> Nothing
