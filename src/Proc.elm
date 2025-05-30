module Proc exposing (Proc, State(..))

import PID exposing (PID)


type alias Proc =
    { mailbox : List String
    , state : State
    }


type State
    = ReadyToRun
    | Running
    | WaitingForMsg
