module Trace exposing (Step(..), Trace)

import PID exposing (PID)
import Program exposing (Message)


type alias Trace =
    List Step


type Step
    = DidWork { worker : PID, label : String, amount : Int }
    | DidGetSelfPid { worker : PID }
    | DidSendMessageTo { worker : PID, recipient : PID, message : Message }
    | DidTryToSendMessageToNonexistentPid { worker : PID, recipient : PID, message : Message }
    | DidReceiveMsg { worker : PID, message : Message }
    | DidTryToReceiveUnsuccessfully { worker : PID }
    | DidSpawn { worker : PID, child : PID }
    | DidEnd { worker : PID }
    | DidTryToRunNonexistentProcess { process : PID }
    | NothingInTheReadyQueue
