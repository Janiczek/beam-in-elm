module Trace exposing (Step(..), Trace)

import PID exposing (PID)
import Program exposing (CrashReason, Message)


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
    | DidEndNormally { worker : PID }
    | DidCrash { worker : PID, reason : CrashReason }
    | DidTryToRunNonexistentProcess { process : PID }
    | NothingInTheReadyQueue
