module Scheduler exposing (Scheduler)

import Dict exposing (Dict)
import PID exposing (PID)
import Proc exposing (Proc)
import Queue exposing (Queue)


reductionsBudget : Int
reductionsBudget =
    7


type alias Scheduler =
    -- TODO: reduction counting
    -- TODO: when a process receives on empty mailbox or one with no matching messages, it will yield and go to the Waiting state
    -- TODO: when a message is delivered to inbox, we'll check if the receiver is SLEEPING in the WAITING state, then wake it, change it to ReadyToRun, and put it at the end of the ready queue
    -- TODO: timeouts
    -- OMIT: priorities, schedule counts - not interesting. https://blog.stenmans.org/theBeamBook/#_the_ready_queue
    { readyQueue : Queue PID
    , procs : Dict PID Proc
    , nextUnusedPid : PID
    }



{-
   1. Update reduction counters.
   2. Check timers
   3. If needed check balance
   4. If needed migrate processes and ports
   5. Do auxiliary scheduler work
   6. If needed check IO and update time
   7. While needed pick a port task to execute
   8. Pick a process to execute
-}


step : Scheduler -> Scheduler
step scheduler =
    case Queue.dequeue scheduler.readyQueue of
        ( Nothing, _ ) ->
            Debug.todo "step Nothing"

        ( Just pid, restOfQueue ) ->
            Debug.todo "step Just(pid)"
