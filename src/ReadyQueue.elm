module ReadyQueue exposing (ReadyQueue, dequeue, empty, enqueue, toList)

import PID exposing (PID)
import Queue exposing (Queue)
import Set exposing (Set)


type alias ReadyQueue =
    { queue : Queue PID
    , set : Set PID
    }


empty : ReadyQueue
empty =
    { queue = Queue.empty
    , set = Set.empty
    }


enqueue : PID -> ReadyQueue -> ReadyQueue
enqueue pid readyQueue =
    if Set.member pid readyQueue.set then
        readyQueue

    else
        { readyQueue
            | queue = Queue.enqueue pid readyQueue.queue
            , set = Set.insert pid readyQueue.set
        }


dequeue : ReadyQueue -> Maybe ( PID, ReadyQueue )
dequeue readyQueue =
    case Queue.dequeue readyQueue.queue of
        ( Nothing, _ ) ->
            Nothing

        ( Just pid, restOfQueue ) ->
            Just
                ( pid
                , { readyQueue
                    | queue = restOfQueue
                    , set = Set.remove pid readyQueue.set
                  }
                )


toList : ReadyQueue -> List PID
toList readyQueue =
    Queue.toList readyQueue.queue
