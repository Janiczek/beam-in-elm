module Scheduler exposing
    ( Scheduler, Step(..), Program(..), Proc, Pid
    , init, step
    , ex1, ex2, ex7, ex7b
    )

{-|

@docs Scheduler, Step, Program, Proc, Pid
@docs init, step
@docs ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex7b

-}

import Dict exposing (Dict)
import Queue exposing (Queue)
import Set exposing (Set)


type alias Scheduler =
    { procs : Dict Pid Proc
    , nextUnusedPid : Pid
    , readyQueue : Queue Pid
    , revTraces : List (List Step)
    , reductionsBudget : Int
    }


type alias Pid =
    Int


type alias Proc =
    { program : Program
    , mailbox : List String
    , links : Set Pid
    }


type Program
    = End
    | Work Int K
    | Spawn Program KPid
    | Send Pid String K
    | Receive ( String, K )
    | Crash
    | Link Pid K
    | SpawnLink Program KPid


type alias K =
    () -> Program


type alias KPid =
    Pid -> Program


type Step
    = DidWork { worker : Pid, amount : Int }
    | DidSendMessageTo { worker : Pid, recipient : Pid, message : String }
    | DidTryToSendMessageToNonexistentPid { worker : Pid, recipient : Pid, message : String }
    | DidReceiveMsg { worker : Pid, message : String }
    | DidTryToReceiveUnsuccessfully { worker : Pid }
    | DidSpawn { worker : Pid, child : Pid }
    | DidLink { worker : Pid, linked : Pid }
    | DidUnsuccessfullyTryToLink { worker : Pid, linked : Pid }
    | DidEndNormally { worker : Pid }
    | DidCrash { worker : Pid }
    | DidTryToRunNonexistentProcess { process : Pid }
    | DidSpawnLink { worker : Pid, child : Pid }
    | NothingInTheReadyQueue



-- Example program


ex7 : Program
ex7 =
    Spawn ex7Child <|
        \childPid ->
            Link childPid <|
                \() ->
                    Receive
                        ( "CRASH: " ++ String.fromInt childPid
                        , \() -> End
                        )


ex7Child : Program
ex7Child =
    Crash


ex7b : Program
ex7b =
    SpawnLink ex7Child <|
        \childPid ->
            Receive
                ( "CRASH: " ++ String.fromInt childPid
                , \() -> End
                )


ex1 : Program
ex1 =
    End


ex2 : Program
ex2 =
    Work 5 <|
        \() ->
            End


init : { reductionsBudget : Int, program : Program } -> Scheduler
init r =
    { procs = Dict.empty
    , nextUnusedPid = 0
    , readyQueue = Queue.empty
    , revTraces = []
    , reductionsBudget = r.reductionsBudget
    }
        |> spawn r.program
        |> Tuple.first


spawn : Program -> Scheduler -> ( Scheduler, Pid )
spawn program sch =
    let
        pid =
            sch.nextUnusedPid
    in
    ( { sch
        | procs =
            sch.procs
                |> Dict.insert pid (initProc program)
        , nextUnusedPid = pid + 1
      }
        |> enqueue pid
    , pid
    )


initProc : Program -> Proc
initProc program =
    { program = program
    , mailbox = []
    , links = Set.empty
    }


shouldEnqueue : Proc -> Bool
shouldEnqueue proc =
    case proc.program of
        End ->
            False

        Work _ _ ->
            True

        Spawn _ _ ->
            True

        Send _ _ _ ->
            True

        Receive ( branchString, _ ) ->
            List.member branchString proc.mailbox

        Crash ->
            False

        Link _ _ ->
            True

        SpawnLink _ _ ->
            True


enqueue : Pid -> Scheduler -> Scheduler
enqueue pid sch =
    { sch
        | readyQueue =
            if List.member pid (Queue.toList sch.readyQueue) then
                sch.readyQueue

            else
                sch.readyQueue
                    |> Queue.enqueue pid
    }


step : Scheduler -> Scheduler
step sch =
    let
        ( maybePid, restOfQueue ) =
            Queue.dequeue sch.readyQueue
    in
    case maybePid of
        Nothing ->
            sch
                |> log [ NothingInTheReadyQueue ]

        Just pid ->
            let
                newSch =
                    { sch | readyQueue = restOfQueue }
            in
            case Dict.get pid newSch.procs of
                Nothing ->
                    newSch
                        |> log [ DidTryToRunNonexistentProcess { process = pid } ]

                Just proc ->
                    newSch
                        |> stepInner pid proc sch.reductionsBudget


stepInner : Pid -> Proc -> Int -> Scheduler -> Scheduler
stepInner pid proc budget sch =
    if budget <= 0 then
        sch
            |> setProc pid proc
            |> (if shouldEnqueue proc then
                    enqueue pid

                else
                    identity
               )

    else
        let
            stop : Scheduler -> Scheduler
            stop sch_ =
                sch_
                    |> stepInner pid proc 0

            continueWith : Proc -> Program -> Int -> Scheduler -> Scheduler
            continueWith newProc newProgram newBudget sch_ =
                sch_
                    |> stepInner pid (newProc |> setProcProgram newProgram) newBudget
        in
        case proc.program of
            End ->
                sch
                    |> log [ DidEndNormally { worker = pid } ]
                    |> stop

            Work n k ->
                if n <= 0 then
                    sch
                        |> continueWith proc (k ()) budget

                else
                    let
                        workDone =
                            min n budget

                        workRemaining =
                            n - workDone

                        budgetRemaining =
                            budget - workDone
                    in
                    sch
                        |> log [ DidWork { worker = pid, amount = workDone } ]
                        |> continueWith proc (Work workRemaining k) budgetRemaining

            Spawn childProgram kpid ->
                let
                    ( schWithChild, childPid ) =
                        sch |> spawn childProgram

                    newProgram =
                        kpid childPid
                in
                schWithChild
                    |> log [ DidSpawn { worker = pid, child = childPid } ]
                    |> continueWith proc newProgram (budget - 1)

            Send destinationPid message k ->
                sch
                    |> send destinationPid message
                    |> log [ DidSendMessageTo { worker = pid, recipient = destinationPid, message = message } ]
                    |> continueWith proc (k ()) (budget - 1)

            Receive ( branch, k ) ->
                let
                    processMessages : List String -> List String -> Scheduler
                    processMessages unmatchedStartRev restOfMailbox =
                        case restOfMailbox of
                            [] ->
                                sch
                                    |> log [ DidTryToReceiveUnsuccessfully { worker = pid } ]
                                    |> stop

                            message :: rest ->
                                if branch == message then
                                    let
                                        newMailbox =
                                            List.reverse unmatchedStartRev ++ rest
                                    in
                                    sch
                                        |> log [ DidReceiveMsg { worker = pid, message = message } ]
                                        |> continueWith (proc |> setMailbox newMailbox) (k ()) (budget - 1)

                                else
                                    processMessages (message :: unmatchedStartRev) rest
                in
                processMessages [] proc.mailbox

            Crash ->
                sch
                    |> log [ DidCrash { worker = pid } ]
                    |> propagateCrashToLinks pid
                    |> stop

            Link linkedPid k ->
                let
                    ( schWithLink, wasSuccessful ) =
                        sch
                            |> link pid linkedPid

                    newProc =
                        schWithLink.procs
                            |> Dict.get pid
                            |> Maybe.withDefault proc
                in
                schWithLink
                    |> (if wasSuccessful then
                            log [ DidLink { worker = pid, linked = linkedPid } ]

                        else
                            log [ DidUnsuccessfullyTryToLink { worker = pid, linked = linkedPid } ]
                       )
                    |> continueWith newProc (k ()) (budget - 1)

            SpawnLink childProgram kpid ->
                let
                    ( schWithChild, childPid ) =
                        sch |> spawn childProgram

                    ( schWithLink, _ ) =
                        schWithChild
                            |> link pid childPid

                    newProc =
                        schWithLink.procs
                            |> Dict.get pid
                            |> Maybe.withDefault proc

                    newProgram =
                        kpid childPid
                in
                schWithLink
                    |> log [ DidSpawnLink { worker = pid, child = childPid } ]
                    |> continueWith newProc newProgram (budget - 1)


propagateCrashToLinks : Pid -> Scheduler -> Scheduler
propagateCrashToLinks pid sch =
    case Dict.get pid sch.procs of
        Nothing ->
            sch

        Just proc ->
            proc.links
                |> Set.foldl
                    (\linkedPid accSch ->
                        accSch
                            |> send linkedPid ("CRASH: " ++ String.fromInt pid)
                    )
                    sch


isQueueMember : Pid -> Scheduler -> Bool
isQueueMember pid sch =
    List.member pid (Queue.toList sch.readyQueue)


link : Pid -> Pid -> Scheduler -> ( Scheduler, Bool )
link pid1 pid2 sch =
    case ( Dict.get pid1 sch.procs, Dict.get pid2 sch.procs ) of
        ( Just proc1, Just proc2 ) ->
            if
                ((proc1.program == End || proc1.program == Crash) && not (isQueueMember pid1 sch))
                    || ((proc2.program == End || proc2.program == Crash) && not (isQueueMember pid2 sch))
            then
                ( sch, False )

            else
                ( sch
                    |> updateProc pid1 (addLink pid2)
                    |> updateProc pid2 (addLink pid1)
                , True
                )

        _ ->
            ( sch, False )


addLink : Pid -> Proc -> Proc
addLink pid proc =
    { proc | links = proc.links |> Set.insert pid }


send : Pid -> String -> Scheduler -> Scheduler
send destinationPid message sch =
    sch
        |> updateProc destinationPid (enqueueProcMessage message)
        |> enqueue destinationPid


enqueueProcMessage : String -> Proc -> Proc
enqueueProcMessage message proc =
    { proc | mailbox = message :: proc.mailbox }


updateProc : Pid -> (Proc -> Proc) -> Scheduler -> Scheduler
updateProc pid fn sch =
    { sch | procs = sch.procs |> Dict.update pid (Maybe.map fn) }


setProc : Pid -> Proc -> Scheduler -> Scheduler
setProc pid proc sch =
    sch
        |> updateProc pid (\_ -> proc)


setProcProgram : Program -> Proc -> Proc
setProcProgram program proc =
    { proc | program = program }


setMailbox : List String -> Proc -> Proc
setMailbox mailbox proc =
    { proc | mailbox = mailbox }


log : List Step -> Scheduler -> Scheduler
log trace scheduler =
    { scheduler | revTraces = trace :: scheduler.revTraces }
