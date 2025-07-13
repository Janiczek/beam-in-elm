module Scheduler exposing (Scheduler, init, reductionsBudget, step)

import Dict exposing (Dict)
import PID exposing (PID)
import Proc exposing (Proc, State(..))
import Program exposing (Message, Program(..))
import ReadyQueue exposing (ReadyQueue)
import Trace exposing (Step(..))


type alias Scheduler =
    -- OMIT: timeouts
    -- OMIT: priorities, schedule counts - not interesting. https://blog.stenmans.org/theBeamBook/#_the_ready_queue
    { readyQueue : ReadyQueue
    , procs : Dict PID Proc
    , nextUnusedPid : PID
    , revTraces : List (List Step)
    , reductionsBudget : Int
    }


init : { reductionsBudget : Int, program : Program } -> Scheduler
init r =
    { readyQueue = ReadyQueue.empty
    , procs = Dict.empty
    , nextUnusedPid = 0
    , revTraces = []
    , reductionsBudget = r.reductionsBudget
    }
        |> spawn r.program
        |> Tuple.first



{-
   From the book:
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
step sch =
    case ReadyQueue.dequeue sch.readyQueue of
        Nothing ->
            -- No more work (finished? deadlock?)
            sch
                |> log [ NothingInTheReadyQueue ]

        Just ( pid, restOfQueue ) ->
            case Dict.get pid sch.procs of
                Nothing ->
                    sch
                        |> setReadyQueue restOfQueue
                        |> log [ DidTryToRunNonexistentProcess { process = pid } ]

                Just proc ->
                    let
                        ( sch2, program2, revTrace ) =
                            stepProgram (sch |> setReadyQueue restOfQueue) sch.reductionsBudget pid proc

                        proc2 =
                            Dict.get pid sch2.procs
                                |> Maybe.withDefault proc

                        shouldReenqueue =
                            case proc2.state of
                                ReadyToRun ->
                                    True

                                EndedNormally ->
                                    False

                                Crashed _ ->
                                    False

                                WaitingForMsg ->
                                    False
                    in
                    sch2
                        |> updateProc pid (Proc.setProgram program2)
                        |> log (List.reverse revTrace)
                        |> (if shouldReenqueue then
                                enqueue pid

                            else
                                identity
                           )


stepProgram : Scheduler -> Int -> PID -> Proc -> ( Scheduler, Program, List Step )
stepProgram sch budget pid proc =
    stepProgram_ sch budget pid proc.mailbox [] proc.program


stepProgram_ : Scheduler -> Int -> PID -> List Message -> List Step -> Program -> ( Scheduler, Program, List Step )
stepProgram_ sch budget pid mailbox revTrace program =
    if budget <= 0 then
        ( sch, program, revTrace )

    else
        let
            recur : Int -> Scheduler -> Step -> Program -> ( Scheduler, Program, List Step )
            recur workDone sch2 newStep newProgram =
                stepProgram_ sch2 (budget - workDone) pid mailbox (newStep :: revTrace) newProgram

            recur1 : Scheduler -> Step -> Program -> ( Scheduler, Program, List Step )
            recur1 =
                recur 1
        in
        case program of
            Work label amount k ->
                case compare amount budget of
                    LT ->
                        -- do this work _and then some_
                        recur amount
                            sch
                            (DidWork { worker = pid, label = label, amount = amount })
                            (k ())

                    EQ ->
                        -- finish doing this work then yield
                        recur amount
                            sch
                            (DidWork { worker = pid, label = label, amount = amount })
                            (k ())

                    GT ->
                        -- do only a part of this work
                        recur budget
                            sch
                            (DidWork { worker = pid, label = label, amount = budget })
                            (Work label (amount - budget) k)

            GetSelfPid kp ->
                recur1 sch (DidGetSelfPid { worker = pid }) (kp pid)

            SendMessage recipientPid message k ->
                case Dict.get recipientPid sch.procs of
                    Nothing ->
                        -- TODO: we might want to throw an error in this process instead?
                        recur1 sch
                            (DidTryToSendMessageToNonexistentPid { worker = pid, recipient = recipientPid, message = message })
                            (k ())

                    Just recipientProc ->
                        let
                            shouldEnqueue =
                                case recipientProc.state of
                                    WaitingForMsg ->
                                        True

                                    ReadyToRun ->
                                        False

                                    EndedNormally ->
                                        False

                                    Crashed _ ->
                                        False

                            newRecipientProc =
                                recipientProc
                                    |> Proc.addToMailbox message
                                    |> (if shouldEnqueue then
                                            Proc.setState ReadyToRun

                                        else
                                            identity
                                       )
                        in
                        recur1
                            (sch
                                |> setProc recipientPid newRecipientProc
                                |> (if shouldEnqueue then
                                        enqueue recipientPid

                                    else
                                        identity
                                   )
                            )
                            (DidSendMessageTo { worker = pid, recipient = recipientPid, message = message })
                            (k ())

            Receive km ->
                -- TODO: should mailbox be a queue? zipper would make sense too, see below
                -- TODO PERF: hold tried unsuccessful msgs so we don't retry them on every msg?
                let
                    receive : List Message -> List Message -> ( Scheduler, Program, List Step )
                    receive revAcc restOfMailbox =
                        case restOfMailbox of
                            [] ->
                                -- didn't find the matching message
                                ( sch
                                    |> updateProc pid (Proc.setState WaitingForMsg)
                                , program
                                , DidTryToReceiveUnsuccessfully { worker = pid } :: revTrace
                                )

                            m :: ms ->
                                case km m of
                                    Nothing ->
                                        -- TODO: make these loops consume red.budget too?
                                        receive (m :: revAcc) ms

                                    Just program2 ->
                                        let
                                            sch2 =
                                                sch
                                                    |> updateProc pid
                                                        (Proc.setMailbox (List.reverse revAcc ++ ms))
                                        in
                                        recur1 sch2 (DidReceiveMsg { worker = pid, message = m }) program2
                in
                receive [] mailbox

            Spawn childProgram_ kp ->
                let
                    ( sch2, newPid ) =
                        sch |> spawn childProgram_
                in
                recur1 sch2 (DidSpawn { worker = pid, child = newPid }) (kp newPid)

            End ->
                ( sch
                    |> updateProc pid (Proc.setState EndedNormally)
                , End
                , DidEndNormally { worker = pid } :: revTrace
                )

            Crash reason ->
                ( sch
                    |> updateProc pid (Proc.setState (Crashed reason))
                , Crash reason
                , DidCrash { worker = pid, reason = reason } :: revTrace
                )


spawn : Program -> Scheduler -> ( Scheduler, PID )
spawn program sch =
    let
        pid =
            sch.nextUnusedPid
    in
    ( { sch
        | nextUnusedPid = pid + 1
        , procs = Dict.insert pid (Proc.init program) sch.procs
      }
        |> enqueue pid
    , pid
    )


updateProc : PID -> (Proc -> Proc) -> Scheduler -> Scheduler
updateProc pid f scheduler =
    { scheduler | procs = Dict.update pid (Maybe.map f) scheduler.procs }


setProc : PID -> Proc -> Scheduler -> Scheduler
setProc pid newProc scheduler =
    updateProc pid (always newProc) scheduler


enqueue : PID -> Scheduler -> Scheduler
enqueue pid scheduler =
    { scheduler | readyQueue = ReadyQueue.enqueue pid scheduler.readyQueue }


setReadyQueue : ReadyQueue -> Scheduler -> Scheduler
setReadyQueue readyQueue scheduler =
    { scheduler | readyQueue = readyQueue }


log : List Step -> Scheduler -> Scheduler
log trace scheduler =
    { scheduler | revTraces = trace :: scheduler.revTraces }


reductionsBudget : Scheduler -> Int
reductionsBudget scheduler =
    scheduler.reductionsBudget
