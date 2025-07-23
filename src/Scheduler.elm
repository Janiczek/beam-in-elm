module Scheduler exposing (Scheduler, init, reductionsBudget, step)

import Dict exposing (Dict)
import PID exposing (PID)
import Proc exposing (Proc, State(..))
import Program exposing (Expr(..), Message, Program, Stmt(..))
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


type alias Environment =
    Dict String PID


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
                        ( sch2, program2, trace ) =
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
                        |> log trace
                        |> (if shouldReenqueue then
                                enqueue pid

                            else
                                identity
                           )


stepProgram : Scheduler -> Int -> PID -> Proc -> ( Scheduler, Program, List Step )
stepProgram sch budget pid proc =
    stepProgram_ sch budget pid proc.mailbox [] Dict.empty proc.program


stepProgram_ : Scheduler -> Int -> PID -> List Message -> List Step -> Environment -> Program -> ( Scheduler, Program, List Step )
stepProgram_ sch budget pid mailbox trace env program =
    if budget <= 0 then
        ( sch, program, trace )

    else
        let
            recur : Int -> Scheduler -> List Step -> Environment -> Program -> ( Scheduler, Program, List Step )
            recur workDone sch2 newTrace newEnv newProgram =
                stepProgram_ sch2 (budget - workDone) pid mailbox (trace ++ newTrace) newEnv newProgram

            recur1 : Scheduler -> List Step -> Environment -> Program -> ( Scheduler, Program, List Step )
            recur1 =
                recur 1
        in
        case program of
            [] ->
                ( sch, [], trace )

            stmt :: rest ->
                case stmt of
                    Work label amount ->
                        let
                            workAmount =
                                min amount budget
                        in
                        if workAmount == amount then
                            -- Work is complete, continue with rest
                            recur workAmount sch [ DidWork { worker = pid, label = label, amount = workAmount } ] env rest

                        else
                            -- Work is not complete, put remaining work back at front
                            let
                                remainingWork =
                                    Work label (amount - workAmount)
                            in
                            ( sch
                            , remainingWork :: rest
                            , trace ++ [ DidWork { worker = pid, label = label, amount = workAmount } ]
                            )

                    Let varName expr ->
                        let
                            ( sch2, value, trace1 ) =
                                stepExpr sch pid env expr
                        in
                        recur1 sch2 trace1 (Dict.insert varName value env) rest

                    SendMessage recipientExpr message ->
                        let
                            ( sch2, recipientPid, trace1 ) =
                                stepExpr sch pid env recipientExpr
                        in
                        case Dict.get recipientPid sch2.procs of
                            Nothing ->
                                recur1 sch2 (trace1 ++ [ DidTryToSendMessageToNonexistentPid { worker = pid, recipient = recipientPid, message = message } ]) env rest

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

                                    sch3 =
                                        sch2
                                            |> setProc recipientPid newRecipientProc
                                            |> (if shouldEnqueue then
                                                    enqueue recipientPid

                                                else
                                                    identity
                                               )
                                in
                                recur1 sch3 (trace1 ++ [ DidSendMessageTo { worker = pid, recipient = recipientPid, message = message } ]) env rest

                    Receive patterns ->
                        let
                            receive : List Message -> List Message -> ( Scheduler, Program, List Step )
                            receive revAcc restOfMailbox =
                                case restOfMailbox of
                                    [] ->
                                        -- didn't find the matching message
                                        let
                                            sch2 =
                                                sch
                                                    |> updateProc pid (Proc.setState WaitingForMsg)
                                        in
                                        ( sch2, program, trace ++ [ DidTryToReceiveUnsuccessfully { worker = pid } ] )

                                    m :: ms ->
                                        case findMatchingPattern m patterns of
                                            Nothing ->
                                                -- TODO: make these loops consume red.budget too?
                                                receive (m :: revAcc) ms

                                            Just matchingPattern ->
                                                let
                                                    sch2 =
                                                        sch
                                                            |> updateProc pid
                                                                (Proc.setMailbox (List.reverse revAcc ++ ms))
                                                in
                                                recur1 sch2 [ DidReceiveMsg { worker = pid, message = m } ] env (matchingPattern.body ++ rest)
                        in
                        receive [] mailbox

                    ExprStmt expr ->
                        let
                            ( sch2, _, trace1 ) =
                                stepExpr sch pid env expr
                        in
                        recur1 sch2 trace1 env rest

                    End ->
                        ( sch
                            |> updateProc pid (Proc.setState EndedNormally)
                        , []
                        , trace ++ [ DidEndNormally { worker = pid } ]
                        )

                    Crash reason ->
                        ( sch
                            |> updateProc pid (Proc.setState (Crashed reason))
                        , []
                        , trace ++ [ DidCrash { worker = pid, reason = reason } ]
                        )


stepExpr : Scheduler -> PID -> Environment -> Expr -> ( Scheduler, PID, List Step )
stepExpr sch pid env expr =
    case expr of
        GetSelfPid ->
            ( sch, pid, [ DidGetSelfPid { worker = pid } ] )

        Spawn childProgram_ ->
            let
                ( sch2, newPid ) =
                    spawn childProgram_ sch
            in
            ( sch2, newPid, [ DidSpawn { worker = pid, child = newPid } ] )

        Var varName ->
            ( sch, Dict.get varName env |> Maybe.withDefault 0, [] )


findMatchingPattern : Message -> List { message : String, body : Program } -> Maybe { message : String, body : Program }
findMatchingPattern msg patterns =
    List.head (List.filter (\pattern -> pattern.message == msg) patterns)


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
