module Shared exposing
    ( ProcessState(..)
    , SchedulerViewMode(..)
    , deriveProcessState
    , handleHasScrolledToBottomOfTrace
    , handleStepBackward
    , handleStepForward
    , isFinished
    , isReceive
    , jumpToBottomOfTraces
    , processStateToColor
    , processStateToText
    , stepToString
    , traceId
    , viewCodeExample
    , viewDemoLayout
    , viewProcesses
    , viewProcessesWithMailbox
    , viewProgram
    , viewReadyQueue
    , viewScheduler
    , viewTraces
    , workTypeToString
    )

import Browser.Dom as Dom
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Queue exposing (Queue)
import Scheduler exposing (Pid, Proc, Scheduler, Step(..))
import String.Extra
import Task


type ProcessState
    = ReadyToRun
    | WaitingForMsg
    | EndedNormally
    | Crashed
    | InQueue
    | Zombie


type SchedulerViewMode
    = SimpleProgram
    | ProcessTable
    | ProcessTableWithMailbox


deriveProcessState : Scheduler -> Pid -> Proc -> ProcessState
deriveProcessState scheduler pid proc =
    let
        isFirstInQueue : Bool
        isFirstInQueue =
            Queue.front scheduler.readyQueue == Just pid

        isInQueue : Bool
        isInQueue =
            Queue.toList scheduler.readyQueue
                |> List.member pid
    in
    if isFirstInQueue then
        ReadyToRun

    else if isInQueue then
        InQueue

    else if proc.program == Scheduler.End then
        EndedNormally

    else if proc.program == Scheduler.Crash then
        Crashed

    else if isReceive proc.program then
        WaitingForMsg

    else
        Zombie


isReceive : Scheduler.Program -> Bool
isReceive program =
    case program of
        Scheduler.Receive _ ->
            True

        _ ->
            False


processStateToColor : ProcessState -> String
processStateToColor state =
    case state of
        ReadyToRun ->
            "greenyellow"

        WaitingForMsg ->
            "orange"

        EndedNormally ->
            "lightgray"

        Crashed ->
            "red"

        InQueue ->
            "transparent"

        Zombie ->
            "cyan"


processStateToText : ProcessState -> String
processStateToText state =
    case state of
        ReadyToRun ->
            "Will run next"

        WaitingForMsg ->
            "Waiting for message"

        EndedNormally ->
            "Ended normally"

        Crashed ->
            "Crashed"

        InQueue ->
            "In queue"

        Zombie ->
            "Zombie (bug?)"


traceId : String
traceId =
    "trace"


jumpToBottomOfTraces : Cmd (Result Dom.Error ())
jumpToBottomOfTraces =
    Dom.getViewportOf traceId
        |> Task.andThen (\info -> Dom.setViewportOf traceId 0 info.scene.height)
        |> Task.attempt identity


isFinished : Scheduler -> Bool
isFinished scheduler =
    case Queue.dequeue scheduler.readyQueue of
        ( Nothing, _ ) ->
            True

        ( Just _, _ ) ->
            False


stepToString : Step -> String
stepToString step =
    case step of
        DidWork { worker, amount } ->
            "PID " ++ String.fromInt worker ++ " did work: " ++ String.fromInt amount ++ " reductions"

        DidSendMessageTo { worker, recipient, message } ->
            "PID " ++ String.fromInt worker ++ " sent message to PID " ++ String.fromInt recipient ++ ": " ++ message

        DidTryToSendMessageToNonexistentPid { worker, recipient, message } ->
            "PID " ++ String.fromInt worker ++ " tried to send message to nonexistent PID " ++ String.fromInt recipient ++ ": " ++ message

        DidReceiveMsg { worker, message } ->
            "PID " ++ String.fromInt worker ++ " received message: " ++ message

        DidTryToReceiveUnsuccessfully { worker } ->
            "PID " ++ String.fromInt worker ++ " tried to receive but no matching message found"

        DidSpawn { worker, child } ->
            "PID " ++ String.fromInt worker ++ " spawned child PID " ++ String.fromInt child

        DidLink { worker, linked } ->
            "PID " ++ String.fromInt worker ++ " linked to PID " ++ String.fromInt linked

        DidUnsuccessfullyTryToLink { worker, linked } ->
            "PID " ++ String.fromInt worker ++ " tried to link to PID " ++ String.fromInt linked ++ " (unsuccessfully)"

        DidSpawnLink { worker, child } ->
            "PID " ++ String.fromInt worker ++ " spawned and linked child PID " ++ String.fromInt child

        DidEndNormally { worker } ->
            "PID " ++ String.fromInt worker ++ " ended normally"

        DidCrash { worker } ->
            "PID " ++ String.fromInt worker ++ " crashed"

        DidTryToRunNonexistentProcess { process } ->
            "Tried to run nonexistent process PID " ++ String.fromInt process

        NothingInTheReadyQueue ->
            "Nothing in the ready queue"


viewProcesses : Scheduler -> Html msg
viewProcesses scheduler =
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
        [ h3 [] [ text "Processes" ]
        , table
            [ style "border-collapse" "collapse"
            , style "font-family" "monospace"
            , style "font-size" "14px"
            , style "width" "fit-content"
            ]
            [ thead []
                [ tr []
                    [ th [ style "padding" "8px", style "text-align" "left", style "border-bottom" "2px solid #ddd", style "background" "#f5f5f5", style "width" "5ch" ] [ text "PID" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "border-bottom" "2px solid #ddd", style "background" "#f5f5f5", style "width" "20ch" ] [ text "State" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "border-bottom" "2px solid #ddd", style "background" "#f5f5f5", style "width" "40ch" ] [ text "Program" ]
                    ]
                ]
            , tbody []
                (Dict.toList scheduler.procs
                    |> List.map (viewProcessRow scheduler)
                )
            ]
        ]


viewProcessesWithMailbox : Scheduler -> Html msg
viewProcessesWithMailbox scheduler =
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
        [ h3 [] [ text "Processes" ]
        , table
            [ style "border-collapse" "collapse"
            , style "font-family" "monospace"
            , style "font-size" "14px"
            , style "width" "fit-content"
            ]
            [ thead []
                [ tr []
                    [ th [ style "padding" "8px", style "text-align" "left", style "border-bottom" "2px solid #ddd", style "background" "#f5f5f5", style "width" "5ch" ] [ text "PID" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "border-bottom" "2px solid #ddd", style "background" "#f5f5f5", style "width" "20ch" ] [ text "State" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "border-bottom" "2px solid #ddd", style "background" "#f5f5f5", style "width" "20ch" ] [ text "Mailbox" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "border-bottom" "2px solid #ddd", style "background" "#f5f5f5", style "width" "40ch" ] [ text "Program" ]
                    ]
                ]
            , tbody []
                (Dict.toList scheduler.procs
                    |> List.map (viewProcessRowWithMailbox scheduler)
                )
            ]
        ]


viewProcessRow : Scheduler -> ( Pid, Proc ) -> Html msg
viewProcessRow scheduler ( pid, proc ) =
    let
        processState : ProcessState
        processState =
            deriveProcessState scheduler pid proc

        stateColor : String
        stateColor =
            processStateToColor processState

        stateText : String
        stateText =
            processStateToText processState
    in
    tr []
        [ td
            [ style "padding" "8px"
            , style "font-weight" "bold"
            , style "background" stateColor
            ]
            [ text (String.fromInt pid) ]
        , td
            [ style "padding" "8px"
            , style "background" stateColor
            ]
            [ text stateText ]
        , td
            [ style "padding" "8px"
            , style "background" stateColor
            ]
            [ viewProgramText proc.program ]
        ]


viewProcessRowWithMailbox : Scheduler -> ( Pid, Proc ) -> Html msg
viewProcessRowWithMailbox scheduler ( pid, proc ) =
    let
        processState : ProcessState
        processState =
            deriveProcessState scheduler pid proc

        stateColor : String
        stateColor =
            processStateToColor processState

        stateText : String
        stateText =
            processStateToText processState
    in
    tr []
        [ td
            [ style "padding" "8px"
            , style "font-weight" "bold"
            , style "background" stateColor
            ]
            [ text (String.fromInt pid) ]
        , td
            [ style "padding" "8px"
            , style "background" stateColor
            ]
            [ text stateText ]
        , td
            [ style "padding" "8px"
            , style "background" stateColor
            ]
            [ if List.isEmpty proc.mailbox then
                text "Empty"

              else
                div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "gap" "2px"
                    ]
                    (List.map (\message -> div [] [ text ("- " ++ message) ]) proc.mailbox)
            ]
        , td
            [ style "padding" "8px"
            , style "background" stateColor
            ]
            [ viewProgramText proc.program ]
        ]


viewProgram : Scheduler -> Html msg
viewProgram scheduler =
    let
        programText : String
        programText =
            case Dict.values scheduler.procs of
                [] ->
                    "No program"

                proc :: _ ->
                    programToText proc.program

        processState : ProcessState
        processState =
            case Dict.toList scheduler.procs of
                [] ->
                    Zombie

                ( pid, proc ) :: _ ->
                    deriveProcessState scheduler pid proc

        stateColor : String
        stateColor =
            processStateToColor processState

        stateText : String
        stateText =
            processStateToText processState
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        [ h3 [] [ text "Program" ]
        , div
            [ style "background" stateColor
            , style "padding" "10px"
            , style "border-radius" "4px"
            , style "font-family" "'JetBrains Mono'"
            ]
            [ div
                [ style "font-weight" "bold" ]
                [ text stateText ]
            , div
                [ style "margin-top" "5px" ]
                [ text programText ]
            ]
        ]

programToText : Scheduler.Program -> String
programToText program =
            case program of
                Scheduler.Work amount _ ->
                    "Work: " ++ String.fromInt amount

                Scheduler.End ->
                    "End"

                Scheduler.Spawn _ _ ->
                    "Spawn"

                Scheduler.Send recipientPid message _ ->
                    "Send to PID " ++ String.fromInt recipientPid ++ ": " ++ message

                Scheduler.Receive ( expectedMessage, _ ) ->
                    "Receive: " ++ expectedMessage

                Scheduler.Crash ->
                    "Crash"

                Scheduler.Link targetPid _ ->
                    "Link to PID " ++ String.fromInt targetPid

                Scheduler.SpawnLink _ _ ->
                    "SpawnLink"

viewProgramText : Scheduler.Program -> Html msg
viewProgramText program =
    div
        [ style "color" "#333" ]
        [ text (programToText program) ]


viewReadyQueue : Queue Pid -> Html msg
viewReadyQueue readyQueue =
    let
        queueItems : List String
        queueItems =
            Queue.toList readyQueue |> List.map String.fromInt
    in
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
        [ h3 [] [ text "Ready Queue" ]
        , div [ style "background" "#f5f5f5", style "padding" "10px", style "border-radius" "4px" ]
            [ ul
                [ style "list-style-type" "none"
                , style "padding" "0"
                , style "text-align" "center"
                ]
                (if List.isEmpty queueItems then
                    [ li [] [ text "Empty" ] ]

                 else
                    queueItems
                        |> List.indexedMap
                            (\i item ->
                                let
                                    displayText =
                                        if i == 0 then
                                            "➡️ PID " ++ item ++ " ⬅️"

                                        else
                                            "PID " ++ item
                                in
                                li [] [ text displayText ]
                            )
                )
            ]
        ]


viewScheduler : SchedulerViewMode -> Scheduler -> String -> Html msg
viewScheduler mode scheduler code =
    case mode of
        SimpleProgram ->
            div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "20px"
                ]
                [ div
                    [ style "display" "flex"
                    , style "gap" "20px"
                    , style "align-items" "start"
                    ]
                    [ viewCodeExample code
                    , viewProgram scheduler
                    ]
                , viewTraces (List.reverse scheduler.revTraces)
                ]

        ProcessTable ->
            div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "20px"
                ]
                [ div
                    [ style "display" "flex"
                    , style "gap" "20px"
                    , style "align-items" "start"
                    ]
                    [ viewCodeExample code
                    , viewProcesses scheduler
                    , viewReadyQueue scheduler.readyQueue
                    ]
                , viewTraces (List.reverse scheduler.revTraces)
                ]

        ProcessTableWithMailbox ->
            div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "20px"
                ]
                [ div
                    [ style "display" "flex"
                    , style "gap" "20px"
                    , style "align-items" "start"
                    ]
                    [ viewCodeExample code
                    , viewProcessesWithMailbox scheduler
                    , viewReadyQueue scheduler.readyQueue
                    ]
                , viewTraces (List.reverse scheduler.revTraces)
                ]


viewTraces : List (List Step) -> Html msg
viewTraces traces =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        [ h3 [] [ text "Execution Trace" ]
        , div
            [ style "background" "#f9f9f9"
            , style "padding" "10px"
            , style "border-radius" "4px"
            , style "height" "200px"
            , style "max-height" "200px"
            , style "overflow-y" "auto"
            , id traceId
            ]
            (if List.isEmpty traces then
                [ div
                    [ style "margin" "2px 0"
                    , style "padding" "2px 0"
                    ]
                    [ text "No steps yet" ]
                ]

             else
                traces
                    |> List.map (List.map stepToString)
                    |> List.intersperse [ "----" ]
                    |> List.concat
                    |> List.map
                        (\step ->
                            div
                                [ style "margin" "2px 0"
                                , style "padding" "2px 0"
                                ]
                                [ text step ]
                        )
            )
        ]


viewCodeExample : String -> Html msg
viewCodeExample codeString =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        [ h3 [] [ text "Example" ]
        , div
            [ style "background" "#f5f5f5"
            , style "padding" "15px"
            , style "border-radius" "4px"
            , style "font-family" "'JetBrains Mono', monospace"
            , style "font-size" "14px"
            , style "white-space" "pre-wrap"
            , style "overflow-x" "auto"
            ]
            [ codeString
                |> String.Extra.unindent
                |> String.trim
                |> text
            ]
        ]



-- Shared update functions for common message handling


handleStepForward : { model | history : Zipper Scheduler } -> { model | history : Zipper Scheduler }
handleStepForward model =
    let
        currentScheduler : Scheduler
        currentScheduler =
            Zipper.current model.history

        nextScheduler : Scheduler
        nextScheduler =
            Scheduler.step currentScheduler

        newHistory : Zipper Scheduler
        newHistory =
            Zipper.consAfter nextScheduler model.history
    in
    { model | history = newHistory }


handleStepBackward : { model | history : Zipper Scheduler } -> { model | history : Zipper Scheduler }
handleStepBackward model =
    case Zipper.prev model.history of
        Just newHistory ->
            { model | history = newHistory }

        Nothing ->
            model


handleHasScrolledToBottomOfTrace : model -> model
handleHasScrolledToBottomOfTrace model =
    model



-- Shared view layout for demos


viewDemoLayout :
    { title : String
    , stepForward : msg
    , stepBackward : msg
    , reset : msg
    , history : Zipper Scheduler
    , schedulerMode : SchedulerViewMode
    , codeExample : String
    , additionalControls : List (Html msg)
    , budgetControls :
        Maybe
            { resetWithBudget : Int -> msg
            , updateBudget : String -> msg
            , budgetField : String
            }
    }
    -> Html msg
viewDemoLayout config =
    let
        currentScheduler : Scheduler
        currentScheduler =
            Zipper.current config.history

        canStepBackward : Bool
        canStepBackward =
            Zipper.hasPrev config.history

        canStepForward : Bool
        canStepForward =
            not (isFinished currentScheduler)

        stepNumber : Int
        stepNumber =
            List.length (Zipper.listPrev config.history) + 1
    in
    div
        [ style "padding" "20px"
        , style "font-family" "'JetBrains Mono'"
        ]
        [ h1 [ style "font-size" "24px" ] [ text config.title ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "20px"
            ]
            [ div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "10px"
                ]
                [ div
                    [ style "display" "flex"
                    , style "gap" "10px"
                    , style "align-items" "center"
                    , style "flex-wrap" "wrap"
                    ]
                    ([ button
                        [ onClick config.stepBackward
                        , disabled (not canStepBackward)
                        , style "padding" "8px 16px"
                        ]
                        [ text "← Step Backward" ]
                     , button
                        [ onClick config.stepForward
                        , disabled (not canStepForward)
                        , style "padding" "8px 16px"
                        ]
                        [ text "Step Forward →" ]
                     , button
                        [ onClick config.reset
                        , style "padding" "8px 16px"
                        ]
                        [ text "Reset" ]
                     ]
                        ++ config.additionalControls
                        ++ (case config.budgetControls of
                                Just budgetConfig ->
                                    [ div
                                        [ style "display" "flex"
                                        , style "align-items" "center"
                                        , style "gap" "5px"
                                        ]
                                        [ label [] [ text "Budget:" ]
                                        , input
                                            [ value budgetConfig.budgetField
                                            , onInput budgetConfig.updateBudget
                                            , style "width" "60px"
                                            , style "padding" "4px"
                                            , style "font-family" "monospace"
                                            , type_ "number"
                                            ]
                                            []
                                        , button
                                            [ case String.toInt budgetConfig.budgetField of
                                                Just budget ->
                                                    if budget >= 1 then
                                                        onClick (budgetConfig.resetWithBudget budget)

                                                    else
                                                        disabled True

                                                Nothing ->
                                                    disabled True
                                            , style "padding" "8px 16px"
                                            ]
                                            [ text "Reset with budget" ]
                                        , span
                                            [ style "color" "#666"
                                            , style "font-size" "14px"
                                            ]
                                            [ text ("Current budget: " ++ String.fromInt (Scheduler.currentBudget currentScheduler)) ]
                                        ]
                                    ]

                                Nothing ->
                                    []
                           )
                    )
                , div
                    [ style "color" "#666" ]
                    [ text ("Step " ++ String.fromInt stepNumber) ]
                ]
            , viewScheduler config.schedulerMode currentScheduler config.codeExample
            ]
        ]


workTypeToString : Scheduler.WorkType -> String
workTypeToString workType =
    case workType of
        Scheduler.AllAtOnce ->
            ""

        Scheduler.ReductionsBudget budget ->
            "Current budget: " ++ String.fromInt budget
