module Demo2 exposing (main)

import Browser
import Browser.Dom as Dom
import Scheduler exposing (Pid, Proc, Scheduler, Step(..), WorkType(..))
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Queue exposing (Queue)
import Task


type ProcessState
    = ReadyToRun
    | WaitingForMsg
    | EndedNormally
    | Crashed
    | InQueue
    | Zombie


type alias Model =
    { history : Zipper Scheduler
    }


type Msg
    = StepForward
    | StepBackward
    | Reset
    | HasScrolledToBottomOfTrace (Result Dom.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    initDefault


initDefault : ( Model, Cmd Msg )
initDefault =
    initWithProgram Scheduler.ex2


initWithProgram : Scheduler.Program -> ( Model, Cmd Msg )
initWithProgram program =
    let
        initialScheduler : Scheduler
        initialScheduler =
            Scheduler.init { program = program, workType = AllAtOnce }
    in
    ( { history = Zipper.singleton initialScheduler }
    , Cmd.none
    )


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


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt HasScrolledToBottomOfTrace


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StepForward ->
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
            ( { model | history = newHistory }
            , jumpToBottom traceId
            )

        StepBackward ->
            case Zipper.prev model.history of
                Just newHistory ->
                    ( { model | history = newHistory }
                    , jumpToBottom traceId
                    )

                Nothing ->
                    ( model, Cmd.none )

        Reset ->
            initDefault

        HasScrolledToBottomOfTrace _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        currentScheduler : Scheduler
        currentScheduler =
            Zipper.current model.history

        canStepBackward : Bool
        canStepBackward =
            Zipper.hasPrev model.history

        canStepForward : Bool
        canStepForward =
            not (isFinished currentScheduler)
    in
    div [ style "padding" "20px", style "font-family" "monospace" ]
        [ h1 [] [ text "Demo 2: Work" ]
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "20px" ]
            [ div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                [ div [ style "display" "flex", style "gap" "10px", style "align-items" "center" ]
                    [ button
                        [ onClick StepBackward
                        , disabled (not canStepBackward)
                        , style "padding" "8px 16px"
                        ]
                        [ text "← Step Backward" ]
                    , button
                        [ onClick StepForward
                        , disabled (not canStepForward)
                        , style "padding" "8px 16px"
                        ]
                        [ text "Step Forward →" ]
                    , button
                        [ onClick Reset
                        , style "padding" "8px 16px"
                        ]
                        [ text "Reset" ]
                    ]
                , div [ style "color" "#666" ]
                    [ text ("Step " ++ String.fromInt (List.length (Zipper.listPrev model.history) + 1)) ]
                ]
            , viewScheduler currentScheduler
            ]
        ]


viewScheduler : Scheduler -> Html msg
viewScheduler scheduler =
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "20px" ]
        [ div [ style "display" "flex", style "gap" "20px", style "align-items" "start" ]
            [ viewProgram scheduler
            ]
        , viewTraces (List.reverse scheduler.revTraces)
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
                    case proc.program of

                        Scheduler.End ->
                            "End"
                        Scheduler.Work amount _ ->
                            "Work: " ++ String.fromInt amount
                        _ -> "Shouldn't show at this point yet"

        processState : ProcessState
        processState =
            case Dict.values scheduler.procs of
                [] ->
                    Zombie

                proc :: _ ->
                    case Dict.keys scheduler.procs of
                        [] ->
                            Zombie

                        pid :: _ ->
                            deriveProcessState scheduler pid proc

        stateColor : String
        stateColor =
            processStateToColor processState

        stateText : String
        stateText =
            processStateToText processState
    in
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
        [ h3 [] [ text "Program" ]
        , div
            [ style "background" stateColor
            , style "padding" "10px"
            , style "border-radius" "4px"
            , style "font-family" "monospace"
            ]
            [ div [ style "font-weight" "bold" ] [ text stateText ]
            , div [ style "margin-top" "5px" ] [ text programText ]
            ]
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
                [ text "No steps yet" ]

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


traceId : String
traceId =
    "trace"


stepToString : Step -> String
stepToString step =
    case step of
        DidWork { amount } ->
            "Did work: " ++ String.fromInt amount

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

        DidEndNormally _ ->
            "Ended normally"

        DidCrash { worker } ->
            "PID " ++ String.fromInt worker ++ " crashed"

        DidTryToRunNonexistentProcess { process } ->
            "Tried to run nonexistent process PID " ++ String.fromInt process

        NothingInTheReadyQueue ->
            "Nothing in the ready queue"


isFinished : Scheduler -> Bool
isFinished scheduler =
    case Queue.dequeue scheduler.readyQueue of
        ( Nothing, _ ) ->
            True

        ( Just _, _ ) ->
            False


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
