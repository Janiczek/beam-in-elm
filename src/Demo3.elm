module Demo3 exposing (main)

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
    initWithProgram Scheduler.ex3



initWithProgram : Scheduler.Program -> ( Model, Cmd Msg )
initWithProgram program =
    let
        initialScheduler : Scheduler
        initialScheduler =
            Scheduler.init { workType = AllAtOnce, program = program }
    in
    ( { history = Zipper.singleton initialScheduler
      }
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
                init ()


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
        [ h1 [] [ text "Demo 3: Spawn" ]
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
            [ viewProcesses scheduler
            , viewReadyQueue scheduler.readyQueue
            ]
        , viewTraces (List.reverse scheduler.revTraces)
        ]


viewReadyQueue : Queue Pid -> Html msg
viewReadyQueue readyQueue =
    let
        queueItems : List String
        queueItems =
            Queue.toList readyQueue |> List.map String.fromInt
    in
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
        [ h3 [] [ text "Ready Queue (top = next to run)" ]
        , div [ style "background" "#f5f5f5", style "padding" "10px", style "border-radius" "4px" ]
            [ if List.isEmpty queueItems then
                text "Empty"

              else
                ul [] (List.map (\item -> li [] [ text ("PID " ++ item) ]) queueItems)
            ]
        ]


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
            [ viewProgram proc.program ]
        ]


viewProgram : Scheduler.Program -> Html msg
viewProgram program =
    let
        programText : String
        programText =
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
    in
    div
        [ style "color" "#333" ]
        [ text programText ]


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
        DidWork { worker, amount } ->
            "PID " ++ String.fromInt worker ++ " did work: " ++ String.fromInt amount

        DidSpawn { worker, child } ->
            "PID " ++ String.fromInt worker ++ " spawned child PID " ++ String.fromInt child

        DidEndNormally { worker } ->
            "PID " ++ String.fromInt worker ++ " ended normally"

        DidTryToRunNonexistentProcess { process } ->
            "Tried to run nonexistent process PID " ++ String.fromInt process

        NothingInTheReadyQueue ->
            "Nothing in the ready queue"
        _ -> "Shouldn't show at this point yet"


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
