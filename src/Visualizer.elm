module Visualizer exposing (main)

import Browser
import Browser.Dom as Dom
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import PID exposing (PID)
import Proc exposing (Proc, State(..))
import Program exposing (example)
import ReadyQueue exposing (ReadyQueue)
import Scheduler exposing (Scheduler)
import Task
import Trace exposing (Step(..))


type alias Model =
    { history : Zipper Scheduler
    , budget : Int
    }


type Msg
    = StepForward
    | StepBackward
    | Reset
    | UpdateBudget String
    | HasScrolledToBottomOfTrace (Result Dom.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    initWithBudget 1


initWithBudget : Int -> ( Model, Cmd Msg )
initWithBudget budget =
    let
        initialScheduler : Scheduler
        initialScheduler =
            Scheduler.init
                { reductionsBudget = budget
                , program = example
                }
    in
    ( { history = Zipper.singleton initialScheduler
      , budget = budget
      }
    , Cmd.none
    )


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
            initWithBudget model.budget

        UpdateBudget budgetStr ->
            case String.toInt budgetStr of
                Just budget ->
                    initWithBudget budget

                Nothing ->
                    ( model, Cmd.none )

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
        [ h1 [] [ text "Scheduler Visualizer" ]
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
                    , div [ style "display" "flex", style "align-items" "center", style "gap" "5px" ]
                        [ label [] [ text "Budget:" ]
                        , input
                            [ value (String.fromInt model.budget)
                            , onInput UpdateBudget
                            , style "width" "60px"
                            , style "padding" "4px"
                            , style "font-family" "monospace"
                            , type_ "number"
                            , Html.Attributes.min "1"
                            ]
                            []
                        ]
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
            [ viewProcesses scheduler.procs
            , viewReadyQueue scheduler.readyQueue
            ]
        , viewTrace scheduler.trace
        ]


viewReadyQueue : ReadyQueue -> Html msg
viewReadyQueue readyQueue =
    let
        queueItems : List String
        queueItems =
            ReadyQueue.toList readyQueue |> List.map String.fromInt
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


viewProcesses : Dict.Dict PID Proc -> Html msg
viewProcesses procs =
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
                (Dict.toList procs
                    |> List.map viewProcessRow
                )
            ]
        ]


viewProcessRow : ( PID, Proc ) -> Html msg
viewProcessRow ( pid, proc ) =
    let
        stateColor : String
        stateColor =
            case proc.state of
                ReadyToRun ->
                    "greenyellow"

                WaitingForMsg ->
                    "orange"

                Ended ->
                    "lightgray"
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
            [ text (stateToString proc.state) ]
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
            [ viewProgram proc.program ]
        ]


stateToString : State -> String
stateToString state =
    case state of
        ReadyToRun ->
            "Ready to Run"

        WaitingForMsg ->
            "Waiting for Message"

        Ended ->
            "Ended"


viewProgram : Program.Program -> Html msg
viewProgram program =
    let
        programText : String
        programText =
            case program of
                Program.Work label amount _ ->
                    "Work: " ++ label ++ " (" ++ String.fromInt amount ++ " reductions)"

                Program.GetSelfPid _ ->
                    "GetSelfPid"

                Program.SendMessage recipientPid message _ ->
                    "SendMessage to PID " ++ String.fromInt recipientPid ++ ": " ++ message

                Program.Receive _ ->
                    "Receive"

                Program.Spawn _ _ ->
                    "Spawn"

                Program.End ->
                    "End"
    in
    div [ style "color" "#333", style "font-size" "0.9em" ] [ text programText ]


traceId : String
traceId =
    "trace"


viewTrace : List Step -> Html msg
viewTrace trace =
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
        [ h3 [] [ text "Execution Trace" ]
        , div
            [ style "background" "#f9f9f9"
            , style "padding" "10px"
            , style "border-radius" "4px"
            ]
            [ div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "2px"
                , style "height" "200px"
                , style "max-height" "200px"
                , style "overflow-y" "auto"
                , id traceId
                ]
                (if List.isEmpty trace then
                    [ text "No steps yet" ]

                 else
                    List.map viewTraceStep trace
                )
            ]
        ]


viewTraceStep : Step -> Html msg
viewTraceStep step =
    let
        stepText : String
        stepText =
            case step of
                DidWork { worker, label, amount } ->
                    "PID " ++ String.fromInt worker ++ " did work: " ++ label ++ " (" ++ String.fromInt amount ++ " reductions)"

                DidGetSelfPid { worker } ->
                    "PID " ++ String.fromInt worker ++ " got self PID"

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

                DidEnd { worker } ->
                    "PID " ++ String.fromInt worker ++ " ended"

                DidTryToRunNonexistentProcess { process } ->
                    "Tried to run nonexistent process PID " ++ String.fromInt process

                NothingInTheReadyQueue ->
                    "Nothing in the ready queue"
    in
    div [ style "padding" "2px 0" ] [ text stepText ]


isFinished : Scheduler -> Bool
isFinished scheduler =
    case ReadyQueue.dequeue scheduler.readyQueue of
        Nothing ->
            True

        Just _ ->
            False


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
