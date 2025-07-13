module Visualizer exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import PID exposing (PID)
import Proc exposing (Proc, State(..))
import Program exposing (example)
import ReadyQueue exposing (ReadyQueue)
import Scheduler exposing (Scheduler)
import Trace exposing (Step(..))


type alias Model =
    { history : Zipper Scheduler
    }


type Msg
    = StepForward
    | StepBackward
    | Reset


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialScheduler : Scheduler
        initialScheduler =
            Scheduler.init example
    in
    ( { history = Zipper.singleton initialScheduler
      }
    , Cmd.none
    )


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
            , Cmd.none
            )

        StepBackward ->
            case Zipper.prev model.history of
                Just newHistory ->
                    ( { model | history = newHistory }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Reset ->
            let
                initialScheduler : Scheduler
                initialScheduler =
                    Scheduler.init example
            in
            ( { history = Zipper.singleton initialScheduler }
            , Cmd.none
            )


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
        , div [ style "margin-bottom" "20px" ]
            [ button
                [ onClick StepBackward
                , disabled (not canStepBackward)
                , style "margin-right" "10px"
                , style "padding" "8px 16px"
                ]
                [ text "← Step Backward" ]
            , button
                [ onClick StepForward
                , disabled (not canStepForward)
                , style "margin-right" "10px"
                , style "padding" "8px 16px"
                ]
                [ text "Step Forward →" ]
            , button
                [ onClick Reset
                , style "padding" "8px 16px"
                ]
                [ text "Reset" ]
            , div [ style "margin-top" "10px", style "color" "#666" ]
                [ text ("Step " ++ String.fromInt (List.length (Zipper.listPrev model.history) + 1)) ]
            ]
        , viewScheduler currentScheduler
        ]


viewScheduler : Scheduler -> Html msg
viewScheduler scheduler =
    div []
        [ viewReadyQueue scheduler.readyQueue
        , viewProcesses scheduler.procs
        , viewTrace scheduler.trace
        ]


viewReadyQueue : ReadyQueue -> Html msg
viewReadyQueue readyQueue =
    let
        queueItems : List String
        queueItems =
            ReadyQueue.toList readyQueue |> List.map String.fromInt
    in
    div [ style "margin-bottom" "20px" ]
        [ h3 [] [ text "Ready Queue" ]
        , div [ style "background" "#f5f5f5", style "padding" "10px", style "border-radius" "4px" ]
            [ if List.isEmpty queueItems then
                text "Empty"

              else
                ul [] (List.map (\item -> li [] [ text ("PID " ++ item) ]) queueItems)
            ]
        ]


viewProcesses : Dict.Dict PID Proc -> Html msg
viewProcesses procs =
    div [ style "margin-bottom" "20px" ]
        [ h3 [] [ text "Processes" ]
        , div []
            (Dict.toList procs
                |> List.map viewProcess
            )
        ]


viewProcess : ( PID, Proc ) -> Html msg
viewProcess ( pid, proc ) =
    let
        stateColor : String
        stateColor =
            case proc.state of
                ReadyToRun ->
                    "#4CAF50"

                WaitingForMsg ->
                    "#FF9800"
    in
    div
        [ style "border" "1px solid #ddd"
        , style "margin" "5px 0"
        , style "padding" "10px"
        , style "border-radius" "4px"
        , style "border-left" ("4px solid " ++ stateColor)
        ]
        [ div [ style "font-weight" "bold" ] [ text ("PID " ++ String.fromInt pid) ]
        , div [ style "color" "#666", style "font-size" "0.9em" ]
            [ text ("State: " ++ stateToString proc.state) ]
        , div [ style "margin-top" "5px" ]
            [ div []
                [ div [ style "font-weight" "bold", style "margin-bottom" "5px" ] [ text "Mailbox:" ]
                , if List.isEmpty proc.mailbox then
                    div [ style "color" "#999", style "font-style" "italic" ] [ text "Empty" ]

                  else
                    ul [ style "margin" "0", style "padding-left" "20px" ]
                        (List.map (\message -> li [] [ text message ]) proc.mailbox)
                ]
            ]
        , viewProgram proc.program
        ]


stateToString : State -> String
stateToString state =
    case state of
        ReadyToRun ->
            "Ready to Run"

        WaitingForMsg ->
            "Waiting for Message"


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


viewTrace : List Step -> Html msg
viewTrace trace =
    div [ style "margin-bottom" "20px" ]
        [ h3 [] [ text "Execution Trace" ]
        , div [ style "background" "#f9f9f9", style "padding" "10px", style "border-radius" "4px", style "max-height" "300px", style "overflow-y" "auto" ]
            [ if List.isEmpty trace then
                text "No steps yet"

              else
                div [] (List.map viewTraceStep (List.reverse trace))
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
    div [ style "margin" "2px 0", style "padding" "2px 0" ] [ text stepText ]


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
