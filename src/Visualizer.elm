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
import Trace exposing (Step(..), Trace)


type alias Model =
    { history : Zipper Scheduler
    , budget : String
    }


type Msg
    = StepForward
    | StepBackward
    | Reset
    | UpdateBudget String
    | ResetWithBudget Int
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
      , budget = String.fromInt budget
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
            model.budget
                |> String.toInt
                |> Maybe.withDefault 1
                |> initWithBudget

        UpdateBudget budgetStr ->
            ( { model | budget = budgetStr }, Cmd.none )

        ResetWithBudget budgetInt ->
            initWithBudget budgetInt

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
                    , div
                        [ style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "5px"
                        ]
                        [ label [] [ text "Budget:" ]
                        , input
                            [ value model.budget
                            , onInput UpdateBudget
                            , style "width" "60px"
                            , style "padding" "4px"
                            , style "font-family" "monospace"
                            , type_ "number"
                            ]
                            []
                        , button
                            [ case String.toInt model.budget of
                                Just budget ->
                                    if budget >= 1 then
                                        onClick (ResetWithBudget budget)

                                    else
                                        disabled True

                                Nothing ->
                                    disabled True
                            , style "padding" "8px 16px"
                            ]
                            [ text "Reset with budget" ]
                        ]
                    ]
                , div [ style "color" "#666" ]
                    [ text ("Step " ++ String.fromInt (List.length (Zipper.listPrev model.history) + 1)) ]
                , div [ style "color" "#666" ]
                    [ text ("Used budget: " ++ String.fromInt (Scheduler.reductionsBudget currentScheduler)) ]
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
        , viewTraces (List.reverse scheduler.revTraces)
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

                EndedNormally ->
                    "lightgray"

                Crashed _ ->
                    "red"
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

        EndedNormally ->
            "Ended Normally"

        Crashed reason ->
            "Crashed: " ++ reason


viewProgram : Program.Program -> Html msg
viewProgram program =
    let
        exprToString : Program.Expr -> String
        exprToString expr =
            case expr of
                Program.GetSelfPid ->
                    "GetSelfPid"

                Program.Spawn childProgram_ ->
                    "Spawn(...)"

                Program.Var varName ->
                    varName

        stmtToString : Program.Stmt -> String
        stmtToString stmt =
            case stmt of
                Program.Work label amount ->
                    "Work: " ++ label ++ " (" ++ String.fromInt amount ++ " reductions)"

                Program.Let varName expr ->
                    "Let " ++ varName ++ " = " ++ exprToString expr

                Program.SendMessage recipientExpr message ->
                    "SendMessage(" ++ exprToString recipientExpr ++ ", \"" ++ message ++ "\")"

                Program.Receive patterns ->
                    "Receive (" ++ String.fromInt (List.length patterns) ++ " patterns)"

                Program.ExprStmt expr ->
                    "ExprStmt(" ++ exprToString expr ++ ")"

                Program.End ->
                    "End"

                Program.Crash reason ->
                    "Crash: " ++ reason

        programText : String
        programText =
            if List.isEmpty program then
                "Empty Program"

            else
                program
                    |> List.map stmtToString
                    |> String.join "\n"
    in
    div
        [ style "color" "#333"
        , style "font-size" "0.9em"
        , style "white-space" "pre"
        ]
        [ text programText ]


traceId : String
traceId =
    "trace"


viewTraces : List Trace -> Html msg
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


stepToString : Step -> String
stepToString step =
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

        DidEndNormally { worker } ->
            "PID " ++ String.fromInt worker ++ " ended normally"

        DidCrash { worker, reason } ->
            "PID " ++ String.fromInt worker ++ " crashed: " ++ reason

        DidTryToRunNonexistentProcess { process } ->
            "Tried to run nonexistent process PID " ++ String.fromInt process

        NothingInTheReadyQueue ->
            "Nothing in the ready queue"


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
