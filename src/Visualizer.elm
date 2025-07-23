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
            [ viewProcesses scheduler.procs scheduler.readyQueue scheduler
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


viewProcesses : Dict.Dict PID Proc -> ReadyQueue -> Scheduler -> Html msg
viewProcesses procs readyQueue scheduler =
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
                    [ th [ style "padding" "8px", style "text-align" "left", style "background" "#f5f5f5", style "width" "5ch" ] [ text "PID" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "background" "#f5f5f5", style "width" "20ch" ] [ text "State" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "background" "#f5f5f5", style "width" "20ch" ] [ text "Mailbox" ]
                    , th [ style "padding" "8px", style "text-align" "left", style "background" "#f5f5f5", style "width" "40ch" ] [ text "Program" ]
                    ]
                ]
            , tbody []
                (Dict.toList procs
                    |> List.map (\procTuple -> viewProcessRow procTuple readyQueue scheduler)
                )
            ]
        ]


viewProcessRow : ( PID, Proc ) -> ReadyQueue -> Scheduler -> Html msg
viewProcessRow ( pid, proc ) readyQueue scheduler =
    let
        isNextToRun : Bool
        isNextToRun =
            case ReadyQueue.dequeue readyQueue of
                Just ( nextPid, _ ) ->
                    nextPid == pid

                Nothing ->
                    False

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

        rowStyle : List (Html.Attribute msg)
        rowStyle =
            if isNextToRun then
                [ style "border" "2px solid #0066cc"
                , style "font-weight" "bold"
                ]

            else
                []
    in
    tr rowStyle
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
            [ viewProgramWithHighlighting proc.originalProgram proc.currentStmtId scheduler ]
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


viewProgramWithHighlighting : Program.Program -> Maybe Program.StmtId -> Scheduler -> Html msg
viewProgramWithHighlighting originalProgram currentStmtId scheduler =
    let
        exprToString : Program.Expr -> String
        exprToString expr =
            case expr of
                Program.GetSelfPid ->
                    "GetSelfPid"

                Program.Spawn _ ->
                    "Spawn(...)"

                Program.Var varName ->
                    varName

        stmtToString : Program.Stmt -> String
        stmtToString stmt =
            case stmt of
                Program.Work _ label amount ->
                    "Work: " ++ label ++ " (" ++ String.fromInt amount ++ " reductions)"

                Program.Let _ varName expr ->
                    "Let " ++ varName ++ " = " ++ exprToString expr

                Program.SendMessage _ recipientExpr message ->
                    "SendMessage(" ++ exprToString recipientExpr ++ ", \"" ++ message ++ "\")"

                Program.Receive _ patterns ->
                    let
                        patternToString : { message : String, body : List Program.Stmt } -> String
                        patternToString pattern =
                            "  \""
                                ++ pattern.message
                                ++ "\" ->\n"
                                ++ (pattern.body
                                        |> List.map (\innerStmt -> "    " ++ stmtToString innerStmt)
                                        |> String.join "\n"
                                   )

                        patternsText : String
                        patternsText =
                            patterns
                                |> List.map patternToString
                                |> String.join "\n"
                    in
                    "Receive:\n" ++ patternsText

                Program.ExprStmt _ expr ->
                    "ExprStmt(" ++ exprToString expr ++ ")"

                Program.End _ ->
                    "End"

                Program.Crash _ reason ->
                    "Crash: " ++ reason

        viewStmtWithRemainingWork : Program.Stmt -> Html msg
        viewStmtWithRemainingWork originalStmt =
            viewStmtWithRemainingWorkAndStyle originalStmt []

        viewStmtWithRemainingWorkAndStyle : Program.Stmt -> List (Html.Attribute msg) -> Html msg
        viewStmtWithRemainingWorkAndStyle originalStmt additionalStyle =
            case originalStmt of
                Program.Work stmtId label originalAmount ->
                    let
                        remainingAmount : Int
                        remainingAmount =
                            findRemainingWorkAmount stmtId
                                |> Maybe.withDefault originalAmount

                        workText : String
                        workText =
                            if remainingAmount == originalAmount then
                                "Work: " ++ label ++ " (" ++ String.fromInt originalAmount ++ " reductions)"

                            else
                                "Work: " ++ label ++ " (" ++ String.fromInt remainingAmount ++ "/" ++ String.fromInt originalAmount ++ " reductions)"

                        isCurrentLine : Bool
                        isCurrentLine =
                            currentStmtId == Just stmtId

                        lineStyle : List (Html.Attribute msg)
                        lineStyle =
                            if isCurrentLine then
                                [ style "background-color" "#ffffcc"
                                , style "font-weight" "bold"
                                , style "padding" "2px 4px"
                                , style "border-radius" "3px"
                                ]

                            else
                                [ style "padding" "2px 4px" ]
                    in
                    div (lineStyle ++ additionalStyle) [ text workText ]

                _ ->
                    case originalStmt of
                        Program.Receive _ patterns ->
                            viewReceiveStmt patterns (currentStmtId == Just (Program.getStmtId originalStmt))

                        _ ->
                            let
                                stmtId : Program.StmtId
                                stmtId =
                                    Program.getStmtId originalStmt

                                isCurrentLine : Bool
                                isCurrentLine =
                                    currentStmtId == Just stmtId

                                lineStyle : List (Html.Attribute msg)
                                lineStyle =
                                    if isCurrentLine then
                                        [ style "background-color" "#ffffcc"
                                        , style "font-weight" "bold"
                                        , style "padding" "2px 4px"
                                        , style "border-radius" "3px"
                                        ]

                                    else
                                        [ style "padding" "2px 4px" ]
                            in
                            div (lineStyle ++ additionalStyle) [ text (stmtToString originalStmt) ]

        findRemainingWorkAmount : Program.StmtId -> Maybe Int
        findRemainingWorkAmount targetStmtId =
            Scheduler.getRemainingWork targetStmtId scheduler

        viewReceiveStmt : List { message : String, body : List Program.Stmt } -> Bool -> Html msg
        viewReceiveStmt patterns isCurrentLine =
            let
                receiveHeaderStyle : List (Html.Attribute msg)
                receiveHeaderStyle =
                    if isCurrentLine then
                        [ style "background-color" "#ffffcc"
                        , style "font-weight" "bold"
                        , style "padding" "2px 4px"
                        , style "border-radius" "3px"
                        ]

                    else
                        [ style "padding" "2px 4px" ]

                viewPattern : { message : String, body : List Program.Stmt } -> List (Html msg)
                viewPattern pattern =
                    let
                        patternHeader : Html msg
                        patternHeader =
                            div [ style "padding" "2px 4px", style "margin-left" "16px" ]
                                [ text ("\"" ++ pattern.message ++ "\" ->") ]

                        patternBody : List (Html msg)
                        patternBody =
                            pattern.body
                                |> List.map
                                    (\innerStmt ->
                                        let
                                            innerStmtId : Program.StmtId
                                            innerStmtId =
                                                Program.getStmtId innerStmt

                                            isInnerCurrent : Bool
                                            isInnerCurrent =
                                                currentStmtId == Just innerStmtId

                                            innerStyle : List (Html.Attribute msg)
                                            innerStyle =
                                                if isInnerCurrent then
                                                    [ style "background-color" "#ffffcc"
                                                    , style "font-weight" "bold"
                                                    , style "padding" "2px 4px"
                                                    , style "border-radius" "3px"
                                                    , style "margin-left" "32px"
                                                    ]

                                                else
                                                    [ style "padding" "2px 4px"
                                                    , style "margin-left" "32px"
                                                    ]
                                        in
                                        viewStmtWithRemainingWorkAndStyle innerStmt innerStyle
                                    )
                    in
                    patternHeader :: patternBody
            in
            div []
                (div receiveHeaderStyle [ text "Receive:" ]
                    :: (patterns |> List.concatMap viewPattern)
                )

        programLines : List (Html msg)
        programLines =
            if List.isEmpty originalProgram then
                [ div [ style "padding" "2px 4px" ] [ text "Empty Program" ] ]

            else
                List.map viewStmtWithRemainingWork originalProgram
    in
    div
        [ style "color" "#333"
        , style "font-size" "0.9em"
        , style "white-space" "pre"
        ]
        programLines


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
