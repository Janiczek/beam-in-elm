module Demo7 exposing (main)

import Browser
import Browser.Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Scheduler exposing (Scheduler, Step(..), WorkType(..), code7, code7b)
import Shared exposing (ProcessState(..), SchedulerViewMode(..))


type alias Model =
    { history : Zipper Scheduler
    , budget : String
    , isFixedVersion : Bool
    }


type Msg
    = StepForward
    | StepBackward
    | Reset
    | FixBug
    | UpdateBudget String
    | ResetWithBudget Int
    | HasScrolledToBottomOfTrace (Result Browser.Dom.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    initWithBudget 1


initWithBudget : Int -> ( Model, Cmd Msg )
initWithBudget budget =
    initWithBudgetAndProgram budget { fixed = False }


initWithBudgetAndProgram : Int -> { fixed : Bool } -> ( Model, Cmd Msg )
initWithBudgetAndProgram budget { fixed } =
    let
        initialScheduler : Scheduler
        initialScheduler =
            Scheduler.init
                { workType = ReductionsBudget budget
                , program =
                    if fixed then
                        Scheduler.ex7b

                    else
                        Scheduler.ex7
                }
    in
    ( { history = Zipper.singleton initialScheduler
      , budget = String.fromInt budget
      , isFixedVersion = fixed
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StepForward ->
            let
                updatedModel =
                    Shared.handleStepForward model
            in
            ( updatedModel, Shared.jumpToBottomOfTraces |> Cmd.map HasScrolledToBottomOfTrace )

        StepBackward ->
            let
                updatedModel =
                    Shared.handleStepBackward model
            in
            ( updatedModel, Shared.jumpToBottomOfTraces |> Cmd.map HasScrolledToBottomOfTrace )

        Reset ->
            model.budget
                |> String.toInt
                |> Maybe.withDefault 1
                |> initWithBudget

        FixBug ->
            let
                budget : Int
                budget =
                    model.budget
                        |> String.toInt
                        |> Maybe.withDefault 1
            in
            initWithBudgetAndProgram budget { fixed = True }

        UpdateBudget budgetStr ->
            ( { model | budget = budgetStr }, Cmd.none )

        ResetWithBudget budgetInt ->
            initWithBudget budgetInt

        HasScrolledToBottomOfTrace _ ->
            ( model
                |> Shared.handleHasScrolledToBottomOfTrace
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        currentScheduler : Scheduler
        currentScheduler =
            Zipper.current model.history

        additionalControls : List (Html Msg)
        additionalControls =
            [ button
                [ onClick FixBug
                , style "padding" "8px 16px"
                ]
                [ text "Fix the bug" ]
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

        additionalInfo : List (Html Msg)
        additionalInfo =
            [ div
                [ style "color" "#666" ]
                [ text (Shared.workTypeToString currentScheduler.workType) ]
            ]
    in
    Shared.viewDemoLayout
        { title = "Demo 7: Link, Crash and a surprise"
        , stepForward = StepForward
        , stepBackward = StepBackward
        , reset = Reset
        , history = model.history
        , schedulerMode = Shared.ProcessTableWithMailbox
        , codeExample = if model.isFixedVersion then code7b else code7
        , additionalControls = additionalControls
        , additionalInfo = additionalInfo
        }


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
