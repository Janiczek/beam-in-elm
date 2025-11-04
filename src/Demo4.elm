module Demo4 exposing (main)

import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Scheduler exposing (Scheduler)
import Shared


type alias Model =
    { history : Zipper Scheduler
    , budget : String
    , workType : Scheduler.WorkType
    }


type Msg
    = StepForward
    | StepBackward
    | Reset
    | UpdateBudget String
    | ResetWithBudget Int
    | SwitchToAllAtOnce
    | SwitchToReductionsBudget
    | HasScrolledToBottomOfTrace (Result Browser.Dom.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    let
        workType : Scheduler.WorkType
        workType =
            Scheduler.AllAtOnce
    in
    ( { history =
            Scheduler.init
                { workType = workType
                , program = Scheduler.ex4
                }
                |> Zipper.singleton
      , budget = String.fromInt 8
      , workType = workType
      }
    , Cmd.none
    )


initWithBudget : Int -> ( Model, Cmd Msg )
initWithBudget budget =
    let
        workType : Scheduler.WorkType
        workType =
            Scheduler.ReductionsBudget budget
    in
    ( { history =
            Scheduler.init
                { workType = workType
                , program = Scheduler.ex4
                }
                |> Zipper.singleton
      , budget = String.fromInt budget
      , workType = workType
      }
    , Cmd.none
    )


resetWithAllAtOnce : Model -> ( Model, Cmd Msg )
resetWithAllAtOnce model =
    let
        workType : Scheduler.WorkType
        workType =
            Scheduler.AllAtOnce
    in
    ( { model
        | history =
            Scheduler.init
                { workType = workType
                , program = Scheduler.ex4
                }
                |> Zipper.singleton
        , workType = workType
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StepForward ->
            ( Shared.handleStepForward model
            , Shared.jumpToBottomOfTraces
                |> Cmd.map HasScrolledToBottomOfTrace
            )

        StepBackward ->
            ( Shared.handleStepBackward model
            , Shared.jumpToBottomOfTraces
                |> Cmd.map HasScrolledToBottomOfTrace
            )

        Reset ->
            init ()

        UpdateBudget budgetStr ->
            ( { model | budget = budgetStr }, Cmd.none )

        ResetWithBudget budgetInt ->
            initWithBudget budgetInt

        SwitchToAllAtOnce ->
            model
                |> resetWithAllAtOnce

        SwitchToReductionsBudget ->
            model.budget
                |> String.toInt
                |> Maybe.withDefault 1
                |> initWithBudget

        HasScrolledToBottomOfTrace _ ->
            ( model
                |> Shared.handleHasScrolledToBottomOfTrace
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        workTypeButtons : List (Html Msg)
        workTypeButtons =
            [ Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "gap" "5px"
                , Html.Attributes.style "align-items" "center"
                ]
                [ Html.label [] [ Html.text "Work Type:" ]
                , Html.button
                    [ Html.Events.onClick SwitchToAllAtOnce
                    , Html.Attributes.style "padding" "8px 16px"
                    , Html.Attributes.style "font-family" "'JetBrains Mono', monospace"
                    , Html.Attributes.style "background-color"
                        (case model.workType of
                            Scheduler.AllAtOnce ->
                                "greenyellow"

                            _ ->
                                ""
                        )
                    ]
                    [ Html.text "All At Once" ]
                , Html.button
                    [ Html.Events.onClick SwitchToReductionsBudget
                    , Html.Attributes.style "padding" "8px 16px"
                    , Html.Attributes.style "font-family" "'JetBrains Mono', monospace"
                    , Html.Attributes.style "background-color"
                        (case model.workType of
                            Scheduler.ReductionsBudget _ ->
                                "greenyellow"

                            _ ->
                                ""
                        )
                    ]
                    [ Html.text "Reductions Budget" ]
                ]
            ]
    in
    Shared.viewDemoLayout
        { title = "Demo 4: Reduction Budget"
        , stepForward = StepForward
        , stepBackward = StepBackward
        , reset = Reset
        , history = model.history
        , schedulerMode = Shared.ProcessTable
        , codeExample = Scheduler.code4
        , additionalControls = workTypeButtons
        , budgetControls =
            case model.workType of
                Scheduler.ReductionsBudget _ ->
                    Just
                        { resetWithBudget = ResetWithBudget
                        , updateBudget = UpdateBudget
                        , budgetField = model.budget
                        }

                Scheduler.AllAtOnce ->
                    Nothing
        }


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
