module Demo5 exposing (main)

import Browser
import Browser.Dom
import Html exposing (Html)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Scheduler exposing (Scheduler)
import Shared


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
    | HasScrolledToBottomOfTrace (Result Browser.Dom.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    initWithBudget 1


initWithBudget : Int -> ( Model, Cmd Msg )
initWithBudget budget =
    let
        initialScheduler : Scheduler
        initialScheduler =
            Scheduler.init
                { workType = Scheduler.ReductionsBudget budget
                , program = Scheduler.ex5
                }
    in
    ( { history = Zipper.singleton initialScheduler
      , budget = String.fromInt budget
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
            model.budget
                |> String.toInt
                |> Maybe.withDefault 1
                |> initWithBudget


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
    Shared.viewDemoLayout
        { title = "Demo 5: Sending messages"
        , stepForward = StepForward
        , stepBackward = StepBackward
        , reset = Reset
        , history = model.history
        , schedulerMode = Shared.ProcessTableWithMailbox
        , codeExample = Scheduler.code5
        , additionalControls = []
        , budgetControls =
            Just
                { resetWithBudget = ResetWithBudget
                , updateBudget = UpdateBudget
                , budgetField = model.budget
                }
        }


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
