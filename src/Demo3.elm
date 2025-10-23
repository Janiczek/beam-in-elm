module Demo3 exposing (main)

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
    , isHintingAtProblem : Bool
    }


type Msg
    = StepForward
    | StepBackward
    | Reset
    | HintAtProblem
    | HasScrolledToBottomOfTrace (Result Browser.Dom.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    initWithProgram { hint = False }


initWithProgram : { hint : Bool } -> ( Model, Cmd Msg )
initWithProgram { hint } =
    ( { history =
            Scheduler.init
                { workType = Scheduler.AllAtOnce
                , program =
                    if hint then
                        Scheduler.ex4

                    else
                        Scheduler.ex3
                }
                |> Zipper.singleton
      , isHintingAtProblem = hint
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

        HintAtProblem ->
            initWithProgram { hint = True }

        Reset ->
            init ()

        HasScrolledToBottomOfTrace _ ->
            ( model
                |> Shared.handleHasScrolledToBottomOfTrace
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Shared.viewDemoLayout
        { title = "Demo 3: Spawn"
        , stepForward = StepForward
        , stepBackward = StepBackward
        , reset = Reset
        , history = model.history
        , schedulerMode = Shared.ProcessTable
        , codeExample =
            if model.isHintingAtProblem then
                Scheduler.code4

            else
                Scheduler.code3
        , additionalControls =
            [ Html.button
                [ Html.Events.onClick HintAtProblem
                , Html.Attributes.style "padding" "8px 16px"
                ]
                [ Html.text "Hint at problem" ]
            ]
        , budgetControls = Nothing
        }


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
