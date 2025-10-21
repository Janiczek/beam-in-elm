module Demo3 exposing (main)

import Browser
import Browser.Dom
import Html exposing (Html)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Scheduler exposing (Scheduler)
import Shared


type alias Model =
    { history : Zipper Scheduler
    }


type Msg
    = StepForward
    | StepBackward
    | Reset
    | HasScrolledToBottomOfTrace (Result Browser.Dom.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    ( { history =
            Scheduler.init
                { workType = Scheduler.AllAtOnce
                , program = Scheduler.ex3
                }
                |> Zipper.singleton
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
        , codeExample = Scheduler.code3
        , additionalControls = []
        , additionalInfo = []
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
