module Program exposing (Program(..), Trace(..))

-- TODO: test at function entry: if (reductionBudget < 0) { yield }

import PID exposing (PID)


type alias Message =
    String


type alias K =
    () -> Program


type alias KM =
    Message -> Maybe Program


type alias KP =
    PID -> Program


type Program
    = Work String Int K
    | GetSelfPid KP
    | SendMessage PID Message K
    | Receive { timeout : Maybe Int } KM
      -- TODO crash/exit/raise
    | Spawn Program KP
    | End


type Trace
    = DidWork String Int
    | DidGetSelfPid Int
    | DidSendMessage PID Message
    | DidReceiveWanted String
    | DidReceiveUnwanted String
    | DidntReceive
    | DidSpawn PID
    | DidEnd



-- EXAMPLES


example : Program
example =
    Work "ex before spawn" 2 <| \() ->
    GetSelfPid <| \pid ->
    Spawn (childProgram pid) <| \childPid ->
    Work "ex after spawn" 10 <| \() ->
    Receive { timeout = Nothing } <| \msg ->
    case msg of
        "done" ->
            Just <|
                Work "ex after receive" 2 <| \() ->
                End

        _ ->
            Nothing


childProgram : PID -> Program
childProgram parentPid =
    Work "child" 20 <| \() ->
    SendMessage parentPid "done" <| \() ->
    End



-- STEP


step : Int -> Program -> ( Program, List Trace )
step reductionBudget program =
    step_ reductionBudget [] program


step_ : Int -> PID -> List Trace -> Program -> ( Program, List Trace )
step_ reductionBudget pid trace program =
    if reductionBudget <= 0 then
        ( program, trace )

    else
        case program of
            Work label amount k ->
                case compare amount reductionBudget of
                    LT ->
                        -- do this work _and then some_
                        Debug.todo "step Work LT"

                    EQ ->
                        -- finish doing this work then yield
                        Debug.todo "step Work EQ"

                    GT ->
                        -- do only a part of this work
                        Debug.todo "step Work GT"

            GetSelfPid kp ->
                step_ (reductionBudget - 1) pid (DidGetSelfPid pid :: trace) (kp pid)

            SendMessage recipientPid message k ->
                Debug.todo "step SendMessage"

            Receive { timeout } km ->
                Debug.todo "step Receive"

            Spawn childProgram kp ->
                Debug.todo "step Spawn"

            End ->
                ( program, DidEnd :: trace )
