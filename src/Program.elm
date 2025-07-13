module Program exposing (Message, Program(..), example, hasEnded)

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
    | Receive KM
      -- TODO crash/exit/raise
    | Spawn Program KP
    | End


hasEnded : Program -> Bool
hasEnded program =
    case program of
        End ->
            True

        Work _ _ _ ->
            False

        GetSelfPid _ ->
            False

        SendMessage _ _ _ ->
            False

        Receive _ ->
            False

        Spawn _ _ ->
            False



-- EXAMPLES


example : Program
example =
    Work "ex before spawn" 2 <| \() ->
    GetSelfPid <| \pid ->
    Spawn (childProgram pid) <| \childPid ->
    Work ("ex after spawn PID " ++ String.fromInt childPid) 10 <| \() ->
    Receive <| \msg ->
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
