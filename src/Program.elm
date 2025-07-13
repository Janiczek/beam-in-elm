module Program exposing (CrashReason, Message, Program(..), example)

import PID exposing (PID)


type alias Message =
    String


type alias CrashReason =
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
    | Spawn Program KP
    | End
    | Crash String



-- EXAMPLES


example : Program
example =
    Work "ex before spawn" 2 <|
        \() ->
            GetSelfPid <|
                \pid ->
                    Spawn (childProgram pid) <|
                        \childPid ->
                            Work ("ex after spawn PID " ++ String.fromInt childPid) 10 <|
                                \() ->
                                    Receive <|
                                        \msg ->
                                            case msg of
                                                "done" ->
                                                    Just <|
                                                        Work "ex after receive" 2 <|
                                                            \() ->
                                                                End

                                                _ ->
                                                    Nothing


childProgram : PID -> Program
childProgram parentPid =
    Work "child" 20 <|
        \() ->
            SendMessage parentPid "done" <|
                \() ->
                    End
