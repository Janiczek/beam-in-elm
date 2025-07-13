module Proc exposing (Proc, State(..), addToMailbox, init, setMailbox, setProgram, setState)

import Program exposing (CrashReason, Message, Program)


type alias Proc =
    { mailbox : List Message
    , state : State
    , program : Program
    }


{-| There could also be "Running", but we can derive that from being at the head of the ready queue.
This would perhaps be more important to track if we allowed parallelism.
-}
type State
    = ReadyToRun
    | WaitingForMsg
    | EndedNormally
    | Crashed CrashReason


init : Program -> Proc
init program =
    { mailbox = []
    , state = ReadyToRun
    , program = program
    }


setState : State -> Proc -> Proc
setState state proc =
    { proc | state = state }


setProgram : Program -> Proc -> Proc
setProgram program proc =
    { proc | program = program }


addToMailbox : Message -> Proc -> Proc
addToMailbox message proc =
    { proc | mailbox = message :: proc.mailbox }


setMailbox : List Message -> Proc -> Proc
setMailbox mailbox proc =
    { proc | mailbox = mailbox }
