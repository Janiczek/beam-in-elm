module Program exposing (CrashReason, Expr(..), Message, Program, Stmt(..), example)


type alias Message =
    String


type alias CrashReason =
    String


type alias Program =
    List Stmt


type Stmt
    = Work String Int
    | Let String Expr
    | SendMessage Expr Message
    | Receive (List { message : String, body : Program })
    | ExprStmt Expr
    | End
    | Crash CrashReason


type Expr
    = GetSelfPid
    | Spawn Program
    | Var String



-- EXAMPLES


example : Program
example =
    [ Work "ex before spawn" 2
    , Let "pid" GetSelfPid
    , ExprStmt (Spawn (childProgram (Var "pid")))
    , Work "ex after spawn" 10
    , Receive
        [ { message = "done"
          , body =
                [ Work "ex after receive" 2
                , End
                ]
          }
        ]
    ]


childProgram : Expr -> Program
childProgram parentPidExpr =
    [ Work "child" 20
    , SendMessage parentPidExpr "done"
    , End
    ]
