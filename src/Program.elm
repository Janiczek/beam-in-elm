module Program exposing (CrashReason, Expr(..), Message, Program, Stmt(..), StmtId, example, getStmtId)


type alias Message =
    String


type alias CrashReason =
    String


type alias Program =
    List Stmt


type alias StmtId =
    Int


type Stmt
    = Work StmtId String Int
    | Let StmtId String Expr
    | SendMessage StmtId Expr Message
    | Receive StmtId (List { message : String, body : Program })
    | ExprStmt StmtId Expr
    | End StmtId
    | Crash StmtId CrashReason


type Expr
    = GetSelfPid
    | Spawn Program
    | Var String


getStmtId : Stmt -> StmtId
getStmtId stmt =
    case stmt of
        Work id _ _ ->
            id

        Let id _ _ ->
            id

        SendMessage id _ _ ->
            id

        Receive id _ ->
            id

        ExprStmt id _ ->
            id

        End id ->
            id

        Crash id _ ->
            id



-- EXAMPLES


example : Program
example =
    [ Work 1 "ex before spawn" 2
    , Let 2 "pid" GetSelfPid
    , ExprStmt 3 (Spawn (childProgram (Var "pid")))
    , Work 4 "ex after spawn" 10
    , Receive 5
        [ { message = "done"
          , body =
                [ Work 6 "ex after receive" 2
                , End 7
                ]
          }
        ]
    ]


childProgram : Expr -> Program
childProgram parentPidExpr =
    [ Work 8 "child" 20
    , SendMessage 9 parentPidExpr "done"
    , End 10
    ]
