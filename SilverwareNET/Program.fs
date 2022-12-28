namespace SilverwareNET

module Interpreter =
    open AST

    let rec eval env (expr: Expression) =
        match expr with
        | ELiteral value -> Value.VLiteral value |> Some
        | EVariable label -> Map.tryFind label env |> Option.bind (eval env)
        | EAbstraction(_, label, body) -> Value.VClosure(label, body, env) |> Some
        | EApplication(fn, arg) ->
            eval env fn
            |> function
                | Some(Value.VClosure(label, body, closedEnvironment)) ->
                    let newEnv = Map.add label arg closedEnvironment in eval newEnv body
                | Some _ -> None
                | None -> None
        | ECondition(test, ``then``, ``else``) ->
            eval env test
            |> Option.bind (function
                | Value.VLiteral(LBoolean true) -> eval env ``then``
                | Value.VLiteral(LBoolean false) -> eval env ``else``
                | _ -> None)

module Main =
    open AST

    [<EntryPointAttribute>]
    let main _ =
        Emit.Compile "test" [ EAbstraction ("main", "args", (EApplication (EVariable "print", ELiteral(LString "Hello World!")))) ]
        |> function Ok _ -> printfn "Compiled!"
                  | Error reason -> failwith $"{reason}"
        0
