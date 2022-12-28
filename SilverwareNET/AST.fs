namespace SilverwareNET

module Prelude =
    let GetEnvironment () =
        Map.empty
        |> Map.add "print" "call void [mscorlib]System.Console::WriteLine(class System.Object)"
        |> Map.add "+" "add"
        |> Map.add "-" "sub"
        |> Map.add "/" "div"
        |> Map.add "*" "mul"

module AST =

    [<RequireQualifiedAccess>]
    type Type =
        | TUnit
        | TInteger
        | TRational
        | TBool
        | TString
        | TArrow of Type * Type

    type Literal =
        | LUnit
        | LInteger of int
        | LReal of float
        | LBoolean of bool
        | LString of string

    type Expression =
        | ELiteral of Value: Literal
        | EVariable of Label: string
        | EAbstraction of Name: string * Label: string * Body: Expression
        | EApplication of Function: Expression * Argument: Expression
        | ECondition of Test: Expression * Then: Expression * Else: Expression

    [<RequireQualifiedAccess>]
    type Value =
        | VUnit
        | VLiteral of Literal
        | VClosure of Label: string * Body: Expression * ClosedEnvironment: Map<string, Expression>
