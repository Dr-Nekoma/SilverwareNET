namespace SoleilML

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
        | EAbstraction of Label: string * Body: Expression
        | EApplication of Function: Expression * Argument: Expression
        | ECondition of Test: Expression * Then: Expression * Else: Expression
        | 

    [<RequireQualifiedAccess>]
    type Value =
        | VUnit
        | VLiteral of Literal
        | VClosure of Label: string * Body: Expression * ClosedEnvironment: Map<string, Expression>
