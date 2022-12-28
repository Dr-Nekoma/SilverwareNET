namespace SilverwareNET

module Emit =
    open AST

    let assemblyDefinition = ".assembly extern mscorlib {}"
    let assemblyName = sprintf ".assembly %s {}"
    
    let buildMethod name returnType argType body =
        sprintf ".method static public %s %s(%s) cil managed {%s\nret\n}" returnType name argType body
        
    let ConvertLiteral (literal: Literal) =
        match literal with
        | LString value -> $"ldstr \"{value}\"\n"
        | LInteger value -> $"ldc.i4 {value}\n"
        | LBoolean value -> if value then "ldc.i4.1\n" else "ldc.i4.0\n"
        | LUnit -> ""
        | LReal value -> $"ldc.r4 {value}\n"
        
    let ConvertVariable (variable: string) = //(args: string list) =
        let environment = Prelude.GetEnvironment() in
        Map.tryFind variable environment
        |> function Some x -> x
                  | None -> failwith $"Variable '{variable}' not in context."
        
    let rec Generate (expr: Expression list) acc : string list =
        printfn "%A" expr
        let stackSize = 10000
        match expr with
        | (ELiteral literal)::rest ->
            let newAcc =
                List.append acc [(ConvertLiteral literal)] in Generate rest newAcc
        | (EVariable value)::rest ->
            let newAcc =
                List.append acc [(ConvertVariable value)] in Generate rest newAcc
        | EAbstraction("main", _, body)::rest ->
            match body with
            | ELiteral _
            | EVariable _
            | EApplication _ ->
                // For now it considers that only MAIN is valid
                let entryPoint = buildMethod "main" "void" "" in
                let metadataForEntryPoint = $"\n.entrypoint \n.maxstack {stackSize}\n" in
                let emittedBody =
                    Generate [body] []
                    |> List.fold (+) "" in
                [entryPoint (metadataForEntryPoint + "\n" + emittedBody)]
            | EAbstraction _ -> failwith "Functions inside functions not available yet."
            | ECondition _ -> failwith "TODO"
        | EApplication (fn, arg)::rest ->
            let emittedArg = Generate [arg] acc in
            let emittedFunc = Generate [fn] emittedArg
            Generate rest emittedFunc
        | ECondition _::rest -> failwith "TODO"
        | x when List.length x = 1 -> Generate x acc
        | [] -> acc
        | _ -> failwith "TODO"
        
    let Compile moduleName program =
        try
            Generate program []
            |> List.fold (+) ""
            |> fun content ->
                let content = assemblyDefinition + assemblyName moduleName + content
                System.IO.File.WriteAllText(moduleName + ".il", content)
            |> Ok
        with ex -> Error ex.Message
        