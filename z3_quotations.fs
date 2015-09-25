module Z3_quotations
open Microsoft.Z3 
open System

open Microsoft.FSharp.Quotations

let rec bodyToExpr (ctx:Context) (vars:Map<string, ArithExpr>) = function
    | Patterns.Var(v) -> vars.[v.Name]
    | Patterns.Value(v, t) when t = typeof<int> -> 
        ctx.MkInt(v :?> int) :> ArithExpr
    | DerivedPatterns.SpecificCall <@ (*) @> (None, _, [l; r]) -> 
        let le = bodyToExpr ctx vars l
        let re = bodyToExpr ctx vars r
        ctx.MkMul [| le; re |]
    | DerivedPatterns.SpecificCall <@ (+) @> (None, _, [l; r]) -> 
        let le = bodyToExpr ctx vars l
        let re = bodyToExpr ctx vars r
        ctx.MkAdd [| le; re |]
    | e -> failwithf "Unexpected: %A" e

let funcToExpr (ctx:Context) = function 
    | DerivedPatterns.Lambdas
        (vars, DerivedPatterns.SpecificCall <@ (=) @> (None, _, [l; Patterns.Value(v, ty)])) 
        when ty = typeof<int> ->
        let vars = 
            List.concat vars 
            |> List.map (fun v -> v.Name, ctx.MkIntConst(v.Name) :> ArithExpr)
        List.map snd vars |> Array.ofList,
        ctx.MkInt(v :?> int),
        bodyToExpr ctx (Map.ofList vars) l
    | _ -> failwith "Expected function"


