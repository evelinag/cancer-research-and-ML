// Download Z3 binaries from
// https://github.com/Z3Prover

#I @"z3-4.4.0-x64-win/bin/"
#r "Microsoft.Z3.dll"

open Microsoft.Z3 
open System

let ctx = new Context()

let note1 = ctx.MkInt(1)
let note5 = ctx.MkInt(5)

let m1 = ctx.MkIntConst("m1")
let m5 = ctx.MkIntConst("m5")

let mult1 = ctx.MkMul([|note1:>ArithExpr; m1:>_|])
let mult5 = ctx.MkMul([|note5:>ArithExpr; m5:>_|])
let sum15 = ctx.MkAdd([|mult1; mult5|])
let totalNotes = ctx.MkAdd([|m1 :>ArithExpr; m5 :>_ |])

let value = 42
let nNotes = 10

let solver = ctx.MkSolver()

solver.Add( [| ctx.MkEq(ctx.MkInt(value), sum15);
               ctx.MkEq(ctx.MkInt(nNotes), totalNotes) |])

let result = solver.Check([||])
match result with
| Status.UNSATISFIABLE -> printfn "Unsatisfiable"
| Status.SATISFIABLE -> 
    printfn "Satisfiable"                
    let model = solver.Model
    printfn "m1 = %O, m5 = %O" (model.Evaluate(m1)) (model.Evaluate(m5))
| Status.UNKNOWN -> printfn "unknown"
| _ -> printfn "Other"

solver.Reset()



#load "z3_quotations.fs"
open Z3_quotations

let context = new Context()
let expression = <@ fun a1 a2 -> a1 * 5 + a2 * 1 = 2042 @>

let rec findNotes expression (k:int) =
    let vars, sum, expr = funcToExpr context expression
    let totalCoins = context.MkAdd(vars)

    let solver = context.MkSolver()
    solver.Add( [| context.MkEq(sum, expr);
                   context.MkEq(totalCoins, context.MkInt(k)) |])
    solver.Add( vars |> Array.map (fun v -> context.MkGe(v, context.MkInt(0))))
    let result = solver.Check([||])
    match result with
    | Status.UNSATISFIABLE -> 
        printfn "Unsatisfiable for k = %d" k
        findNotes expression (k+1)
    | Status.SATISFIABLE -> 
        printfn "Satisfiable for k = %d" k                
        let model = solver.Model
        for v in vars do
            printfn "%s = %O" (v.SExpr()) (model.Evaluate(v))
        
    | Status.UNKNOWN -> printfn "unknown"
    | _ -> printfn "Other"

#time
findNotes expression 1

