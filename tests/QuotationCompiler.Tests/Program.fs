
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open QuotationCompiler


// 1. hello world
let a = obj()
let b = obj()
let hello() = 
    let ea = Expr.Value(a) |> Expr.Cast
    let eb = Expr.Value(b) |> Expr.Cast

    <@ fun () -> %ea, %eb @> |> QuotationCompiler.ToObject |> unbox<unit -> obj * obj>





[<EntryPoint>]
let main args =
    let h = hello()

    let ta, tb = h()
    printfn "%A" (System.Object.ReferenceEquals(ta, a))
    printfn "%A" (System.Object.ReferenceEquals(tb, b))

    0