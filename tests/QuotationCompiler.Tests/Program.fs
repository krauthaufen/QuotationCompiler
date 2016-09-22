
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open QuotationCompiler


[<ReflectedDefinition>]
type ThingWithPrivateField() =
    let mutable a = 10
    member x.Test(b : int) =
        let res = a + b
        a <- a + 1
        res

// 1. hello world
let a = obj()
let b = obj()
let hello() = 
    let ea : Expr<obj> = Expr.Value(a) |> Expr.Cast
    let eb : Expr<obj> = Expr.Value(b) |> Expr.Cast

    <@ %ea, %eb @> |> QuotationCompiler.Eval //|> unbox<unit -> obj * obj>



open System
open System.Reflection
open System.Diagnostics

type Helpers =
    
    static member FieldGet<'a, 'b>(f : FieldInfo, instance : 'a) =
        f.GetValue(instance) |> unbox<'b>


[<EntryPoint>]
let main args =

    let fi = typeof<ThingWithPrivateField>.GetField("a", BindingFlags.NonPublic ||| BindingFlags.Instance)

    let iter = 10000000
    let a = ThingWithPrivateField()
    let sw = Stopwatch()
    sw.Start()
    for i in 1..iter do
        fi.GetValue(a) |> ignore
        //Helpers.FieldGet<ThingWithPrivateField, int>(fi, a) |> ignore
    sw.Stop()

    printfn "get took %.3fns" (sw.Elapsed.TotalMilliseconds * 1000000.0 / float iter)


    let thing = ThingWithPrivateField()
    let mi = typeof<ThingWithPrivateField>.GetMethod "Test"
    match Expr.TryGetReflectedDefinition mi with
        | Some e ->
            let e : Expr<ThingWithPrivateField -> int -> int> = Expr.Cast e
            let f = QuotationCompiler.Eval e
            let res = f thing 5
            printfn "%A = 15" res
            let res = f thing 5
            printfn "%A = 16" res
        | None ->
            ()

//    let ta, tb = hello()
//
//    //let ta, tb = h()
//    printfn "%A" (System.Object.ReferenceEquals(ta, a))
//    printfn "%A" (System.Object.ReferenceEquals(tb, b))

    0