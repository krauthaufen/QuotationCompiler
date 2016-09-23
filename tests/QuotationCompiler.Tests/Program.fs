
open System
open System.Reflection
open System.Diagnostics
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open QuotationCompiler
open Microsoft.FSharp.Compiler.SourceCodeServices

[<ReflectedDefinition>]
type ThingWithPrivateField() =
    let mutable a = 10

    static member Sepp(a : seq<'a>, b : int) =
        printfn "%A" a

    static member Seppy(a : ref<'a>, b : int) =
        printfn "%A" a

    member x.Test(b : int) =
        let res = a + b
        a <- a + 1
        res

let createType() =

    let rec meth (name : string) (e : Expr) =
        match e with    
            | Lambda(v,b) -> 
                let _,args,b = meth name b
                name,v::args,b
            | _ ->
                name,[],e

    let mk (e0) =
        match e0 with   
            | Microsoft.FSharp.Quotations.DerivedPatterns.Lambdas(args, b) ->
                let rec mk (args : list<Var>) (e : Expr) =
                    match args with
                        | [] -> e
                        | h :: r -> Expr.Lambda(h, mk r e)

                mk (List.concat args) b
            | _ -> e0
    let e0 = typeof<ThingWithPrivateField>.GetMethod("Sepp") |> Expr.TryGetReflectedDefinition |> Option.get |> mk
    let e1 = typeof<ThingWithPrivateField>.GetMethod("Seppy") |> Expr.TryGetReflectedDefinition |> Option.get |> mk


    let myObj = obj()
    let target, methods = 
        QuotationCompiler.CreateInstance [
            meth "Bla" e0
            meth "Bla" e1
        ]

    let mi = methods.[0].MakeGenericMethod [| typeof<int> |]
    let res = mi.Invoke(target, [| [1;2;3] :> obj; 1 :> obj|])
    printfn "%A" (res = myObj)

[<EntryPoint>]
let main args =
    createType()
    Environment.Exit 0


    

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


    0