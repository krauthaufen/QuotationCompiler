
open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open QuotationCompiler
open Microsoft.FSharp.Compiler.SourceCodeServices

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

    <@ fun (a : int) (b : int) -> %ea, %eb @> |> QuotationCompiler.Eval //|> unbox<unit -> obj * obj>



open System
open System.Reflection
open System.Diagnostics

type Helpers =
    
    static member FieldGet<'a, 'b>(f : FieldInfo, instance : 'a) =
        f.GetValue(instance) |> unbox<'b>

let checker = FSharpChecker.Create()
/// Get untyped tree for a specified input
let getUntypedTree file input = 
  // Get compiler options for the 'project' implied by a single script file
  let projOptions = 
      checker.GetProjectOptionsFromScript(file, input)
      |> Async.RunSynchronously

  // Run the first phase (untyped parsing) of the compiler
  let parseFileResults = 
      checker.ParseFileInProject(file, input, projOptions) 
      |> Async.RunSynchronously

  match parseFileResults.ParseTree with
  | Some tree -> tree
  | None -> failwith "Something went wrong during parsing!"

let createType() =

    let rec meth (name : string) (e : Expr) =
        match e with    
            | Lambda(v,b) -> 
                let _,args,b = meth name b
                name,v::args,b
            | _ ->
                name,[],e

    let myObj = obj()
    let target, methods = 
        QuotationCompiler.CreateInstance [
            meth "Bla" <@ fun (a : int) -> 
                myObj 
            @>

            meth "Blubb" <@ fun (b : int) -> 
                b * b 
            @>
        ]

    let mi = methods.[0]
    let res = mi.Invoke(target, [|1 :> obj|])
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

    let test = hello()

    let ta, tb = test 1 2



    //let ta, tb = h()
    printfn "%A" (System.Object.ReferenceEquals(ta, a))
    printfn "%A" (System.Object.ReferenceEquals(tb, b))

    0