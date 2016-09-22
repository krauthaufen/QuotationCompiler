﻿module QuotationCompiler.Tests.TestModule

open NUnit.Framework
open FsUnit

open Microsoft.FSharp.Quotations

open QuotationCompiler

let compile (e : Expr<'T>) = QuotationCompiler.ToFunc e
let compileRun (e : Expr<'T>) = QuotationCompiler.Eval e

[<Test>]
let ``1. Constant leaf`` () =
    let testLeaf value = compileRun <@ value @> |> should equal value

    testLeaf true
    testLeaf 78uy
    testLeaf 99y
    testLeaf 'c'
    testLeaf 1.1231M
    testLeaf 1970s
    testLeaf 1970
    testLeaf 1231231L
    testLeaf 1970us
    testLeaf 1970u
    testLeaf 1231231uL
    testLeaf 1231n
    testLeaf 1231un
    testLeaf 3.1415926
    testLeaf "lorem ipsum"
    testLeaf ()
    testLeaf [|1uy..100uy|]
    testLeaf [|1us..100us|]

[<Test>]
let ``1. List leaf`` () =
    compileRun <@ [] : int list @> |> should equal List.empty<int>
    compileRun <@ [1;2;3] @> |> should equal [1;2;3]
    compileRun <@ 1 :: 2 :: 3 :: [] @> |> should equal [1;2;3]
    compileRun <@ [1 .. 10] @> |> should equal [1..10]

[<Test>]
let ``1. Array leaf`` () =
    compileRun <@ [||] : int [] @> |> should equal Array.empty<int>
    compileRun <@ [|1;2;3|] @> |> should equal [|1;2;3|]
//    compileRun <@ let x = [|0|] in x.[0] <- 42 ; x.[0] @> |> should equal 42
//    compileRun <@ [|1 .. 10|] @> |> should equal [|1..10|]

[<Test>]
let ``1. Multi-dimensional array`` () =
    compileRun <@ let x = Array3D.zeroCreate<int> 10 10 10 in x.[5,6,7] <- 42 ; x.[5,6,7] @> |> should equal 42

[<Test>]
let ``1. Null literal leaf`` () =
    compileRun <@ null : string @> |> should equal null

[<Test>]
let ``1. Union leaf`` () =
    compileRun <@ None : int option @> |> should equal Option<int>.None
    compileRun <@ Some (1,"lorem ipsum") @> |> should equal (Some (1,"lorem ipsum"))
    compileRun <@ Choice1Of2 "value" : Choice<string,exn> @> |> should equal (Choice<string,exn>.Choice1Of2 "value")
    compileRun <@ Choice1Of2 "value" : Choice<string,exn> @> |> should equal (Choice<string,exn>.Choice1Of2 "value")
    compileRun <@ A @> |> should equal A
    compileRun <@ B(42, "lorem ipsum") @> |> should equal (B(42, "lorem ipsum"))
    compileRun <@ GA (1,1) @> |> should equal (GA(1,1))

[<Test>]
let ``1. Record leaf`` () =
    compileRun <@ { contents = 42 } @> |> should equal (ref 42)
    compileRun <@ { Num = 42 ; Text = "text" ; Value = 3. } @> |> should equal { Num = 42 ; Text = "text" ; Value = 3. }
    compileRun <@ { GNum = 42 ; GText = "text" ; GValue = Some 3. } @> |> should equal { GNum = 42 ; GText = "text" ; GValue = Some 3. }

[<Test>]
let ``1. Tuple leaf`` () =
    compileRun <@ (1,"2") @> |> should equal (1,"2")
    compileRun <@ (1,"2", Some 42, [1..2]) @> |> should equal (1,"2", Some 42, [1..2])
    compileRun <@ (1,"2", Some 42, [1..2], 1, 2, 3, 4, 5, 6, 7, 9, (1,100)) @> |> ignore

[<Test>]
let ``2. Simple let binding`` () =
    compileRun <@ let x = 1 + 1 in x + x @> |> should equal 4

[<Test>]
let ``2. Simple mutable binding`` () =
    compileRun <@ let mutable x = 0 in x <- 4 ; x @> |> should equal 4

[<Test>]
let ``2. Simple use binding`` () =
    compileRun 
        <@ 
            let x = new DummyDisposable () 
            do use y = x in () 
            x.IsDisposed
        @> |> should equal true

[<Test>]
let ``2. Nested let binding`` () =
    compileRun 
        <@ 
            let x = let z = 1 in z + z
            let y = let z = 1. in z + z
            (x,y) 
        @> |> should equal (2,2.)

[<Test>]
let ``2. Recursive binding`` () =
    compileRun 
        <@ 
            let rec fib n =
                if n <= 1 then n
                else
                    fib(n-2) + fib(n-1)

            fib 10
        @> |> should equal 55

[<Test>]
let ``2. Mutual recursive binding`` () =
    let even,odd =
        compileRun
            <@
                let rec even n =
                    if n = 0 then true
                    else odd (n-1)

                and odd n =
                    if n = 0 then false
                    else even (n-1)

                even,odd
            @>

    for i in 0 .. 10 do
        even i |> should equal (i % 2 = 0)
        odd i |> should equal (i % 2 = 1)

[<Test>]
let ``2. Simple Lambda`` () =
    let f = compileRun <@ fun x -> x + 1 @>
    f 0 |> should equal 1
    let g = compileRun <@ fun x y -> x * y @>
    g 2 2 |> should equal 4

[<Test>]
let ``2. Simple Coercion`` () =
    compileRun <@ () :> obj :?> int option @> |> should equal None

[<Test>]
let ``2. Simple Sequential`` () =
    compileRun <@ let x = ref 0 in incr x ; !x @> |> should equal 1

[<Test>]
let ``2. Simple Type test`` () =
    compileRun <@ match box 42 with :? string as t -> t | :? int as i -> string i | _ -> null @> |> should equal "42"

[<Test>]
let ``2. Simple Object`` () =
    compileRun <@ let tc = new TestClass<int * string>((42, "test")) in tc.Value @> |> should equal (42, "test")
    compileRun <@ let tc = new TestClass<int>(0) in tc.Value <- 42 ; tc.Value @> |> should equal 42
    compileRun <@ let tc = new TestClass<int>(0) in tc.TestMethod 0 @> |> should equal (0,1)
    compileRun <@ let tc = TestClass<int>.GenericMethod<int>(1,1) in tc.Value @> |> should equal (1,1)

[<Test>]
let ``2. Higher-order function`` () =
    let twice = compileRun <@ let twice (f : int -> int) = f << f in twice @>
    twice (fun x -> x + x) 1 |> should equal 4

[<Test>]
let ``2. Simple if-then-else`` () =
    compileRun <@ if 101 % 2 = 1 then 25 + 17 else -1 @> |> should equal 42

[<Test>]
let ``2. Simple Dictionary`` () =
    compileRun <@ let d = new System.Collections.Generic.Dictionary<int, string> () in d.[42] <- "42" ; d.[42] @> |> should equal "42"

[<Test>]
let ``2. Simple while loop`` () =
    compileRun 
        <@
            let mutable x = 0
            while x < 10 do x <- x + 1
            x
        @>
    |> should equal 10

[<Test>]
let ``2. Simple numeric for loop`` () =
    compileRun 
        <@
            let x = ref 0
            for i = 1 to 10 do
                incr x
            !x
        @>
    |> should equal 10

[<Test>]
let ``2. Simple enumerating for loop`` () =
    compileRun 
        <@
            let x = ref 0
            for i in [1 .. 100] do
                incr x
            !x
        @>
    |> should equal 100

[<Test>]
let ``2. Simple try/with`` () =
    compileRun
        <@
            try 1 / 0
            with
            | :? System.DivideByZeroException -> -1
            | e -> -20
        @>
    |> should equal -1

[<Test>]
let ``2. Simple try/finally`` () =
    let tester =
        compileRun
            <@
                let test fail =
                    let mutable isFinalized = false
                    try
                        try if fail then failwith "kaboom"
                        finally
                            isFinalized <- true
                    with _ -> ()

                    isFinalized

                test
            @>

    tester true  |> should equal true
    tester false |> should equal true

[<Test>]
let ``2. Simple delegate`` () =
    let d = compileRun <@ let d = FSharpDelegate(fun x y -> sprintf "%d%s" x y) in d @>
    d.Invoke(42, "test") |> should equal "42test"

[<Test>]
let ``3. Tuple pattern match`` () =
    compileRun <@ match (1,"1") with 1,"2" -> 1 | (1,"1") -> 2 | _ -> 3 @> |> should equal 2
    compileRun <@ let (x,_) = (1,"") in x @> |> should equal 1
    compileRun <@ match "lorem ipsum", 42, "test" with _,_,"test1" -> true | _ -> false @> |> should equal false

[<Test>]
let ``3. List pattern match`` () =
    compileRun <@ match [42] with [] -> -1 | i :: [] -> i | _ -> -1 @> |> should equal 42
    compileRun <@ match [41] with [] -> -1 | [3] -> 3 | [x] -> x + 1 | _ -> -1 @> |> should equal 42
    compileRun <@ match [1;2;3] with [x;y;z] -> x + y + z | _ -> -1 @> |> should equal 6
    compileRun <@ match [Some 2; None; Some 40] with [Some i; None; Some j] -> i + j | _ -> -1 @> |> should equal 42

[<Test>]
let ``3. Record pattern match`` () =
    compileRun <@ match ref 42 with { contents = x } -> x @> |> should equal 42
    compileRun <@ match { Num = 1 ; Text = "test" ; Value = 2.1 } with { Value = v ; Num = n } -> float n + v @> |> should equal 3.1
    compileRun <@ match { GNum = 1 ; GText = "test" ; GValue = 2.1 } with { GValue = v ; GNum = n } -> float n + v @> |> should equal 3.1

[<Test>]
let ``3. Union pattern match`` () =
    compileRun <@ match None with Some 42 -> 1 | Some x -> x - 1 | None -> -1 @> |> should equal -1
    compileRun <@ match Some (1,"test") with Some(2,"test") -> false | Some(1,"test") -> true | _ -> false @> |> should equal true
    compileRun <@ match Some (Some(Some (42))) with None -> false | Some None -> false | Some (Some None) -> false | Some (Some (Some _)) -> true @> |> should equal true
    compileRun <@ match Choice<int,int>.Choice1Of2 12 with Choice2Of2 _ -> 0 | Choice1Of2 i -> i @> |> should equal 12
    compileRun <@ match Choice<string * int,int>.Choice1Of2("test", 42) with Choice2Of2 _ -> 0 | Choice1Of2("test", i) -> i | Choice1Of2 _ -> -1 @> |> should equal 42
    compileRun <@ match B(42, "text") with B(i,_) -> i | _ -> -1 @> |> should equal 42
    compileRun <@ match GA (15,27) with GA (i,j) -> i + j | _ -> -1 @> |> should equal 42

[<Test>]
let ``3. Union with user-defined property`` () =
    compileRun <@ (Float 900.).ToScalar @> |> should equal ((Float 900.).ToScalar)

[<Test>]
let ``3. Union with user-defined property 2`` () =
    compileRun <@ (Red 1).Convert @> |> should equal ((Red 1).Convert)

[<Test>]
let ``3. Peano arithmetic`` () =
    let int2Peano = compileRun <@ let rec int2Peano n = if n = 0 then Zero else Succ(int2Peano (n-1)) in int2Peano @>
    let peano2Int = compileRun <@ let rec peano2Int p = match p with Zero -> 0 | Succ p -> 1 + peano2Int p in peano2Int @>
    42 |> int2Peano |> peano2Int |> should equal 42


[<Test>]
let ``3. FSharp exceptions`` () =
    compileRun <@ try raise FSharpException1 with FSharpException1 -> true | _ -> false @> |> should equal true
    compileRun <@ try raise <| FSharpException2 "test" with FSharpException1 -> "" | FSharpException2 t -> t @> |> should equal "test"
    compileRun <@ try raise <| MatchFailureException ("test", 17, 25) with MatchFailureException(_,i,j) -> i + j @> |> should equal 42

[<Test>]
let ``3. AutoOpened modules`` () =
    compileRun <@ autoOpenedValue @> |> should equal autoOpenedValue

[<Test>]
let ``3. Extension methods`` () =
    compileRun <@ let c = new ClassWithOptionalParams() in c.ExtensionMethod 42 @> |> should equal 42
    compileRun <@ ClassWithOptionalParams.StaticExtensionMethod 42 @> |> should equal 42

[<Test>]
let ``3. Generic values`` () =
    compileRun <@ genericValue<int> @> |> should equal genericValue<int>

[<Test>]
let ``4. Sequence builders`` () =
    compileRun 
        <@ 
            seq { 
                for i in 1 .. 100 do
                    if i % 2 = 0 then
                        yield i
            }
        @> 
    |> Seq.length |> should equal 50

[<Test>]
let ``4. List builders`` () =
    compileRun 
        <@ 
            [
                for i in 1 .. 100 do
                    if i % 2 = 0 then
                        yield i
            ]
        @> 
    |> List.length |> should equal 50

[<Test>]
let ``4. Async workflows`` () =
    let fibAsync =
        compileRun 
            <@ 
                let rec fibAsync n = async {
                    if n <= 1 then return n
                    else
                        let! fnn = fibAsync(n-2)
                        let! fn = fibAsync(n-1)
                        return fnn + fn
                }

                Async.RunSynchronously << fibAsync
            @> 

    fibAsync 10 |> should equal 55

[<Test>]
let ``4. Constructor with optional params`` () =
    compileRun <@ let c = new ClassWithOptionalParams(age = 42) in c.Age @> |> should equal 42

[<Test>]
let ``4. Method with optional params`` () =
    compileRun <@ let c = ClassWithOptionalParams.Create(?age = Some 42) in c.Age @> |> should equal 42

[<Test>]
let ``4. Pickled values`` () =
    let value = [|1..100|] |> Array.map (fun i -> string i, i)
    compileRun <@ let x = value in x.[42] <- ("",0) ; value.[42] @> |> should equal ("",0)

[<Test>]
let ``4. Default constructor`` () =
    compileRun <@ let ts = new TestStruct() in ts.Age @> |> should equal ((new TestStruct()).Age)

[<Test>]
let ``4. Nested Quotations`` () =
    compileRun 
        <@ 
            compileRun 
                <@ 
                    let rec fact n = 
                        if n = 0 then 1 
                        else n * fact(n-1) 
                    fact 5 
                @> 
        @> 
    |> should equal 120


[<Test>]
let ``4. Query expressions`` () =
    compileRun
        <@
            query {
                for i in 1 .. 10000 do
                where (i % 3 = 0)
                sortByDescending i
                take 5
            }
        @>
    |> Seq.length |> should equal 5

[<Test>]
let ``5. Closure values`` () =
    let a = obj()
    compileRun 
        <@
            a
        @>
    |> should equal a