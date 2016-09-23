namespace QuotationCompiler.Utilities

open System
open System.Reflection
open System.IO

open Microsoft.FSharp.Quotations
    
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

open QuotationCompiler

/// Contains the result of a quotation converted to F# AST
type QuotationAst =
    {
        /// Syntax tree containing the converted quotation
        Tree : ParsedInput
        /// Assembly dependencies of F# Quotation
        Dependencies : Assembly list
        /// F# module containing converted quotation
        ModuleName : string
        /// F# function name defining converted quotation
        FunctionName : string

        Arguments : list<obj>
    }

type QuotationCompiler =

    /// <summary>
    ///     Converts supplied quotation tree to F# AST.
    /// </summary>
    /// <param name="expr">Quotation to be converted.</param>
    /// <param name="compiledModuleName">Name of compiled module containing AST.</param>
    /// <param name="compiledFunctionName">Name of compiled function name containing AST.</param>
    /// <param name="serializer">Serializer used for pickling values spliced into expression trees. Defaults to BinaryFormatter.</param>
    /// <returns>Untyped AST and assembly dependencies.</returns>
    static member ToParsedInput(expr : Expr, ?compiledModuleName : string, ?compiledFunctionName : string) : QuotationAst =
        let compiledModuleName =
            match compiledModuleName with
            | None -> sprintf "CompiledQuotationModule-%O" <| Guid.NewGuid()
            | Some cmn -> cmn

        let compiledFunctionName = defaultArg compiledFunctionName "compiledQuotation"
        let dependencies, ast, args = Compiler.convertExprToModule compiledModuleName compiledFunctionName expr
        {
            Tree = ast
            Dependencies = dependencies
            ModuleName = compiledModuleName
            FunctionName = compiledFunctionName
            Arguments = args
        }

#if DEBUG
    /// <summary>
    ///     Parses source code string into untyped assembly.
    /// </summary>
    /// <param name="source">F# code to be parsed.</param>
    static member ParseFSharpSource(source : string) : ParsedInput option = 
        Async.RunSynchronously(async {
            let fileName = "/mock.fs"
            let checker = FSharpChecker.Create()
            let! options = checker.GetProjectOptionsFromScript(fileName, "")
            let! parsed = checker.ParseFileInProject(fileName, source, options)
            return parsed.ParseTree
        })
#endif

namespace QuotationCompiler

open System
open System.Reflection
open System.Collections.Concurrent
open System.IO

open Microsoft.FSharp.Quotations

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

open QuotationCompiler.Utilities
open QuotationCompiler.Dependencies


type QuotationCompiler private () =

    /// Memoized compiled expression trees
    static let compiledDelayedExprs = new ConcurrentDictionary<Expr, obj>(new ExprEqualityComparer())
    static let compiledExprs = new ConcurrentDictionary<Expr, obj>(new ExprEqualityComparer())
    static let sscs = lazy (new SimpleSourceCodeServices())

    static let printErrors (errors : FSharpErrorInfo []) =
        if Array.isEmpty errors then
            sprintf "Compilation failed with errors."
        else
            let errorMsgs = errors |> Seq.map (fun e -> sprintf "   ** %O" e) |> String.concat Environment.NewLine
            sprintf "Compilation failed with errors:%s%s" Environment.NewLine errorMsgs

    /// <summary>
    ///     Compiles provided quotation tree to assembly.
    /// </summary>
    /// <param name="expr">Quotation to be compiled.</param>
    /// <param name="targetDirectory">Target directory. Defaults to system temp folder.</param>
    /// <param name="assemblyName">Assembly name. Defaults to auto generated name.</param>
    /// <param name="compiledModuleName">Name of compiled module containing AST.</param>
    /// <param name="compiledFunctionName">Name of compiled function name containing AST.</param>
    static member ToAssembly(expr : Expr, ?targetDirectory : string, ?assemblyName : string, 
                                            ?compiledModuleName : string, ?compiledFunctionName : string) : string =

        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let targetDirectory = match targetDirectory with None -> Path.GetTempPath() | Some p -> p
        let qast = QuotationCompiler.ToParsedInput(expr, ?compiledModuleName = compiledModuleName, ?compiledFunctionName = compiledFunctionName)
        match qast.Arguments with
            | [] ->
                let dependencies = qast.Dependencies |> List.map (fun a -> a.Location)
                let location = Path.Combine(targetDirectory, assemblyName + ".dll")
                let pdbFile = Path.Combine(targetDirectory, assemblyName + ".pdb")
                let errors, code = sscs.Value.Compile([qast.Tree], assemblyName, location, dependencies, executable = false, pdbFile = pdbFile)
        

                if code = 0 then location
                else
                    raise <| new QuotationCompilerException(printErrors errors)
            | _ ->
                raise <| new QuotationCompilerException("cannot compile Quotation with closure-values to assembly")

    /// <summary>
    ///     Compiles provided quotation tree to dynamic assembly.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="assemblyName">Assembly name. Defaults to auto generated name.</param>
    static member ToDynamicAssembly(expr : Expr, ?assemblyName : string) : MethodInfo * obj[] =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let qast = QuotationCompiler.ToParsedInput(expr)
        let dependencies = qast.Dependencies |> List.map (fun a -> a.Location)
        match sscs.Value.CompileToDynamicAssembly([qast.Tree], assemblyName, dependencies, None, debug = false) with
        | _, _, Some a -> 
            let meth = a.GetType(qast.ModuleName).GetMethod(qast.FunctionName)
            let args = qast.Arguments |> List.toArray
            meth, args

        | errors, _, _ -> raise <| new QuotationCompilerException (printErrors errors)

    /// <summary>
    ///     Compiles provided quotation tree to value.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="assemblyName">Assembly name. Defaults to auto generated name.</param>
    static member ToObject(expr : Expr, ?assemblyName : string) : obj =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let meth, args = QuotationCompiler.ToDynamicAssembly(expr, assemblyName)
        meth.Invoke(null, args)

    /// <summary>
    ///     Compiles provided quotation tree to function.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="useCache">Keep compiled functions for syntactically equal quotations in cache. Defaults to true.</param>
    static member ToFunc(expr : Expr<'T>, ?useCache:bool) : unit -> 'T =
        let useCache = defaultArg useCache true
        let compile _ =
            let lambda = Expr.Lambda(Var("unitVar", typeof<unit>), expr.Raw)
            let value = QuotationCompiler.ToObject(lambda)
            value :?> unit -> 'T

        if useCache then
            compiledDelayedExprs.GetOrAdd(expr, compile >> box) :?> unit -> 'T
        else
            compile ()

    /// <summary>
    ///     Compiles provided quotation tree to value.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="useCache">Keep compiled functions for syntactically equal quotations in cache. Defaults to true.</param>
    static member Eval(expr : Expr<'T>, ?useCache : bool) : 'T =
        let useCache = defaultArg useCache true
        let compile _ =
            let value = QuotationCompiler.ToObject(expr.Raw)
            value :?> 'T

        if useCache then
            compiledExprs.GetOrAdd(expr, compile >> box) :?> 'T
        else
            compile ()


    /// <summary>
    ///     Creates a type having the given methods as members
    /// </summary>
    /// <param name="methods">A list of methods with their (name, arguments, body)s </param>
    static member CreateInstance (methods : list<string * list<Var> * Expr>, ?assemblyName : string, ?compiledModuleName : string) =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let compiledModuleName =
            match compiledModuleName with
            | None -> sprintf "CompiledQuotationModule-%O" <| Guid.NewGuid()
            | Some cmn -> cmn

        let dependencies = DependencyContainer()
        let values = ValueManager()
        let defaultRange = range0

        let methods =
            let flags =
                {
                    IsInstance = true
                    IsDispatchSlot = false
                    IsFinal = false
                    IsOverrideOrExplicitImpl = false
                    MemberKind = MemberKind.Member
                }

            methods |> List.map (fun (name, args, body) ->
                let def = Compiler.convertExprToAst dependencies values body
            
  
                let pats =
                    match args with
                        | [] ->
                            [ SynPat.Paren(SynPat.Const(SynConst.Unit, defaultRange), defaultRange)]
                    
                        | _ ->
                            args |> List.map (fun v ->
                                let t = v.Type
                                let ident = mkIdent range0 v.Name
                                dependencies.Append t
                                SynPat.Typed(
                                    SynPat.Named(SynPat.Wild range0, ident, false, None, defaultRange),
                                    //SynPat.LongIdent(mkLongIdent defaultRange [ident], None, None, Pats [], None, defaultRange),
                                    sysTypeToSynType defaultRange t,
                                    defaultRange
                                )
                            )

                let synConsArgs = SynConstructorArgs.Pats pats

                let self = mkUniqueIdentifier range0

                let synPat = 
                    let id = LongIdentWithDots([self; mkIdent defaultRange name], [range0; range0])

                    
                    SynPat.LongIdent(id, None, None, synConsArgs, None, defaultRange)

                let binding = 
                    let names = args |> List.map (fun a -> a.Name)
                    let argInfo = (self.idText :: names) |> List.map (fun a -> [SynArgInfo([], false, Some (mkIdent range0 a))])
                    let synValData = SynValData.SynValData(Some flags, SynValInfo(argInfo, SynArgInfo([], false, None)), None)
                    
                    SynBinding.Binding(
                        Some SynAccess.Public, 
                        SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, 
                        synValData, synPat, 
                        None, 
                        def, 
                        range0, 
                        SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding
                    )

                SynMemberDefn.Member(
                    binding,
                    range0
                )
            )


        let args = values.Values
        let ctor = 
            let ctorArgs = 
                args |> List.map (fun v -> 
                    SynSimplePat.Typed(
                        SynSimplePat.Id(v.Ident, None, false, false, false, range0),
                        sysTypeToSynType range0 v.Type,
                        range0
                    )
                )

            SynMemberDefn.ImplicitCtor(None, [], ctorArgs, None, range0)
        

        let ident = mkUniqueIdentifier range0

        let comp = SynComponentInfo.ComponentInfo([], [], [], [ident], PreXmlDocEmpty, false, None, range0)
       
        //let repr = SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconClass, ctor :: methods, range0)
        let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.General(SynTypeDefnKind.TyconClass, [], [], [], false, false, None, range0), range0)
        let def = TypeDefn(comp, repr, ctor :: methods, range0)

        let moduleDecl = SynModuleDecl.Types([def], range0)

        let moduleDeclsToParsedInput (decls : SynModuleDecl list) =
            let modl = SynModuleOrNamespace([mkIdent defaultRange compiledModuleName], true, decls, PreXmlDoc.Empty,[], None, defaultRange)
            let file = ParsedImplFileInput("/QuotationCompiler.fs", false, QualifiedNameOfFile(mkIdent defaultRange compiledModuleName), [],[], [modl],false)
            ParsedInput.ImplFile file


        let input = moduleDeclsToParsedInput [moduleDecl]


        let dependencies = dependencies.Assemblies |> List.map (fun a -> a.Location)
        match sscs.Value.CompileToDynamicAssembly([input], assemblyName, dependencies, None, debug = false) with
        | _, _, Some a -> 
            let t = a.GetType(compiledModuleName).GetNestedType(ident.idText)
            let argTypes = args |> List.map (fun a -> a.Type) |> List.toArray
            let argValues = args |> List.map (fun a -> a.Value) |> List.toArray
            let ctor = t.GetConstructor(argTypes)
            if isNull ctor then
                raise <| new QuotationCompilerException ("ctor failed")
            else
                let methods = t.GetMethods(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
                let instance = ctor.Invoke(argValues)
                instance, methods

        | errors, _, _ -> raise <| new QuotationCompilerException (printErrors errors)

