namespace QuotationCompiler.Simple

open System
open System.Reflection
open System.Diagnostics
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open QuotationCompiler
open QuotationCompiler.Utilities
open QuotationCompiler.Dependencies

module Utils =
    type CustomField(parent : Type, name : string, fieldType : Type) =
        inherit FieldInfo()

        override x.Name = name
        override x.FieldType = fieldType
        override x.DeclaringType = parent
        override x.Attributes = FieldAttributes.Public
        override x.GetCustomAttributes(_) = [||]
        override x.GetCustomAttributes(_,_) = [||]
        override x.IsDefined(_,_) = false
        override x.ReflectedType = parent

        override x.FieldHandle = failwith "not supported"
        override x.GetValue(_) = failwith "not supported"
        override x.SetValue(_,_,_,_,_) = failwith "not supported"

    type CustomParameterInfo(name : string, t : Type) =
        inherit ParameterInfo()

        override x.Attributes = ParameterAttributes.None
        override x.CustomAttributes = Seq.empty
        override x.GetCustomAttributes(_) = [||]
        override x.GetCustomAttributes(_,_) = [||]
        override x.GetCustomAttributesData() = System.Collections.Generic.List<CustomAttributeData>() :> _
        override x.IsDefined(_,_) = false
        override x.HasDefaultValue = false
        override x.DefaultValue = null

        override x.Name = name
        override x.ParameterType = t


    type CustomMethod(parent : Type, name : string, argTypes : Type[], retType : Type) =
        inherit MethodInfo()

        let par = argTypes |> Array.mapi (fun i t -> CustomParameterInfo(sprintf "arg%d" i, t) :> ParameterInfo)

        override x.ReflectedType = parent
        override x.DeclaringType = parent
        override x.Name = name
        override x.Attributes = MethodAttributes.Public
        override x.ReturnType = retType

        override x.GetCustomAttributes(_) = [||]
        override x.GetCustomAttributes(_,_) = [||]
        override x.IsDefined(_,_) = false

        override x.GetParameters() = par
        override x.GetMethodImplementationFlags() = MethodImplAttributes.IL
        override x.GetBaseDefinition() = x :> MethodInfo
        override x.ReturnTypeCustomAttributes = 
            { new ICustomAttributeProvider with
                member x.GetCustomAttributes(_) = [||]
                member x.GetCustomAttributes(_,_) = [||]
                member x.IsDefined(_,_) = false
            }


        override x.Invoke(_,_,_,_,_) = failwith ""
        override x.MethodHandle = failwith ""

    type CustomConstructor(parent : Type, argTypes : Type[]) =
        inherit ConstructorInfo()

        let par = argTypes |> Array.mapi (fun i t -> CustomParameterInfo(sprintf "arg%d" i, t) :> ParameterInfo)


        override x.ReflectedType = parent
        override x.DeclaringType = parent
        override x.Attributes = MethodAttributes.Public

        override x.GetCustomAttributes(_) = [||]
        override x.GetCustomAttributes(_,_) = [||]
        override x.IsDefined(_,_) = false
        override x.GetMethodImplementationFlags() = MethodImplAttributes.IL
        override x.GetParameters() = par

        override x.Name = failwith ""
        override x.Invoke(_,_,_,_,_) = failwith ""
        override x.Invoke(_,_,_,_) = failwith ""
        override x.MethodHandle = failwith ""

    type CustomProperty(parent : Type, name : string, valueType : Type) =
        inherit PropertyInfo()

        let get = CustomMethod(parent, sprintf "get_%s" name, [||], valueType) :> MethodInfo
        let set = CustomMethod(parent, sprintf "set_%s" name, [|valueType|], typeof<System.Void>) :> MethodInfo

        let acc = [| get; set |]

        override x.Name = name
        override x.DeclaringType = parent
        override x.ReflectedType = parent

        override x.GetCustomAttributes(_) = [||]
        override x.GetCustomAttributes(_,_) = [||]
        override x.IsDefined(_,_) = false

        override x.PropertyType = valueType
        override x.GetAccessors(_) = acc
        override x.GetGetMethod(_) = get
        override x.GetSetMethod(_) = set
        override x.GetIndexParameters() = [||]
        override x.Attributes = PropertyAttributes.None
        override x.CanRead = true
        override x.CanWrite = true

        override x.SetValue(_,_,_,_,_,_) = failwith ""
        override x.GetValue(_,_,_,_,_) = failwith ""

    type CustomType(baseType : Type, name : string) =
        inherit Type()
        let guid = Guid.NewGuid()

        override x.GUID = guid
        override x.Name = name
        override x.FullName = name
        override x.IsArrayImpl() = false
        override x.IsByRefImpl() = false
        override x.IsPointerImpl() = false
        override x.IsPrimitiveImpl() = false
        override x.IsCOMObjectImpl() = false
        override x.HasElementTypeImpl() = false
        override x.Assembly = typeof<CustomType>.Assembly
        override x.Module = typeof<CustomType>.Module
        override x.UnderlyingSystemType = x :> Type
        override x.BaseType = baseType

        override x.GetField(_,_) = null
        override x.GetFields(_) = [||]

        override x.GetCustomAttributes(_,_) = [||]
        override x.GetCustomAttributes(_) = [||]
        override x.IsDefined(_,_) = false
        override x.GetAttributeFlagsImpl() = TypeAttributes.Class ||| TypeAttributes.Public
        override x.AssemblyQualifiedName = sprintf "Temp.%s" name
        override x.Namespace = "Temp"

        override x.GetNestedType(_,_) = null
        override x.GetNestedTypes(_) = [||]
        override x.GetConstructors(_) = [||]
        override x.GetMethods(_) = [||]
        override x.GetInterfaces() = [||]
        override x.GetEvents(_) = [||]
        override x.GetProperties(_) = [||]
        override x.GetMembers(_) = [||]

        override x.GetMethodImpl(name,flags,binder,_,argTypes,_) = null
        override x.GetConstructorImpl(_,_,_,_,_) = null
        override x.GetInterface(_,_) = null
        override x.GetEvent(_,_) = null
        override x.GetPropertyImpl(_,_,_,_,_,_) = null
        override x.GetElementType() = null

        override x.InvokeMember(_,_,_,_,_,_,_,_) = failwith "not supported"

[<AutoOpen>]
module ExpressionExtensions = 
    open Utils
    type Expr with
        static member FieldGet(obj : Expr, fieldName : string, fieldType : Type) =
            Expr.FieldGet(obj, CustomField(obj.Type, fieldName, fieldType))

        static member PropertyGet(obj : Expr, propName : string, propType : Type) =
            Expr.PropertyGet(obj, CustomProperty(obj.Type, propName, propType))

        static member Call(obj : Expr, methName : string, args : list<Expr>, retType : Type) =
            let meth = CustomMethod(obj.Type, methName, args |> List.map (fun e -> e.Type) |> List.toArray, retType)
            Expr.Call(obj, meth, args)

    type Type with
        static member Create(name : string, baseType : Type) = Utils.CustomType(baseType, name) :> Type
        static member Create(name : string) = Utils.CustomType(typeof<obj>, name) :> Type
    
    type MethodInfo with
        static member Create(this : Type, name : string, args : Type[], ret : Type) = Utils.CustomMethod(this, name, args, ret) :> MethodInfo

    type ConstructorInfo with
        static member Create(this : Type, args : Type[]) = Utils.CustomConstructor(this, args) :> ConstructorInfo

    type PropertyInfo with
        static member Create(this : Type, name : string, valueType : Type) = Utils.CustomProperty(this, name, valueType) :> PropertyInfo

    type FieldInfo with
        static member Create(this : Type, name : string, valueType : Type) = Utils.CustomField(this, name, valueType) :> FieldInfo

type ClassRec =
    {
        Name        : string
        BaseType    : Option<Type * list<Expr>>
        Arguments   : list<Var>
        Fields      : list<Var>
        Members     : list<string * list<Var> * (Expr -> Expr)>
    }

type Declaration =
    | Binding of Var * list<Var> * Expr
    | Class of ClassRec

type Module =
    {
        Name            : string
        Declarations    : list<Declaration>
    }


module Compiler =
    let defaultRange = range0
    let private sscs = lazy ( SimpleSourceCodeServices() )

    let private printErrors (errors : FSharpErrorInfo []) =
        if Array.isEmpty errors then
            sprintf "Compilation failed with errors."
        else
            let errorMsgs = errors |> Seq.map (fun e -> sprintf "   ** %O" e) |> String.concat Environment.NewLine
            sprintf "Compilation failed with errors:%s%s" Environment.NewLine errorMsgs

    let private binding (deps : DependencyContainer) (values : ValueManager) (v : Var) (args : list<Var>) (body : Expr) =
        let synConsArgs = 
            match args with
                | [] -> SynConstructorArgs.Pats []
                | args ->
                    let pats =
                        args |> List.map (fun v ->
                            let t = v.Type
                            deps.Append t
                            SynPat.Typed(
                                SynPat.LongIdent(mkLongIdent defaultRange [mkIdent defaultRange v.Name], None, None, Pats [], None, defaultRange),
                                sysTypeToSynType defaultRange t,
                                defaultRange
                            )
                        )
                    SynConstructorArgs.Pats pats
        let synPat = SynPat.LongIdent(mkLongIdent defaultRange [mkIdent defaultRange v.Name], None, None, synConsArgs, None, defaultRange)
        let argInfo = args |> List.map (fun a -> [SynArgInfo([], false, Some (mkIdent defaultRange a.Name))])
        let synValData = SynValData.SynValData(None, SynValInfo(argInfo, SynArgInfo([], false, None)), None)

        let expr = body |> Compiler.convertExprToAst deps values
        SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, synPat, None, expr, range0, SequencePointInfoForBinding.SequencePointAtBinding defaultRange)

    let private mutableDefaultBinding (deps : DependencyContainer) (ident : Ident) (t : Type) =
        let values = ValueManager()
        let synConsArgs = SynConstructorArgs.Pats []
        let synPat = SynPat.LongIdent(mkLongIdent defaultRange [ident], None, None, synConsArgs, None, defaultRange)
        let synValData = SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None)
        let expr = Expr.DefaultValue(t) |> Compiler.convertExprToAst deps values
        SynBinding.Binding(
            Some SynAccess.Private, 
            SynBindingKind.NormalBinding, 
            false, 
            true, 
            [], PreXmlDoc.Empty, 
            synValData, 
            synPat, 
            None, 
            expr, 
            range0, 
            SequencePointInfoForBinding.SequencePointAtBinding defaultRange
        )
        
    let private classDecl (deps : DependencyContainer) (values : ValueManager) (c : ClassRec) : SynTypeDefn =

        let fields =
            c.Fields |> List.map (fun v ->
                SynField.Field(
                    [{ TypeName = mkLongIdent defaultRange [mkIdent defaultRange "DefaultValue"]; ArgExpr = SynExpr.Tuple([], [], range0); Target = None; AppliesToGetterAndSetter = false; Range = range0 }], 
                    false, 
                    Some (mkIdent defaultRange v.Name), 
                    sysTypeToSynType defaultRange v.Type, 
                    true, 
                    PreXmlDocEmpty, 
                    Some SynAccess.Public, 
                    defaultRange
                )
            )

        let members =
            let flags =
                {
                    IsInstance = true
                    IsDispatchSlot = false
                    IsFinal = false
                    IsOverrideOrExplicitImpl = false
                    MemberKind = MemberKind.Member
                }

            let members =
                c.Members |> List.map (fun (name, args, f) ->
                    let self = mkUniqueIdentifier defaultRange
                    let body = f (Var(self.idText, typeof<obj>) |> Expr.Var)
                    self, name, args, body
                )

            members |> List.map (fun (self, name, args, body) ->
                let def = Compiler.convertExprToAst deps values body
            
                let isGeneric =
                    if body.Type.ContainsGenericParameters then true
                    else args |> List.exists (fun a -> a.Type.ContainsGenericParameters)

                let typePars = 
                    if isGeneric then
                        let rec getAll (t : Type) =
                            if t.IsGenericParameter then [t]
                            elif t.IsPrimitive then []
                            elif t.IsGenericType then 
                                t.GetGenericArguments() |> Array.toList |> List.collect getAll
                            else
                                []  
  
                        let args = 
                            (getAll body.Type) :: (args |> List.map (fun a -> getAll a.Type)) |> List.concat |> System.Collections.Generic.HashSet
                        
                        args |> Seq.toList |> List.map (fun t -> Typar(mkIdent range0 t.Name, TyparStaticReq.NoStaticReq, false))
                    else
                        []

                let pats =
                    args |> List.map (fun v ->
                        let t = v.Type
                        let ident = mkIdent range0 v.Name
                        deps.Append t
                        SynPat.Typed(
                            SynPat.Named(SynPat.Wild range0, ident, false, None, defaultRange),
                            //SynPat.LongIdent(mkLongIdent defaultRange [ident], None, None, Pats [], None, defaultRange),
                            sysTypeToSynType defaultRange t,
                            defaultRange
                        )
                    )

                let pats = 
                    match pats with
                        | [] -> []
                        | _ -> [SynPat.Tuple(pats, range0)]

                let synConsArgs = SynConstructorArgs.Pats pats

                let synPat = 
                    let id = LongIdentWithDots([self; mkIdent defaultRange name], [range0; range0])

                    match typePars with
                        | [] ->
                            SynPat.LongIdent(id, None, None, synConsArgs, None, defaultRange)
                        | pars ->
                            let parDecls = typePars |> List.map (fun tp -> SynTyparDecl.TyparDecl([], tp))
                            let decls = SynValTyparDecls.SynValTyparDecls(parDecls, false, [])
                            SynPat.LongIdent(id, None, Some decls, synConsArgs, None, defaultRange)

                let binding = 
                    let names = args |> List.map (fun a -> a.Name)

                    let argInfo =
                        [
                            [SynArgInfo([], false, Some self)]
                            names |> List.map (fun a -> SynArgInfo([], false, Some (mkIdent range0 a)))
                        ]

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

        let ctor = 
            let ctorArgs = 
                c.Arguments |> List.map (fun v -> 
                    SynSimplePat.Typed(
                        SynSimplePat.Id(mkIdent range0 v.Name, None, false, false, false, range0),
                        sysTypeToSynType range0 v.Type,
                        range0
                    )
                )

            SynMemberDefn.ImplicitCtor(None, [], ctorArgs, None, range0)

        let inh = 
            match c.BaseType with
                | Some(bt, args) -> 
                    let exprs = args |> List.map (Compiler.convertExprToAst deps values)
                    let tup = SynExpr.Tuple(exprs, exprs |> List.map (fun _ -> defaultRange), defaultRange)
                    let i = SynMemberDefn.ImplicitInherit(sysTypeToSynType defaultRange bt, tup, None, defaultRange)
                        
                    //let j = SynMemberDefn.Inherit(sysTypeToSynType defaultRange bt, None, defaultRange)
                    [i]
                | _ ->
                    []

        let all = [ctor] @ inh @ members

        let inhThings =
            match c.BaseType with
                | Some(bt, args) -> 
                    [sysTypeToSynType defaultRange bt, range0, None]
                | _ ->
                    []
//                c.Arguments |> List.map (fun v -> 
//                    let t = sysTypeToSynType defaultRange v.Type
//                    let i = mkIdent defaultRange v.Name
//                    t, range0, Some i
//                )

        let repr = 
            SynTypeDefnRepr.Simple(
                SynTypeDefnSimpleRepr.General(
                    SynTypeDefnKind.TyconClass, 
                    inhThings, 
                    [], 
                    fields, 
                    false, 
                    false, 
                    None, 
                    range0
                ), 
                range0
            )

        let comp = 
            SynComponentInfo.ComponentInfo(
                [], 
                [], 
                [], 
                [mkIdent range0 c.Name], 
                PreXmlDocEmpty, 
                false, 
                None, 
                range0
            )
        //let repr = SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconClass, all, defaultRange)
        TypeDefn(comp, repr, all, range0)

    let compile (m : Module) =
        let dependencies = DependencyContainer()
        let values = ValueManager()
        let defaultRange = range0

        let decls =
            m.Declarations |> List.map (fun d ->
                match d with
                    | Binding(v,args,body) ->
                        let binding = binding dependencies values v args body
                        SynModuleDecl.Let(false, [binding], defaultRange)

                    | Class c ->
                        let c = classDecl dependencies values c
                        SynModuleDecl.Types([c], defaultRange)
            )

        let closure = values.Values
        let valueBindings = 
            closure |> List.map (fun v ->
                SynModuleDecl.Let(false, [mutableDefaultBinding dependencies v.Ident v.Type], defaultRange)
            )

        let all = valueBindings @ decls

        let moduleDeclsToParsedInput (decls : SynModuleDecl list) =
            let modl = SynModuleOrNamespace([mkIdent defaultRange m.Name], true, decls, PreXmlDoc.Empty,[], None, defaultRange)
            let file = ParsedImplFileInput("/QuotationCompiler.fs", false, QualifiedNameOfFile(mkIdent defaultRange m.Name), [],[], [modl],false)
            ParsedInput.ImplFile file

        let input = all |> moduleDeclsToParsedInput
        let assemblyName = sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N"))
        let dependencies = dependencies.Assemblies |> List.map (fun a -> a.Location)
        match sscs.Value.CompileToDynamicAssembly([input], assemblyName, dependencies, None, debug = false) with
        | _, _, Some a -> 
            let t = a.GetType(m.Name)
            if isNull t then failwith "compilation failed"
            else
                for c in closure do
                    let prop = t.GetProperty(c.Ident.idText, BindingFlags.NonPublic ||| BindingFlags.Static)
                    prop.SetValue(null, c.Value)
                t
        | errors, _, _ -> failwith (printErrors errors)