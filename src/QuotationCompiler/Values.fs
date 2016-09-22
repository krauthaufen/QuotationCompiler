namespace QuotationCompiler.Utilities

//
//  Provides a mechanism in which quotation values are captured
//  in the form of top-level cached bindings
//

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

open Microsoft.FSharp.Quotations

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

open QuotationCompiler.Utils


/// Represents a value that will be passed to the construction of the value
type ValueBinding =
    {
        /// Identifier used for referencing the value.
        Ident : Ident
        /// The value.
        Value : obj

        /// The declared type of the value.
        Type : Type
    }


type ValueManager() =
    let idGen = new ObjectIDGenerator()
    let container = new Dictionary<int64, ValueBinding> ()

    /// Caches value and returns unique identifier for instance
    member __.Append(obj:obj, ty : Type) =
        let id, isFirst = idGen.GetId obj
        if isFirst then
            let ident = mkUniqueIdentifier range0
            container.Add(id, { Ident = ident ; Value = obj; Type = ty })
            ident
        else
            let entry = container.[id] in entry.Ident

    /// Returns all cached values
    member __.Values = container |> Seq.map(function (KeyValue(_,v)) -> v) |> Seq.toList