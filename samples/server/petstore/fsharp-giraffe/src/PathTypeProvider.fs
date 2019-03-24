module OpenAPITypeProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open OpenAPITypeProvider.Types

let dataTypeLookup = Map.empty.Add("string","%s").Add("int","%i").Add("float","%f").Add("int","%i").Add("long", "%f").Add("bool","%b")

type Format =
    | FString of Format
    | FInt of Format
    | Other of char * Format
    | End

let parseFormatString str =
    let rec parseFormat chars =
        match chars with
        | '%'::'d'::t -> FInt (parseFormat t)
        | '%'::'s'::t -> FString (parseFormat t)
        | c::t -> Other (c, parseFormat t)
        | [] -> End
    parseFormat (Seq.toList str)

let rec invoker printers format =
    match format with
    | End ->
        let arr = Expr.NewArray(typeof<string>, List.rev printers)
        let conc = typeof<string>.GetMethod("Concat", [|typeof<string[]>|])
        Expr.Call(conc, [arr])
    | Other (c, t) ->
        invoker (<@@ string<char> c @@> :: printers) t
    | FInt t ->
        let v = Var("v", typeof<int>)
        let printer = <@@ string<int> (%%(Expr.Var v)) @@>
        Expr.Lambda(v, invoker (printer::printers) t)
    | FString t ->
        let v = Var("v", typeof<string>)
        let printer = <@@ %%(Expr.Var v):string @@>
        Expr.Lambda(v, invoker (printer::printers) t)

let toMethod format =
    let invokeCode =
        invoker [] format
    let invokeType = invokeCode.Type
    ProvidedProperty("show", invokeType, IsStatic = true, GetterCode = fun _ -> invokeCode)

    
[<TypeProvider>]
type OpenAPITypeProviderImplementation (cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (cfg, assemblyReplacementMap=[("PathTypeProvider.DesignTime", "PathTypeProvider.Runtime")], addDefaultProbingLocation=true)

    let ns = "OpenAPITypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    
    let provider = ProvidedTypeDefinition(asm, ns, "PathTypeProvider", (Some typeof<obj>),  hideObjectMethods = true, nonNullable = true, isErased = true)

    let parameters = [ 
        ProvidedStaticParameter("Path", typeof<string>),
        ProvidedStaticParameter("Types", typeof<string list>)
    ]

    do tp.DefineStaticParameters(parameters, fun typeName args ->
        try
          let path = args.[0] :?> string
          let urlParamTypes = args.[1] :? string list
          formatString |> parseFormatString |> toMethod |> provider.addMember
          provider
        with ex -> ex |> sprintf "%A" |> failwith
    )

    let helpText = 
        """<summary>Type Provider for Open API F#/Giraffe Server Stub Generator</summary>
           <param name='Path'>The path of an OpenApi operation</param>
           <param name='Types'>A list of types for each URL parameter</param>
        """

    do tp.AddXmlDoc helpText
    do this.AddNamespace(ns, [tp])

[<assembly:TypeProviderAssembly()>]
do ()