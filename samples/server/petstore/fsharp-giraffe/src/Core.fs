module TypeSet.Core

open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

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
    ProvidedProperty("show", invokeType, isStatic = true, getterCode = fun _ -> invokeCode)


[<TypeProvider>]
type TPrintProvider (config : TypeProviderConfig, assemblies:_,  assemblyReplacementMap:_, addDefaultProbingLocation:_)  as this =
    inherit TypeProviderForNamespaces (config, assemblies, assemblyReplacementMap, addDefaultProbingLocation)

    let ns = "TypeSet.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let tPrintProvider = ProvidedTypeDefinition(asm, ns, "TPrint", Some(typeof<obj>))

    let parameters = [ProvidedStaticParameter("FormatString", typeof<string>)]

    do tPrintProvider.DefineStaticParameters(parameters, fun typeName args ->
        let formatString = args.[0] :?> string

        let provider = ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        //provider.HideObjectMethods <- true

        formatString |> parseFormatString |> toMethod |> provider.AddMember

        provider
        )
    
    do
        this.AddNamespace(ns, [tPrintProvider])

[<assembly:TypeProviderAssembly>]
do ()

let foo = TPrintProvider<"Hello %s">.show "sup"