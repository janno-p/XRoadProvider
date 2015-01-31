module XRoadTypeProvider.Expressions

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open XRoadTypeProvider.Runtime
open XRoadTypeProvider.Wsdl

type TypeCache = Dictionary<XmlReference,ProvidedTypeDefinition>

let requiredXRoadHeaders operation =
    let headers, rest =
        operation.Request.Header
        |> List.partition (fun part ->
            match part with
            | IsXteeHeader _ when operation.Style = RpcEncoded -> true
            | IsXRoadHeader _ when operation.Style = DocLiteral -> true
            | _ -> false)
    if rest.Length > 0 then
        failwithf "Unhandled SOAP Header elements detected: %A" rest
    headers |> List.map (fun part -> part.Name)

let getType id (cache: TypeCache) =
    match cache.TryGetValue(id) with
    | true, tp -> tp :> System.Type
    | _ -> failwithf "Unknown WSDL type: %A" id // typeof<obj>??

let buildReturnType typeCache operation =
    let getResponseTypes () = [|
        let rec getTypes (xs: MessagePart list) = seq {
            match xs with
            | [] -> ()
            | part::xs -> yield typeCache |> getType part.Reference
                          yield! getTypes xs
        }
        yield! getTypes operation.Response.Body
    |]
    let innerType = match getResponseTypes() with
                    | [||] -> typeof<unit>
                    | [|tp|] -> tp
                    | many -> FSharpType.MakeTupleType many
    // Multipart content will be implemented later
    //match operation.Response.MultipartContent with
    //| [] -> innerType
    //| _ -> typedefof<Runtime.IXRoadResponseWithAttachments<_>>.MakeGenericType(innerType)
    innerType

let buildParameters typeCache operation = [
    yield! [] |> List.foldBack (fun (p: MessagePart) args ->
                    ProvidedParameter(p.Name, typeCache |> getType p.Reference)::args
                    ) operation.Request.Body
    yield ProvidedParameter("settings", typeof<XRoadHeader>)
]

let createXRoadOperationMethod (typeCache: TypeCache) operation =
    let xrdHeaders = requiredXRoadHeaders(operation)
    let parameters = operation |> buildParameters typeCache
    let returnType = operation |> buildReturnType typeCache

    // ====

    let tpoox = parameters
                |> List.choose (fun pm -> match pm.ParameterType with
                                            | :? ProvidedTypeDefinition as x -> Some x
                                            | _ -> None)
                |> List.map (fun tp -> tp, tp.GetMember("Serialize").[0] :?> MethodInfo)
                |> List.tryFind (fun _ -> true)

    let meth = ProvidedMethod(operation.Name.LocalName, parameters, returnType)
    meth.InvokeCode <- (fun args ->
        let opName, opVer, opNs = (operation.Name.LocalName, operation.Version |> Option.orDefault "", operation.Name.NamespaceName)
        let ps = args |> Seq.ofList |> Seq.skip 1 |> Seq.mapi (fun i exp -> match parameters.[i] with
                                                                            | p when p.ParameterType = typeof<obj> -> Expr.Cast<obj> exp :> Expr
                                                                            | p when p.ParameterType = typeof<XRoadHeader> -> Expr.Coerce(Expr.Cast<XRoadHeader> exp, typeof<obj>)
                                                                            | p ->
                                                                                let pi = typeof<IXRoadEntity>.GetProperty("RootName")
                                                                                Expr.Sequential(
                                                                                    Expr.PropertySet(Expr.Cast<XRoadEntity> exp, pi, Expr.Value(p.Name)),
                                                                                    Expr.Coerce(Expr.Cast<XRoadEntity> exp, typeof<obj>)))
        let pl = Expr.NewArray(typeof<obj>, ps |> Seq.toList)
        match operation.Style with
        | RpcEncoded ->
            let f =
                match tpoox with
                | Some (tp, mi) ->
                    let v = Var("w", typeof<System.Xml.XmlWriter>)
                    Expr.Lambda(v, Expr.Call(Expr.Coerce(args.[1], tp), mi, [Expr.Coerce(Expr.Var(v), typeof<System.Xml.XmlWriter>)]))
                | _ -> <@@ printfn "Nuthin'!" @@>
            <@@ XRoadRequest.makeRpcCall((%%args.[0]: XRoadContext) :> IXRoadContext,
                                            opName,
                                            opVer,
                                            opNs,
                                            %%pl,
                                            xrdHeaders |> Array.ofList,
                                            (%%f: System.Xml.XmlWriter -> unit)) @@>
        | DocLiteral ->
            <@@ XRoadRequest.makeDocumentCall((%%args.[0]: XRoadContext) :> IXRoadContext,
                                                opName,
                                                opVer,
                                                opNs,
                                                %%pl,
                                                xrdHeaders |> Array.ofList) @@>)
    meth
    

