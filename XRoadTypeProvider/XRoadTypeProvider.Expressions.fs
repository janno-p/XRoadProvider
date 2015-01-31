module XRoadTypeProvider.Expressions

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open System.Xml
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
    let responseTypes = operation.Response.Body
                        |> List.map (fun p -> typeCache |> getType p.Reference)
    let innerType = match responseTypes with
                    | [] -> typeof<unit>
                    | tp::[] -> tp
                    | many -> many |> Array.ofList |> FSharpType.MakeTupleType
    // Multipart content will be implemented later
    //match operation.Response.MultipartContent with
    //| [] -> innerType
    //| _ -> typedefof<Runtime.IXRoadResponseWithAttachments<_>>.MakeGenericType(innerType)
    innerType

let buildParameters typeCache operation =
  [ yield! operation.Request.Body
           |> List.map (fun p -> ProvidedParameter(p.Name, typeCache |> getType p.Reference))
    yield ProvidedParameter("settings", typeof<XRoadHeader>) ]

let writeStartElement = typeof<XmlWriter>.GetMethod("WriteStartElement", [| typeof<string> |])
let writeEndElement = typeof<XmlWriter>.GetMethod("WriteEndElement", [| |])

let createXRoadOperationMethod typeCache operation =
    let xrdHeaders = requiredXRoadHeaders(operation)
    let parameters = operation |> buildParameters typeCache
    let returnType = operation |> buildReturnType typeCache

    let settingsIndex = 1 + (parameters |> List.findIndex (fun p -> p.ParameterType = typeof<XRoadHeader>))

    let providedMethod = ProvidedMethod(operation.Name.LocalName, parameters, returnType)
    providedMethod.InvokeCode <- fun args ->
        let operationName = operation.Name.LocalName
        let operationNamespace = operation.Name.NamespaceName
        let operationVersion = operation.Version |> Option.orDefault ""
        let writer = Var("writer", typeof<XmlWriter>)
        let f =
            Expr.Lambda(writer,
                let exps = operation.Request.Body
                           |> List.mapi (fun i part ->
                               let partName = part.Name
                               let index = i
                               let tp = typeCache |> getType part.Reference
                               let mi = tp.GetMethod("Serialize", [| typeof<XmlWriter> |])
                               Expr.Sequential(
                                   Expr.Call(Expr.Var(writer), writeStartElement, [ Expr.Value(partName) ]),
                                   Expr.Sequential(
                                       Expr.Call(Expr.Coerce(args.[index + 1], tp), mi, [Expr.Coerce(Expr.Var(writer), typeof<System.Xml.XmlWriter>)]),
                                       Expr.Call(Expr.Var(writer), writeEndElement, [])
                                       )))
                match exps with
                | [] -> Expr.Value(())
                | exp::[] -> exp
                | exp::exps -> exps |> List.fold (fun exp e -> Expr.Sequential(exp, e)) exp)
        <@@ XRoadRequest.makeRpcCall((%%args.[0]: XRoadContext) :> IXRoadContext,
                                     operationName,
                                     operationVersion,
                                     operationNamespace,
                                     (%%args.[settingsIndex]: XRoadHeader),
                                     xrdHeaders |> Array.ofList,
                                     (%%f: System.Xml.XmlWriter -> unit)) @@>
    providedMethod

    // ====

    (*
    let tpoox = parameters
                |> List.choose (fun pm -> match pm.ParameterType with
                                            | :? ProvidedTypeDefinition as x -> Some x
                                            | _ -> None)
                |> List.map (fun tp -> tp, )
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
    *)

