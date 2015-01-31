module XRoadTypeProvider.Expressions

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System
open System.Collections.Generic
open System.Reflection
open System.Xml
open System.Xml.Linq
open XRoadTypeProvider.Runtime
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Wsdl.XsdSchema

type TypeCache = Dictionary<XmlReference,ProvidedTypeDefinition>

let andThen e2 e1 = Expr.Sequential(e1, e2)

let execute defExp exps =
    match exps with
    | [] -> defExp
    | exp::[] -> exp
    | exp::exps -> exps |> List.fold (fun exp e -> exp |> andThen e) exp

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

let getRuntimeType typeName cache =
    match mapPrimitiveType typeName with
    | Some tp -> tp
    | _ -> cache |> getType (SchemaType(typeName))

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

let mWriteStartElement = typeof<XmlWriter>.GetMethod("WriteStartElement", [| typeof<string> |])
let mWriteEndElement = typeof<XmlWriter>.GetMethod("WriteEndElement", [| |])
let mWriteStartAttribute = typeof<XmlWriter>.GetMethod("WriteStartAttribute", [| typeof<string>; typeof<string> |])
let mWriteEndAttribute = typeof<XmlWriter>.GetMethod("WriteEndAttribute", [| |])
let mWriteQualifiedName = typeof<XmlWriter>.GetMethod("WriteQualifiedName", [| typeof<string>; typeof<string> |])
let mGetProperty = typeof<IXRoadEntity>.GetMethod("GetProperty")
let mSetProperty = typeof<IXRoadEntity>.GetMethod("SetProperty")

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
                operation.Request.Body
                |> List.mapi (fun i part ->
                    let partName = part.Name
                    let index = i
                    let tp = typeCache |> getType part.Reference
                    let mi = tp.GetMethod("Serialize", [| typeof<XmlWriter> |])
                    Expr.Call(Expr.Var(writer), mWriteStartElement, [ Expr.Value(partName) ])
                    |> andThen (Expr.Call(Expr.Coerce(args.[index + 1], tp), mi, [Expr.Coerce(Expr.Var(writer), typeof<System.Xml.XmlWriter>)]))
                    |> andThen (Expr.Call(Expr.Var(writer), mWriteEndElement, [])))
                |> execute (Expr.Value(())))
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


let getParentType (cache: TypeCache) (typeDef: TypeDefinition) =
    typeDef.ParentType
    |> Option.bind (fun name ->
        match name with
        | SoapEncType "Array" -> None
        | _ -> cache |> getRuntimeType name |> Some)

let rec addXRoadEntityMembers (providedType: ProvidedTypeDefinition) typeDef cache =
    typeDef |> getParentType cache |> Option.iter (fun tp -> providedType.SetBaseType(tp))

    let createNestedType name typeDef =
        let nestedType = ProvidedTypeDefinition(sprintf "%s'" name, Some typeof<XRoadEntity>, HideObjectMethods=true)
        nestedType.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XRoadEntity() @@>)))
        providedType.AddMember(nestedType)
        addXRoadEntityMembers nestedType typeDef cache
        nestedType :> Type

    let makeGenericMethod (tp: Type) (meth: MethodInfo) =
        meth.MakeGenericMethod(match tp with | :? ProvidedTypeDefinition -> typeof<XRoadEntity> | _ -> tp)

    let properties =
        typeDef.Properties
        |> List.map (fun (name, typ) ->
            let propType = match typ with
                           | XmlReference name -> failwith "never"
                           | TypeReference name -> cache |> getRuntimeType name
                           | TypeDefinition typeDef -> createNestedType name typeDef
            let providedProperty = ProvidedProperty(name, propType)
            providedProperty.GetterCode <- (fun args -> Expr.Call(args.[0], (mGetProperty |> makeGenericMethod propType), [Expr.Value(name)]))
            providedProperty.SetterCode <- (fun args -> Expr.Call(args.[0], (mSetProperty |> makeGenericMethod propType), [Expr.Value(name); args.[1]]))
            providedProperty)

    let serializeMethodParams = [ ProvidedParameter("writer", typeof<XmlWriter>) ]
    let serializeMethod = ProvidedMethod("Serialize", serializeMethodParams, typeof<unit>)
    serializeMethod.InvokeCode <- (fun args ->
        Expr.Call(args.[1], mWriteStartAttribute, [ Expr.Value("type"); Expr.Value(XmlNamespace.Xsi) ])
        |> andThen (Expr.Call(args.[1], mWriteQualifiedName, [ Expr.Value("x"); Expr.Value("x") ]))
        |> andThen (Expr.Call(args.[1], mWriteEndAttribute, []))
        |> andThen (properties
                    |> List.map(fun prop ->
                        let name = prop.Name
                        Expr.Call(args.[1], mWriteStartElement, [Expr.Value(name)])
                        |> andThen (Expr.Call(args.[1], mWriteEndElement, [])))
                    |> execute (Expr.Value(()))))
    providedType.AddMember(serializeMethod)

    properties |> providedType.AddMembers

