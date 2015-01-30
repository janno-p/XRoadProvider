namespace XRoadTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open System.Xml.Linq
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Runtime

type RequestFormat =
    | Legacy = 0uy
    | New = 1uy

[<TypeProvider>]
type public XRoadTypeProvider() as this =
    inherit TypeProviderForNamespaces()

    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "XRoadTypeProvider"
    let baseType = Some typeof<obj>
    let staticParams = [ProvidedStaticParameter("uri", typeof<string>)]
    
    let newType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XRoadTypeProvider", baseType)

    let createXRoadOperation (typeCache: Dictionary<XmlReference,ProvidedTypeDefinition>) (operation: Operation) =
        let xthdrs = operation.Request.Header
                     |> List.choose (fun p -> match p with
                                              | IsXteeHeader true when operation.Style = RpcEncoded ->
                                                  Some p.Name
                                              | IsXRoadHeader true when operation.Style = DocLiteral ->
                                                  Some p.Name
                                              | _ -> None)
                     |> Array.ofList

        let getTypeFromCache id =
            match typeCache.TryGetValue(id) with
            | true, tp -> tp :> System.Type
            | _ -> typeof<obj>

        let getParameters (msg: OperationMessage) = [
            let rec getParameters (xs: MessagePart list) fromCache = seq {
                match xs with
                | [] -> ()
                | part::xs ->
                    if fromCache then yield ProvidedParameter(part.Name, getTypeFromCache part.Reference)
                    else yield ProvidedParameter(part.Name, typeof<obj>)
                    yield! getParameters xs fromCache }
            yield! getParameters msg.Body true

            let rec getHeaderParameters (xs: MessagePart list) = seq {
                match xs with
                | [] -> ()
                | (IsXteeHeader true)::xs when operation.Style = RpcEncoded ->
                    yield! getHeaderParameters xs
                | (IsXRoadHeader true)::xs when operation.Style = DocLiteral ->
                    yield! getHeaderParameters xs
                | x::xs -> yield ProvidedParameter(x.Name, typeof<obj>)
                           yield! getHeaderParameters xs
            }
            yield ProvidedParameter("settings", typeof<XRoadHeader>)
            yield! getHeaderParameters msg.Header

            yield! getParameters msg.MultipartContent false
        ]
        let parameters = getParameters operation.Request

        let getReturnType () = [|
            let rec getTypes (xs: MessagePart list) = seq {
                match xs with
                | [] -> ()
                | part::xs -> yield match typeCache.TryGetValue(part.Reference) with | true, tp -> tp :> System.Type | _ -> typeof<obj>
                              yield! getTypes xs }
            yield! getTypes operation.Response.Body
        |]

        let returnType =
            let innerType =
                match getReturnType() with
                | [||] -> typeof<unit>
                | [| tp |] -> tp
                | many -> FSharpType.MakeTupleType many
            if operation.Response.MultipartContent |> List.isEmpty then
                innerType
            else
                let tp = typedefof<Runtime.IXRoadResponseWithAttachments<_>>
                tp.MakeGenericType(innerType)

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
                                             xthdrs,
                                             (%%f: System.Xml.XmlWriter -> unit)) @@>
            | DocLiteral ->
                <@@ XRoadRequest.makeDocumentCall((%%args.[0]: XRoadContext) :> IXRoadContext,
                                                  opName,
                                                  opVer,
                                                  opNs,
                                                  %%pl,
                                                  xthdrs) @@>)
        meth

    let getRuntimeType (typeCache: IDictionary<XmlReference,ProvidedTypeDefinition>) (typeName: XName) =
        match XsdSchema.mapPrimitiveType typeName with
        | Some tp -> tp
        | _ -> match typeCache.TryGetValue(SchemaType typeName) with
               | true, tp -> upcast tp
               | _ -> failwithf "Unknown type %A found." typeName

    let buildXRoadEntityTypes typeCache (typeSchemas: XsdSchema.SchemaNode list) =
        let rec populateTypeMembers (providedType: ProvidedTypeDefinition) (typeDef: XsdSchema.TypeDefinition) =
            match typeDef.ParentType with
            | Some xname ->
                match xname with
                | XsdSchema.SoapEncType "Array" -> () // TODO
                | _ -> providedType.SetBaseType(getRuntimeType typeCache xname)
            | _ -> ()

            let serializeMethod = ProvidedMethod("Serialize", [ ProvidedParameter("writer", typeof<System.Xml.XmlWriter>) ], typeof<unit>)
            serializeMethod.InvokeCode <- (fun _ ->
                let name = providedType.Name
                <@@ printfn "%s" name @@>)
            providedType.AddMember(serializeMethod)

            typeDef.Properties
            |> List.map (fun (nm, tp) ->
                let propType = match tp with
                               | XsdSchema.XmlReference refName -> failwith "never"
                               | XsdSchema.TypeReference typeName ->
                                   getRuntimeType typeCache typeName
                               | XsdSchema.TypeDefinition typeDef ->
                                   let newType = ProvidedTypeDefinition(sprintf "%s'" nm, Some typeof<XRoadEntity>, HideObjectMethods=true)
                                   newType.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XRoadEntity() @@>)))
                                   providedType.AddMember(newType)
                                   typeDef |> populateTypeMembers newType
                                   upcast newType
                let propDef = ProvidedProperty(nm, propType)
                propDef.GetterCode <- (fun args ->
                    let meth =
                        let m = typeof<IXRoadEntity>.GetMethod("GetProperty")
                        match propType with
                        | :? ProvidedTypeDefinition -> m.MakeGenericMethod(typeof<XRoadEntity>)
                        | _ -> m.MakeGenericMethod(propType)
                    Expr.Call(args.[0], meth, [Expr.Value nm]))
                propDef.SetterCode <- (fun args ->
                    let meth =
                        let m = typeof<IXRoadEntity>.GetMethod("SetProperty")
                        match propType with
                        | :? ProvidedTypeDefinition -> m.MakeGenericMethod(typeof<XRoadEntity>)
                        | _ -> m.MakeGenericMethod(propType)
                    Expr.Call(args.[0], meth, [Expr.Value nm; args.[1]]))
                propDef)
            |> providedType.AddMembers
        typeSchemas |> List.iter (fun schema ->
            schema.Elements |> Seq.iter (fun kvp ->
                match kvp.Value with
                | XsdSchema.XmlReference refName -> ()
                | XsdSchema.TypeReference typeName -> ()
                | XsdSchema.TypeDefinition typeDef ->
                    typeDef |> populateTypeMembers typeCache.[SchemaElement kvp.Key])
            schema.Types |> Seq.iter (fun kvp ->
                kvp.Value |> populateTypeMembers typeCache.[SchemaType kvp.Key]))

    do newType.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = (fun typeName parameterValues ->
            let thisType = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, baseType)
            try
                match parameterValues with
                | [| :? string as uri |] ->
                    let schema = resolveUri uri |> readSchema

                    let typeCache = Dictionary<XmlReference,ProvidedTypeDefinition>()

                    schema.TypeSchemas
                    |> List.map (fun schema ->
                        let typeName = schema.TargetNamespace.NamespaceName
                        let typeNamespace = ProvidedTypeDefinition(typeName, baseType, HideObjectMethods=true)

                        schema.Elements
                        |> Seq.map (fun kvp ->
                            let refName = sprintf "%s'" kvp.Key.LocalName
                            let tp = ProvidedTypeDefinition(refName, Some typeof<XRoadEntity>, HideObjectMethods=true)
                            tp.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XRoadEntity() @@>)))
                            typeCache.[SchemaElement kvp.Key] <- tp
                            tp)
                        |> List.ofSeq
                        |> typeNamespace.AddMembers

                        schema.Types
                        |> Seq.map (fun kvp ->
                            let tp = ProvidedTypeDefinition(kvp.Key.LocalName, Some typeof<XRoadEntity>, HideObjectMethods=true)
                            if not <| kvp.Value.IsAbstract then
                                tp.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XRoadEntity() @@>)))
                            typeCache.[SchemaType kvp.Key] <- tp
                            tp)
                        |> List.ofSeq
                        |> typeNamespace.AddMembers

                        typeNamespace)
                    |> thisType.AddMembers

                    schema.TypeSchemas |> buildXRoadEntityTypes typeCache

                    schema.Services
                    |> List.map (fun service ->
                        let serviceType = ProvidedTypeDefinition(service.Name, baseType, HideObjectMethods=true)
                        service.Ports
                        |> List.map (fun port ->
                            let portType = ProvidedTypeDefinition(port.Name, Some typeof<XRoadContext>, HideObjectMethods=true)

                            let addressProperty = ProvidedProperty("Address", typeof<string>)
                            addressProperty.GetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Address @@>)
                            addressProperty.SetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Address <- %%args.[1] @@>)
                            portType.AddMember(addressProperty)

                            let producerProperty = ProvidedProperty("Producer", typeof<string>)
                            producerProperty.GetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Producer @@>)
                            producerProperty.SetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Producer <- %%args.[1] @@>)
                            portType.AddMember(producerProperty)

                            match port.Documentation.TryGetValue "et" with
                            | true, doc -> portType.AddXmlDoc(doc)
                            | _ -> ()

                            let portTypeConstructor = ProvidedConstructor([])
                            portTypeConstructor.InvokeCode <- (fun args ->
                                <@@
                                    let this = XRoadContext()
                                    (this :> IXRoadContext).Address <- %%Expr.Value(port.Address)
                                    (this :> IXRoadContext).Producer <- %%Expr.Value(port.Producer)
                                    this
                                @@>)
                            portType.AddMember(portTypeConstructor)

                            let defaultAddressField = ProvidedLiteralField("DefaultAddress", typeof<string>, port.Address)
                            defaultAddressField.AddXmlDoc("Default service address defined in WSDL. Overrideable through XRoadSettings.")
                            portType.AddMember(defaultAddressField)

                            let defaultProducerField = ProvidedLiteralField("DefaultProducer", typeof<string>, port.Producer)
                            defaultProducerField.AddXmlDoc("Default producer name defined in WSDL. Overrideable through XRoadSettings.")
                            portType.AddMember(defaultProducerField)

                            let formatVersion = match port.Style with
                                                | RpcEncoded -> RequestFormat.Legacy
                                                | DocLiteral -> RequestFormat.New

                            let requestFormatField = ProvidedLiteralField("RequestFormat", typeof<RequestFormat>, formatVersion)
                            portType.AddMember(requestFormatField)

                            port.Operations
                            |> List.map (fun op -> op |> createXRoadOperation typeCache)
                            |> portType.AddMembers

                            portType)
                        |> serviceType.AddMembers
                        serviceType)
                    |> thisType.AddMembers
                | _ -> failwith "unexpected parameter values"
            with
            | e ->
                let msg = e.ToString()
                let noteProperty = ProvidedProperty("<Note>", typeof<string>, IsStatic=true)
                noteProperty.GetterCode <- (fun _ -> <@@ msg @@>)
                noteProperty.AddXmlDoc(msg)
                thisType.AddMember noteProperty
            thisType))

    do this.AddNamespace(rootNamespace, [newType])

[<TypeProviderAssembly>]
do ()
