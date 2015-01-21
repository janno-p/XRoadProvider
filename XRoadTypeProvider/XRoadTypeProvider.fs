namespace XRoadTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open System.Xml
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Runtime

[<TypeProvider>]
type public XRoadTypeProvider() as this =
    inherit TypeProviderForNamespaces()

    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "XRoadTypeProvider"
    let baseType = Some typeof<obj>
    let staticParams = [ProvidedStaticParameter("uri", typeof<string>)]
    
    let newType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XRoadTypeProvider", baseType)

    let createXRoadOperation (typeCache: Dictionary<XmlReference,ProvidedTypeDefinition>) (operation: Operation) =
        let getParameters (msg: OperationMessage) = [
            let rec getParameters (xs: MessagePart list) fromCache = seq {
                match xs with
                | [] -> ()
                | part::xs ->
                    if fromCache then yield ProvidedParameter(part.Name, match typeCache.TryGetValue(part.Reference) with | true, tp -> tp :> System.Type | _ -> typeof<obj>)
                    else yield ProvidedParameter(part.Name, typeof<obj>)
                    yield! getParameters xs fromCache }
            yield! getParameters msg.Body true
            yield! getParameters msg.Header false
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

        let meth = ProvidedMethod(operation.Name, parameters, returnType)
        meth.InvokeCode <- (fun args ->
            let operationName = match operation.Version with
                                | Some v -> sprintf "%s.%s" operation.Name v
                                | _ -> operation.Name
            let ps = args |> Seq.ofList |> Seq.skip 1 |> Seq.mapi (fun i exp -> match parameters.[i] with
                                                                                | p when p.ParameterType = typeof<obj> -> Expr.Cast<obj> exp :> Expr
                                                                                | _ -> Expr.Coerce(Expr.Cast<XRoadEntity> exp, typeof<obj>))
            let pl = Expr.NewArray(typeof<obj>, ps |> Seq.toList)
            <@@
                use req = new XRoadServiceRequest()
                req.Execute((%%args.[0]: XRoadContext) :> IXRoadContext, operationName, %%pl)
            @@>)
        meth

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
                            tp.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XRoadEntity() @@>)))
                            typeCache.[SchemaType kvp.Key] <- tp
                            tp)
                        |> List.ofSeq
                        |> typeNamespace.AddMembers

                        typeNamespace)
                    |> thisType.AddMembers

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

                            portType.AddMember(ProvidedLiteralField("BindingStyle", typeof<XRoad.XRoadBindingStyle>, port.Style))

                            port.Operations
                            |> List.map (fun op -> op |> createXRoadOperation typeCache)
                            |> portType.AddMembers

                            portType)
                        |> serviceType.AddMembers
                        serviceType)
                    |> thisType.AddMembers

                    (*
                        for part in message.Parts do
                            let tp = match part.Element with
                                     | null -> typeof<obj>
                                     | qn when qn.Name = "" -> typeof<obj>
                                     | qn -> resolveElementType qn description.TargetNamespace
                            let pp = ProvidedProperty(part.Name, tp)
                            pp.GetterCode <- (fun args ->
                                let meth = typeof<XRoadEntity>.GetMethod("GetProperty").MakeGenericMethod(tp)
                                Expr.Call(args.[0], meth, [Expr.Value part.Name]))
                            pp.SetterCode <- (fun args ->
                                let meth = typeof<XRoadEntity>.GetMethod("SetProperty").MakeGenericMethod(tp)
                                Expr.Call(args.[0], meth, [Expr.Value part.Name; args.[1]]))
                            messageType.AddMember(pp)
                        typesType.AddMember(messageType)
                    *)
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
