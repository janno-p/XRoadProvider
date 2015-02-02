namespace XRoadTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open System.Xml.Linq
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Wsdl.XsdSchema
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

    let buildDependentTypes (typeSchemas: SchemaNode list) (typeCache: Expressions.TypeCache) =
        typeSchemas |> List.iter (fun schema ->
            schema.Types |> Seq.iter (fun tpi ->
                match typeCache.TryGetValue(SchemaType tpi.Key) with
                | true, typ ->
                    match tpi.Value with
                    | ComplexType(spec) ->
                        match spec.Content with
                        | ComplexContent(spec) ->
                            match spec with
                            | ComplexContentSpec.Extension(spec) ->
                                match typeCache.TryGetValue(XmlReference.SchemaType spec.Base) with
                                | true, ti ->
                                    let ti = { ti with DependentTypes = typ::ti.DependentTypes }
                                    typeCache.[XmlReference.SchemaType spec.Base] <- ti
                                | _ -> ()
                            | _ -> ()
                        | _ -> ()
                    | _ -> ()
                | _ -> ()))

    let buildXRoadEntityTypes typeCache typeSchemas =
        typeCache |> buildDependentTypes typeSchemas

        typeSchemas |> List.iter (fun schema ->
            schema.Elements |> Seq.iter (fun kvp ->
                match kvp.Value with
                | Ref refName -> ()
                | Name typeName -> ()
                | Type typ ->
                    Expressions.addXRoadEntityMembers typeCache.[SchemaElement kvp.Key].Type (Some kvp.Key) typ typeCache)
            schema.Types |> Seq.iter (fun kvp ->
                    Expressions.addXRoadEntityMembers typeCache.[SchemaType kvp.Key].Type (Some kvp.Key) kvp.Value typeCache))

    do newType.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = (fun typeName parameterValues ->
            let thisType = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, baseType)
            try
                match parameterValues with
                | [| :? string as uri |] ->
                    let schema = resolveUri uri |> readSchema

                    let typeCache = Expressions.TypeCache()

                    schema.TypeSchemas
                    |> List.map (fun schema ->
                        let typeName = schema.TargetNamespace.NamespaceName
                        let typeNamespace = ProvidedTypeDefinition(typeName, baseType, HideObjectMethods=true)

                        schema.Elements
                        |> Seq.map (fun kvp ->
                            let refName = sprintf "%s'" kvp.Key.LocalName
                            let tp = ProvidedTypeDefinition(refName, Some typeof<XRoadEntity>, HideObjectMethods=true)
                            tp.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XRoadEntity() @@>)))
                            let serializeMethodParams = [ ProvidedParameter("writer", typeof<System.Xml.XmlWriter>) ]
                            let serializeMethod = ProvidedMethod("Serialize", serializeMethodParams, typeof<unit>)
                            tp.AddMember(serializeMethod)
                            typeCache.[SchemaElement kvp.Key] <- { Name = kvp.Key; Type = tp; DependentTypes = [] }
                            tp)
                        |> List.ofSeq
                        |> typeNamespace.AddMembers

                        schema.Types
                        |> Seq.map (fun kvp ->
                            let nm, ns = kvp.Key.LocalName, kvp.Key.NamespaceName
                            let tp = ProvidedTypeDefinition(kvp.Key.LocalName, Some typeof<XRoadEntity>, HideObjectMethods=true)
                            let serializeMethodParams = [ ProvidedParameter("writer", typeof<System.Xml.XmlWriter>) ]
                            let serializeMethod = ProvidedMethod("Serialize", serializeMethodParams, typeof<unit>)
                            tp.AddMember(serializeMethod)
                            typeCache.[SchemaType kvp.Key] <- { Name = kvp.Key; Type = tp; DependentTypes = [] }
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
                            |> List.map (fun op -> op |> Expressions.createXRoadOperationMethod typeCache)
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
