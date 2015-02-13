module XRoad.Providers

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System
open System.IO
open System.Reflection

[<TypeProvider>]
type XRoadProducerProvider(config: TypeProviderConfig) as this =
    let invalidation = Event<_,_>()

    let namespaceName = "XRoad.Providers"
    let theAssembly = typeof<XRoadProducerProvider>.Assembly

    interface ITypeProvider with
        override __.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            printfn "ITypeProvider.ApplyStaticArguments(%A)" (typeWithoutArguments, typeNameWithArguments, staticArguments)
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition as ty ->
                match staticArguments with
                | [| :? string as producerUri |] ->
                    XRoad.ProducerDefinitionCodeDom.makeProducerType typeNameWithArguments producerUri
                | _ -> failwith "invalid type provider arguments"
            | _ -> failwith "not implemented"

        override __.GetGeneratedAssemblyContents(assembly) =
            printfn "ITypeProvider.GetGeneratedAssemblyContents(%A)" assembly
            printfn "    File.ReadAllBytes(%s)" assembly.ManifestModule.FullyQualifiedName
            File.ReadAllBytes(assembly.ManifestModule.FullyQualifiedName)

        override __.GetInvokerExpression(syntheticMethodBase, parameters) =
            printfn "ITypeProvider.GetInvokerExpression(%A)" (syntheticMethodBase, parameters)
            match syntheticMethodBase with
            | :? ConstructorInfo as ctor -> Expr.NewObject(ctor, parameters |> List.ofArray)
            | _ -> failwith "not implemented"

        override __.GetNamespaces() =
            printfn "ITypeProvider.GetNamespaces()"
            [| this |]

        override __.GetStaticParameters(typeWithoutArguments) =
            printfn "ITypeProvider.GetStaticParameters(%A)" typeWithoutArguments
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition as ty when ty.Name = typeWithoutArguments.Name ->
                [| ProvidedStaticParameter("ProducerUri", typeof<string>) |]
            | _ -> [| |]

        [<CLIEvent>]
        override __.Invalidate =
            printfn "ITypeProvider.Invalidate"
            invalidation.Publish

        override __.Dispose() =
            printfn "ITypeProvider.Dispose()"
            ()

    interface IProvidedNamespace with
        override __.GetNestedNamespaces() =
            printfn "IProvidedNamespace.GetNestedNamespaces()"
            [| |]

        override __.GetTypes() =
            printfn "IProvidedNamespace.GetTypes()"
            [| ProvidedTypeDefinition(theAssembly, namespaceName, "Test", Some(typeof<obj>), IsErased=false) |]

        override __.ResolveTypeName(typeName) =
            printfn "IProvidedNamespace.ResolveTypeName(%A)" typeName
            failwith "not implemented"

        override __.NamespaceName
            with get() =
                printfn "IProvidedNamespace.NamespaceName.get()"
                namespaceName

[<TypeProvider>]
type XRoadProviders(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let theAssembly = typeof<XRoadProviders>.Assembly
    let namespaceName = "XRoad.Providers"
    let baseTy = typeof<obj>

    let producerTy = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadProducer", Some baseTy, IsErased=false)
    let producerAssembly = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), "dll"))

    let serverTy = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadServer", Some baseTy)

    do
        producerTy.DefineStaticParameters(
            [ProvidedStaticParameter("ProducerUri", typeof<string>)],
            fun typeName parameterValues ->
                let thisTy = ProvidedTypeDefinition(theAssembly, namespaceName, typeName, Some baseTy, IsErased=false)
                match parameterValues with
                | [| :? string as producerUri |] ->
                    thisTy.AddMembers(XRoad.ProducerDefinition.getProducerDefinition(producerUri, theAssembly, namespaceName))
                    producerAssembly.AddTypes([thisTy])
                | _ ->  failwith "Unexpected parameter values!"
                thisTy)

        serverTy.DefineStaticParameters(
            [ProvidedStaticParameter("ServerIP", typeof<string>)],
            fun typeName parameterValues ->
                let thisTy = ProvidedTypeDefinition(theAssembly, namespaceName, typeName, Some baseTy)
                match parameterValues with
                | [| :? string as serverIP |] ->
                    let producersTy = ProvidedTypeDefinition("Producers", Some baseTy, HideObjectMethods=true)
                    producersTy.AddXmlDoc("List of available database names registered at the security server.")
                    thisTy.AddMember(producersTy)
                    XRoad.SecurityServer.discoverProducers(serverIP)
                    |> List.map (fun producer ->
                        let producerTy = ProvidedTypeDefinition(producer.Name, Some baseTy, HideObjectMethods=true)
                        producerTy.AddMember(ProvidedLiteralField("ProducerName", typeof<string>, producer.Name))
                        producerTy.AddMember(ProvidedLiteralField("WsdlUri", typeof<string>, producer.WsdlUri))
                        producerTy.AddXmlDoc(producer.Description)
                        producerTy)
                    |> producersTy.AddMembers
                | _ -> failwith "Unexpected parameter values!"
                thisTy)

    do this.AddNamespace(namespaceName, [producerTy; serverTy])
