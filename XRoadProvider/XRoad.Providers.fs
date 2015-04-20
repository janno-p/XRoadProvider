module XRoad.Providers.Impl

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System
open System.Collections.Generic
open System.IO
open System.Reflection

type private CacheKey =
  { TypeName: string
    ProducerUri: string
    UndescribedFaults: bool }

[<TypeProvider>]
type XRoadProducerProvider(config: TypeProviderConfig) as this =
    let invalidation = Event<_,_>()

    let namespaceName = "XRoad.Providers"
    let theAssembly = typeof<XRoadProducerProvider>.Assembly

    let typeCache = Dictionary<_,_>()
    let staticParameters: ParameterInfo [] = [| ProvidedStaticParameter("ProducerUri", typeof<string>)
                                                ProvidedStaticParameter("UndescribedFaults", typeof<bool>, false) |]

    interface ITypeProvider with
        override __.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            //printfn "ITypeProvider.ApplyStaticArguments(%A)" (typeWithoutArguments, typeNameWithArguments, staticArguments)
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition as ty ->
                match staticArguments with
                | [| :? string as producerUri; :? bool as undescribedFaults |] ->
                    let key = { TypeName = String.Join(".", typeNameWithArguments); ProducerUri = producerUri; UndescribedFaults = undescribedFaults }
                    match typeCache.TryGetValue(key) with
                    | false, _ ->
                        let typ = XRoad.ProducerDefinition.makeProducerType(typeNameWithArguments, producerUri, undescribedFaults)
                        typeCache.Add(key, typ)
                        typ
                    | true, typ -> typ
                | _ -> failwith "invalid type provider arguments"
            | _ -> failwith "not implemented"

        override __.GetGeneratedAssemblyContents(assembly) =
            //printfn "ITypeProvider.GetGeneratedAssemblyContents(%A)" assembly
            //printfn "    File.ReadAllBytes(%s)" assembly.ManifestModule.FullyQualifiedName
            File.ReadAllBytes(assembly.ManifestModule.FullyQualifiedName)

        override __.GetInvokerExpression(syntheticMethodBase, parameters) =
            //printfn "ITypeProvider.GetInvokerExpression(%A)" (syntheticMethodBase, parameters)
            let parameters = parameters |> List.ofArray
            match syntheticMethodBase with
            | :? ConstructorInfo as ctor -> Expr.NewObject(ctor, parameters)
            | :? MethodInfo as mi -> Expr.Call(parameters.Head, mi, parameters.Tail)
            | _ -> failwith "not implemented"

        override __.GetNamespaces() =
            //printfn "ITypeProvider.GetNamespaces()"
            [| this |]

        override __.GetStaticParameters(typeWithoutArguments) =
            //printfn "ITypeProvider.GetStaticParameters(%A)" typeWithoutArguments
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition as ty when ty.Name = typeWithoutArguments.Name -> staticParameters
            | _ -> [| |]

        [<CLIEvent>]
        override __.Invalidate =
            //printfn "ITypeProvider.Invalidate"
            invalidation.Publish

        override __.Dispose() =
            //printfn "ITypeProvider.Dispose()"
            ()

    interface IProvidedNamespace with
        override __.GetNestedNamespaces() =
            //printfn "IProvidedNamespace.GetNestedNamespaces()"
            [| |]

        override __.GetTypes() =
            //printfn "IProvidedNamespace.GetTypes()"
            [| ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadProducer", Some(typeof<obj>), IsErased=false) |]

        override __.ResolveTypeName(typeName) =
            //printfn "IProvidedNamespace.ResolveTypeName(%A)" typeName
            null

        override __.NamespaceName
            with get() =
                //printfn "IProvidedNamespace.NamespaceName.get()"
                namespaceName

[<TypeProvider>]
type XRoadProviders(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let theAssembly = typeof<XRoadProviders>.Assembly
    let namespaceName = "XRoad.Providers"
    let baseTy = typeof<obj>

    let serverTy = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadServer", Some baseTy)

    do
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

    do this.AddNamespace(namespaceName, [serverTy])
