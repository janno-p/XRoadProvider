module XRoadProviders

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.IO

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
                producerAssembly.AddTypes([thisTy])
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
