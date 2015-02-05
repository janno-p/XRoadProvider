module internal XRoad.ProducerDefinition

open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Xml.Linq
open XRoadTypeProvider.Wsdl

let getProducerDefinition(uri, theAssembly, namespacePrefix) =
    let schema = resolveUri uri |> readSchema
    let typeCache = Dictionary<XName,ProvidedTypeDefinition>()

    // Step 1: Populate all global types from schema
    let serviceTypesTy = ProvidedTypeDefinition("ServiceTypes", Some typeof<obj>, IsErased=false)
    schema.TypeSchemas |> List.iter (fun typeSchema ->
        let producerName = 
            match Regex.Match(typeSchema.TargetNamespace.NamespaceName, @"^http://producers\.\w+\.xtee\.riik\.ee/producer/(\w+)$") with
            | m when m.Success -> m.Groups.[1].Value
            | _ -> failwithf "TODO: Implement normal namespace handling for tns: %A" typeSchema.TargetNamespace
        let namespaceTy = ProvidedTypeDefinition(producerName, Some typeof<obj>, IsErased=false)
        serviceTypesTy.AddMember(namespaceTy)
        typeSchema.Types |> Seq.iter (fun kvp ->
            let providedTy = ProvidedTypeDefinition(kvp.Key.LocalName, Some typeof<obj>, IsErased=false)
            namespaceTy.AddMember(providedTy)
            typeCache.Add(kvp.Key, providedTy)))

    [serviceTypesTy]
