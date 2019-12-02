namespace ProviderImplementation.ProvidedTypes

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System
open System.Collections.Concurrent
open System.IO
open System.Reflection
open XRoad
open XRoad.Wsdl

(*
* For shorter arguments list we're using string format of X-Road identifiers as type provider static arguments
*
* MEMBER:[X-Road instance]/[member class]/[member code]
*      MEMBER:EE/BUSINESS/123456789
*
* SUBSYSTEM:[subsystem owner]/[subsystem code] where subsystem owner is member identifier without prefix.
*      SUBSYSTEM:EE/BUSINESS/123456789/highsecurity
*
* SERVICE:[service provider]/[service code]/[service version] where service provider is either member or subsystem identifier without prefix and service version part is optional.
*      SERVICE:EE/BUSINESS/123456789/highsecurity/getSecureData/v1
*      SERVICE:EE/BUSINESS/123456789/highsecurity/getSecureData2
* 
* CENTRALSERVICE:[X-Road instance]/[service code]
*      CENTRALSERVICE:EE/populationRegister_personData
*)

/// Generated type providers for X-Road infrastructure.
/// Currently only one type provider is available, which builds service interface for certain producer.
[<TypeProvider>]
type XRoadProducerProvider() as this =
    let invalidation = Event<_,_>()

    let [<Literal>] GenerateTypesUsingServiceDescriptionName = "GenerateTypesUsingServiceDescription"
    let [<Literal>] GenerateTypesUsingMetaServiceName = "GenerateTypesUsingMetaService"
    let [<Literal>] NamespaceName = "XRoad.Providers"

    let theAssembly = this.GetType().Assembly

    // Already generated assemblies
    let typeCache = ConcurrentDictionary<_,Type>()

    // Helper function to convert static parameters
    let toStaticParameterArray paramDef =
        paramDef
        |> List.map (fun (parameter: ProvidedStaticParameter, doc) -> parameter.AddXmlDoc(doc); parameter :> ParameterInfo)
        |> List.toArray

    // Static parameters that are common to both type provider types
    let commonStaticParameters =
        [
            ProvidedStaticParameter("LanguageCode", typeof<string>, "et"), "Specify language code that is extracted as documentation tooltips. Default value is estonian (et)."
            ProvidedStaticParameter("Filter", typeof<string>, ""), "Comma separated list of operations which should be included in definitions. By default, all operations are included."
        ]

    // Available parameters to use for configuring type provider instance
    let serviceDescriptionProducerTypeParameters =
        [
            yield ProvidedStaticParameter("Uri", typeof<string>), "WSDL document location (either local file or network resource)."
            yield! commonStaticParameters
        ]
        |> toStaticParameterArray

    // Available parameters for type provider which generates service interfaces and types through meta services.
    let metaServiceProducerTypeParameters =
        [
            yield ProvidedStaticParameter("SecurityServerUri", typeof<string>), "Security server uri which is used to access X-Road meta services."
            yield ProvidedStaticParameter("ServiceIdentifier", typeof<string>), "Service identifier (in format of `SERVICE:EE/BUSINESS/123456789/highsecurity/getSecureData/v1`)."
            yield ProvidedStaticParameter("ClientIdentifier", typeof<string>), "Client identifier used to access X-Road infrastructure (MEMBER or SUBSYSTEM)."
            yield! commonStaticParameters
        ]
        |> toStaticParameterArray

    interface ITypeProvider with
        /// Called when type alias is created, generates assembly for given arguments.
        override __.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition as ty when ty.Name = GenerateTypesUsingServiceDescriptionName ->
                let uri = unbox<string> staticArguments.[0]
                let languageCode = unbox<string> staticArguments.[1]
                let filter = unbox<string> staticArguments.[2]

                // Same parameter set should have same output, so caching is reasonable.
                let key = (String.Join(".", typeNameWithArguments), uri, languageCode, filter)
                match typeCache.TryGetValue(key) with
                | false, _ ->
                    let operationFilter =
                        match filter with
                        | null -> []
                        | value -> value.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim()) |> List.ofArray
                    // Load schema details from specified file or network location.
                    let schema = ProducerDescription.Load(resolveUri uri, languageCode, operationFilter)
                    typeCache.GetOrAdd(key, (fun _ -> ProducerDefinition.makeProducerType(typeNameWithArguments, schema)))
                | true, typ -> typ
            | :? ProvidedTypeDefinition as ty when ty.Name = GenerateTypesUsingMetaServiceName ->
                let uri = unbox<string> staticArguments.[0]
                let serviceString = unbox<string> staticArguments.[1]
                let clientString = unbox<string> staticArguments.[2]
                let languageCode = unbox<string> staticArguments.[3]
                let filter = unbox<string> staticArguments.[4]

                // Same parameter set should have same output, so caching is reasonable.
                let key = (String.Join(".", typeNameWithArguments), uri, languageCode, filter)
                match typeCache.TryGetValue(key) with
                | false, _ ->
                    let operationFilter =
                        match filter with
                        | null -> []
                        | value -> value.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim()) |> List.ofArray
                    // Load schema details from specified file or network location.
                    let client = let c = clientString |> Identifiers.parseMemberIdentifier in (c.XRoadInstance, c.MemberClass, c.MemberCode, c.SubsystemCode)
                    let service = let s = serviceString |> Identifiers.parseServiceIdentifier in (s.XRoadInstance, s.MemberClass, s.MemberCode, s.SubsystemCode, s.ServiceCode, s.ServiceVersion)
                    use stream = ForTypes.downloadWsdl uri client service
                    let document = System.Xml.Linq.XDocument.Load(stream)
                    let schema = ProducerDescription.Load(document, languageCode, operationFilter)
                    typeCache.GetOrAdd(key, (fun _ -> ProducerDefinition.makeProducerType(typeNameWithArguments, schema)))
                | true, typ -> typ
            | _ -> failwith "not implemented"

        /// Returns contents of assembly generated by type provider instance.
        override __.GetGeneratedAssemblyContents(assembly) =
            File.ReadAllBytes(assembly.ManifestModule.FullyQualifiedName)

        /// Generated types need to handle only instantiation and method call expressions, others are not used.
        override __.GetInvokerExpression(syntheticMethodBase, parameters) =
            let parameters = parameters |> List.ofArray
            match syntheticMethodBase with
            | :? ConstructorInfo as ctor -> Expr.NewObject(ctor, parameters)
            | :? MethodInfo as mi when mi.IsStatic -> Expr.Call(mi, parameters)
            | :? MethodInfo as mi -> Expr.Call(parameters.Head, mi, parameters.Tail)
            | _ -> failwith "not implemented"

        /// Namespaces provided by this type provider instance.
        override __.GetNamespaces() = [| this |]

        /// Exactly one type can have static parameters with current implementation.
        override __.GetStaticParameters(typeWithoutArguments) =
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition as ty when ty.Name = GenerateTypesUsingServiceDescriptionName ->
                serviceDescriptionProducerTypeParameters
            | :? ProvidedTypeDefinition as ty when ty.Name = GenerateTypesUsingMetaServiceName ->
                metaServiceProducerTypeParameters
            | _ -> [| |]

        /// Default implementation for invalidation event.
        [<CLIEvent>]
        override __.Invalidate = invalidation.Publish

        /// No unmanaged resources to deallocate.
        override __.Dispose() = ()

    interface IProvidedNamespace with
        /// No nested namespaces defined.
        override __.GetNestedNamespaces() = [| |]

        /// Type provider contains exactly one abstract type which allows access to type provider functionality.
        override __.GetTypes() =
            let serviceDescriptionProducerType = ProvidedTypeDefinition(theAssembly, NamespaceName, GenerateTypesUsingServiceDescriptionName, Some(typeof<obj>), isErased = false)
            serviceDescriptionProducerType.AddXmlDoc("Type provider for generating service interfaces and data types for specific X-Road producer.")

            let metaServiceProducerType = ProvidedTypeDefinition(theAssembly, NamespaceName, GenerateTypesUsingMetaServiceName, Some(typeof<obj>), isErased = false)
            metaServiceProducerType.AddXmlDoc("Type provider for generating service interfaces and data types for specific X-Road producer using security server meta services (getWsdl).")

            [| serviceDescriptionProducerType; metaServiceProducerType |]

        /// Use default namespace for type provider namespace.
        override __.NamespaceName with get() = NamespaceName

        /// No types have to be resolved.
        override __.ResolveTypeName(_) = null

/// Erased type providers for X-Road infrastructure.
/// Currently only one type provider is available, which acquires list of all producers from
/// security server.
[<TypeProvider>]
type XRoadProviders(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)

    let theAssembly = typeof<XRoadProviders>.Assembly
    let namespaceName = "XRoad.Providers"
    let baseTy = typeof<obj>

    // Main type which provides access to producer list.
    let serverTy =
        let typ = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadServer", Some baseTy)
        typ.AddXmlDoc("Type provider which discovers available producers from specified X-Road security server.")
        typ

    do
        let serverIPParam = ProvidedStaticParameter("ServerIP", typeof<string>)
        serverIPParam.AddXmlDoc("IP address of X-Road security server which is used for producer discovery task.")

        serverTy.DefineStaticParameters(
            [ serverIPParam ],
            fun typeName parameterValues ->
                let thisTy = ProvidedTypeDefinition(theAssembly, namespaceName, typeName, Some baseTy)
                match parameterValues with
                | [| :? string as serverIP |] ->
                    // Create field which holds default service endpoint for the security server.
                    let requestUri = ProvidedField.Literal("RequestUri", typeof<string>, sprintf "http://%s/cgi-bin/consumer_proxy" serverIP)
                    thisTy.AddMember(requestUri)
                    // Create type which holds producer list.
                    let producersTy = ProvidedTypeDefinition("Producers", Some baseTy, HideObjectMethods=true)
                    producersTy.AddXmlDoc("List of available database names registered at the security server.")
                    thisTy.AddMember(producersTy)
                    // Add list of members which each corresponds to certain producer.
                    SecurityServer.discoverProducers(serverIP)
                    |> List.map (fun producer ->
                        let producerTy = ProvidedTypeDefinition(producer.Name, Some baseTy, HideObjectMethods=true)
                        producerTy.AddMember(ProvidedField.Literal("ProducerName", typeof<string>, producer.Name))
                        producerTy.AddMember(ProvidedField.Literal("WsdlUri", typeof<string>, producer.WsdlUri))
                        producerTy.AddXmlDoc(producer.Description)
                        producerTy)
                    |> producersTy.AddMembers
                | _ -> failwith "Unexpected parameter values!"
                thisTy)

    let noteProperty message : MemberInfo =
        let property = ProvidedProperty("<Note>", typeof<string>, getterCode = (fun _ -> <@@ "" @@>), isStatic = true)
        property.AddXmlDoc(message)
        upcast property

    let buildServer6Types (typeName: string) (args: obj []) =
        let securityServerUriString = unbox<string> args.[0]
        let clientIdentifier = unbox<string> args.[1]
        let refresh: bool = unbox args.[2]

        let securityServerUri = Uri(securityServerUriString)

        let client = clientIdentifier |> Identifiers.parseMemberIdentifier
        let xRoadInstance = client.XRoadInstance
        let memberClass = client.MemberClass
        let memberCode = client.MemberCode
        let subsystemCode = client.SubsystemCode

        let thisTy = ProvidedTypeDefinition(theAssembly, namespaceName, typeName, Some baseTy)

        // Type which holds information about producers defined in selected instance.
        let producersTy = ProvidedTypeDefinition("Producers", Some baseTy, HideObjectMethods = true)
        producersTy.AddXmlDoc("All available producers in particular v6 X-Road instance.")
        thisTy.AddMember(producersTy)

        // Type which holds information about central services defined in selected instance.
        let centralServicesTy = ProvidedTypeDefinition("CentralServices", Some baseTy, HideObjectMethods = true)
        centralServicesTy.AddXmlDoc("All available central services in particular v6 X-Road instance.")
        thisTy.AddMember(centralServicesTy)
        
        let identifier = ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic = true, getterCode = (fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) @@>))
        thisTy.AddMember(identifier)

        let identifierString = ProvidedField.Literal("IdentifierString", typeof<string>, clientIdentifier)
        thisTy.AddMember(identifierString)

        let uriField = ProvidedField.Literal("Uri", typeof<string>, securityServerUriString)
        thisTy.AddMember(uriField)

        let instanceField = ProvidedField.Literal("XRoadInstance", typeof<string>, xRoadInstance)
        thisTy.AddMember(instanceField)

        let memberClassField = ProvidedField.Literal("MemberClass", typeof<string>, memberClass)
        thisTy.AddMember(memberClassField)

        let memberCodeField = ProvidedField.Literal("MemberCode", typeof<string>, memberCode)
        thisTy.AddMember(memberCodeField)

        match subsystemCode with
        | null | "" -> ()
        | value ->
            let subsystemCodeField = ProvidedField.Literal("SubsystemCode", typeof<string>, value)
            thisTy.AddMember(subsystemCodeField)

        producersTy.AddMembersDelayed (fun _ ->
            try
                SecurityServerV6.downloadProducerList securityServerUri xRoadInstance refresh
                |> List.map (fun memberClass ->
                    let memberClassName = memberClass.Name
                    let classTy = ProvidedTypeDefinition(memberClass.Name, Some baseTy, HideObjectMethods = true)
                    classTy.AddXmlDoc(memberClass.Name)
                    classTy.AddMember(ProvidedField.Literal("ClassName", typeof<string>, memberClass.Name))
                    classTy.AddMembersDelayed (fun () ->
                        memberClass.Members
                        |> List.map (fun memberItem ->
                            let memberItemCode = memberItem.Code
                            let memberId = SecurityServerV6.Member(xRoadInstance, memberClassName, memberItemCode)
                            let addServices provider =
                                try
                                    let service: SecurityServerV6.Service = { Provider = provider; ServiceCode = "listMethods"; ServiceVersion = None }
                                    let client = client |> Identifiers.toServiceProvider
                                    SecurityServerV6.downloadMethodsList securityServerUri client service
                                    |> List.map (fun x ->
                                        let versionSuffix = x.ServiceVersion |> Option.fold (fun _ x -> sprintf "/%s" x) ""
                                        let serviceName = sprintf "%s%s" x.ServiceCode versionSuffix
                                        let serviceTy = ProvidedTypeDefinition(serviceName, Some baseTy, HideObjectMethods = true)
                                        let serviceService: SecurityServerV6.Service = { Provider = provider; ServiceCode = x.ServiceCode; ServiceVersion = x.ServiceVersion }
                                        let c1 = client.XRoadInstance
                                        let c2 = client.MemberClass
                                        let c3 = client.MemberCode
                                        let c4 = match client.SubsystemCode with Some(u) -> u | _ -> ""
                                        let s1 = serviceService.Provider.XRoadInstance
                                        let s2 = serviceService.Provider.MemberClass
                                        let s3 = serviceService.Provider.MemberCode
                                        let s4 = match serviceService.Provider.SubsystemCode with Some(u) -> u | _ -> ""
                                        let s5 = serviceService.ServiceCode
                                        let s6 = match serviceService.ServiceVersion with Some(u) -> u | _ -> ""
                                        serviceTy.AddMemberDelayed (fun _ -> ProvidedMethod("GetWsdl", [], typeof<string>, invokeCode = (fun _ -> <@@ ForTypes.downloadWsdlString securityServerUriString (c1, c2, c3, c4) (s1, s2, s3, s4, s5, s6) @@>), isStatic = true))
                                        serviceTy.AddMemberDelayed (fun _ -> ProvidedField.Literal("ServiceCode", typeof<string>, x.ServiceCode))
                                        serviceTy.AddMemberDelayed (fun _ -> ProvidedProperty("Identifier", typeof<XRoadServiceIdentifier>, isStatic = true, getterCode = (fun _ -> <@@ XRoadServiceIdentifier(s1, s2, s3, s4, s5, s6) @@>)))

                                        let m = XRoadServiceIdentifier(s1, s2, s3, s4, s5, s6)
                                        serviceTy.AddMemberDelayed (fun _ -> ProvidedField.Literal("IdentifierString", typeof<string>, m |> Identifiers.formatServiceIdentifier))

                                        x.ServiceVersion |> Option.iter (fun versionValue -> serviceTy.AddMemberDelayed (fun _ -> ProvidedField.Literal("ServiceVersion", typeof<string>, versionValue)))
                                        serviceTy :> MemberInfo
                                    )
                                with e -> [noteProperty (e.ToString())]
                            let memberTy = ProvidedTypeDefinition(sprintf "%s (%s)" memberItem.Name memberItem.Code, Some baseTy, HideObjectMethods = true)
                            memberTy.AddXmlDoc(memberItem.Name)
                            memberTy.AddMember(ProvidedField.Literal("Name", typeof<string>, memberItem.Name))
                            memberTy.AddMember(ProvidedField.Literal("Code", typeof<string>, memberItem.Code))
                            memberTy.AddMember(ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic = true, getterCode = (fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClassName, memberItemCode) @@>)))

                            let m = XRoadMemberIdentifier(xRoadInstance, memberClassName, memberItemCode)
                            memberTy.AddMember(ProvidedField.Literal("IdentifierString", typeof<string>, m |> Identifiers.formatMemberIdentifier))

                            memberTy.AddMembersDelayed(fun _ ->
                                match addServices memberId with
                                | [] -> []
                                | services ->
                                    let servicesTy = ProvidedTypeDefinition("Services", Some baseTy, hideObjectMethods = true)
                                    servicesTy.AddMembers services
                                    [ servicesTy ]
                            )

                            memberTy.AddMembersDelayed(fun _ ->
                                match memberItem.Subsystems with
                                | [] -> []
                                | subsystems ->
                                    let subsystemsTy = ProvidedTypeDefinition("Subsystems", Some baseTy, hideObjectMethods = true)
                                    subsystemsTy.AddXmlDoc(sprintf "Subsystems defined for X-Road member %s (%s)." memberItem.Name memberItem.Code)
                                    subsystemsTy.AddMembersDelayed (fun _ ->
                                        subsystems
                                        |> List.map (fun subsystem ->
                                            let subsystemId = memberId.GetSubsystem(subsystem)
                                            let subsystemTy = ProvidedTypeDefinition(subsystem, Some baseTy, hideObjectMethods = true)
                                            subsystemTy.AddXmlDoc (sprintf "Subsystem %s." subsystem)
                                            subsystemTy.AddMember (ProvidedField.Literal("Name", typeof<string>, subsystem))
                                            subsystemTy.AddMember (ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic = true, getterCode = (fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClassName, memberItemCode, subsystem) @@>)))

                                            let m = XRoadMemberIdentifier(xRoadInstance, memberClassName, memberItemCode, subsystem)
                                            subsystemTy.AddMember (ProvidedField.Literal("IdentifierString", typeof<string>, m |> Identifiers.formatMemberIdentifier))

                                            subsystemTy.AddMembersDelayed (fun _ ->
                                                match addServices subsystemId with
                                                | [] -> []
                                                | services ->
                                                    let subsystemServicesTy = ProvidedTypeDefinition("Services", Some baseTy, hideObjectMethods = true)
                                                    subsystemServicesTy.AddXmlDoc(sprintf "Services defined for subsystem %s." subsystem)
                                                    subsystemServicesTy.AddMembers services
                                                    [ subsystemServicesTy ]
                                            )
                                            subsystemTy))
                                    [ subsystemsTy ]
                            )

                            memberTy))
                    classTy :> MemberInfo)
            with e -> [noteProperty (e.ToString())])

        centralServicesTy.AddMembersDelayed (fun _ ->
            try
                match SecurityServerV6.downloadCentralServiceList securityServerUri xRoadInstance refresh with
                | [] -> [noteProperty "No central services are listed in this X-Road instance."]
                | services -> services |> List.map (fun serviceCode -> upcast ProvidedField.Literal(serviceCode, typeof<string>, serviceCode))
            with e -> [noteProperty (e.ToString())])

        thisTy

    let server6Parameters =
        [ ProvidedStaticParameter("SecurityServerUri", typeof<string>), "X-Road security server uri which is used to connect to that X-Road instance."
          ProvidedStaticParameter("ClientIdentifier", typeof<string>), "Client identifier used to access X-Road infrastructure (MEMBER or SUBSYSTEM)."
          ProvidedStaticParameter("ForceRefresh", typeof<bool>, false), "When `true`, forces type provider to refresh data from security server." ]
        |> List.map (fun (parameter,doc) -> parameter.AddXmlDoc(doc); parameter)

    // Generic type for collecting information from selected X-Road instance.
    let server6Ty =
        let typ = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadServer6", Some baseTy)
        typ.AddXmlDoc("Type provider which collects data from selected X-Road instance.")
        typ

    do server6Ty.DefineStaticParameters(server6Parameters, buildServer6Types)
    do this.AddNamespace(namespaceName, [serverTy; server6Ty])
