namespace XRoadTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System.Reflection
open System.Web.Services.Description
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

    let createXRoadOperation (operation: DesignTime.XRoadOperation) =
        let getParameters (rp: DesignTime.RequestParts) = [
            let rec getParameters (xs: (string * DesignTime.MessagePart) list) = seq {
                match xs with
                | [] -> ()
                | (k,v)::xs ->
                    yield ProvidedParameter(k, typeof<obj>)
                    yield! getParameters xs
            }
            yield! getParameters rp.Body
            yield! getParameters rp.Header
            yield! getParameters rp.MultipartContent
        ]
        let parameters = getParameters operation.Request

        let getReturnType () = [|
            let rec getTypes (xs: (string * DesignTime.MessagePart) list) = seq {
                match xs with
                | [] -> ()
                | (k,v)::xs ->
                    yield typeof<obj>
                    yield! getTypes xs
            }
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
            let ps = args |> Seq.ofList |> Seq.skip 1 |> Seq.map (fun exp -> Expr.Cast<obj> exp :> Expr)
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
                    resolveUri uri
                    |> readServices
                    |> List.map (fun service ->
                        let serviceType = ProvidedTypeDefinition(service.Name, baseType, HideObjectMethods=true)
                        serviceType)
                    |> thisType.AddMembers


                    let typesType = ProvidedTypeDefinition("ServiceTypes", baseType, HideObjectMethods=true)
                    (*
                    for message in description.Messages do
                        let messageType = ProvidedTypeDefinition(message.Name, Some typeof<XRoadEntity>, HideObjectMethods=true)
                        messageType.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XRoadEntity() @@>)))
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
                    thisType.AddMember(typesType)

                    (*
                    for service in description.Services do
                        let serviceType = ProvidedTypeDefinition(service.Name, baseType, HideObjectMethods=true)
                        for port in service.Ports do
                            let portType = ProvidedTypeDefinition(port.Name, Some typeof<XRoadContext>, HideObjectMethods=true)

                            let pAddr = ProvidedProperty("Address", typeof<string>)
                            pAddr.GetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Address @@>)
                            pAddr.SetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Address <- %%args.[1] @@>)
                            portType.AddMember(pAddr)

                            let pProducer = ProvidedProperty("Producer", typeof<string>)
                            pProducer.GetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Producer @@>)
                            pProducer.SetterCode <- (fun args -> <@@ ((%%args.[0]: XRoadContext) :> IXRoadContext).Producer <- %%args.[1] @@>)
                            portType.AddMember(pProducer)

                            let servicePort = Wsdl.parseServicePort port "et"
                            match servicePort.Documentation with | "" -> () | doc -> portType.AddXmlDoc(doc)

                            let fAddr = ProvidedLiteralField("DefaultAddress", typeof<string>, servicePort.Address)
                            fAddr.AddXmlDoc("Default service address defined in WSDL. Overrideable through XRoadSettings.")
                            portType.AddMember(fAddr)

                            let fProducer = ProvidedLiteralField("DefaultProducer", typeof<string>, servicePort.Producer)
                            fProducer.AddXmlDoc("Default producer name defined in WSDL. Overrideable through XRoadSettings.")
                            portType.AddMember(fProducer)

                            let ctor = ProvidedConstructor([])
                            ctor.InvokeCode <- (fun args ->
                                <@@
                                    let this = XRoadContext()
                                    (this :> IXRoadContext).Address <- %%Expr.Value(servicePort.Address)
                                    (this :> IXRoadContext).Producer <- %%Expr.Value(servicePort.Producer)
                                    this
                                @@>)
                            portType.AddMember(ctor)

                            let binding =
                                match port.Binding with
                                | qn when qn.Namespace = description.TargetNamespace -> description.Bindings.[qn.Name]
                                | qn -> failwithf "Bindings defined outside the target namespace are not yet supported (%O)!" qn

                            let bindingStyle =
                                [for ext in binding.Extensions -> ext]
                                |> Seq.choose (fun ext ->
                                    match ext with
                                    | SoapBinding bind ->
                                        let bindStyle =
                                            match bind.Style with
                                            | System.Web.Services.Description.SoapBindingStyle.Rpc -> Some bind.Style
                                            | _ -> Some System.Web.Services.Description.SoapBindingStyle.Document
                                        if not (bind.Transport = "http://schemas.xmlsoap.org/soap/http") then
                                            failwithf "Only HTTP transport for SOAP is accepted (%O)." bind.Transport
                                        Some bindStyle
                                    | _ -> None)
                                |> Seq.exactlyOne
                            let iface =
                                match binding.Type with
                                | qn when qn.Namespace = description.TargetNamespace -> description.PortTypes.[qn.Name]
                                | qn -> failwithf "Port types defined outside the target namespace are not yet supported (%O)!" qn

                            let pt =
                                match [ for pt in description.PortTypes -> pt ] |> List.tryFind (fun x -> x.Name = binding.Type.Name) with
                                | Some pt -> pt
                                | _ -> failwithf "Abstract port type %O not defined." binding.Type

                            [ for op in binding.Operations -> op ]
                            |> List.iter (fun op ->
                                DesignTime.parseOperationDetails description pt op |> createXRoadOperation |> portType.AddMember)

                            portType.AddMember(ProvidedLiteralField("BindingStyle",
                                                                       typeof<System.Web.Services.Description.SoapBindingStyle>,
                                                                       match bindingStyle with
                                                                       | Some x -> x
                                                                       | _ -> System.Web.Services.Description.SoapBindingStyle.Document))

                            serviceType.AddMember portType
                        thisType.AddMember serviceType
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
