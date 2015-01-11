﻿namespace XRoadTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open System.Reflection
open System.Web.Services.Description
open System.Xml
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.XRoad
open XRoadTypeProvider.TypeBuilder
open XRoadTypeProvider.Runtime

[<TypeProvider>]
type public XRoadTypeProvider() as this =
    inherit TypeProviderForNamespaces()

    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "XRoadTypeProvider"
    let baseType = Some typeof<obj>
    let staticParams = [ProvidedStaticParameter("uri", typeof<string>)]
    
    let newType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XRoadTypeProvider", baseType)

    let createXRoadOperation (wsdl: ServiceDescription) (pt: PortType) (op: OperationBinding) =
        let abstractOp =
            match [ for pto in pt.Operations -> pto ] |> List.tryFind (fun x -> x.Name = op.Name) with
            | None -> failwithf "Abstract part of operation %O not defined." op.Name
            | Some x -> x

        let input =
            let msg = abstractOp.Messages.Input.Message
            match [for m in wsdl.Messages -> m] |> List.tryFind (fun m -> m.Name = msg.Name) with
            | Some m -> m
            | _ -> failwithf "Message %O is not defined." msg
        
        let output =
            let msg = abstractOp.Messages.Output.Message
            match [for m in wsdl.Messages -> m] |> List.tryFind (fun m -> m.Name = msg.Name) with
            | Some m -> m
            | _ -> failwithf "Message %O is not defined." msg

        // Multipart messages have AttachmentCollection as optional member

        let parameters =
            if [for x in op.Input.Extensions -> x] |> List.exists (fun x -> x :? System.Web.Services.Description.MimeMultipartRelatedBinding) then
                [ ProvidedParameter("body", typeof<obj>)
                  ProvidedParameter("file", typeof<Runtime.AttachmentCollection>)
                  ProvidedParameter("settings", typeof<XRoad.XRoadHeader option>, optionalValue=None) ]
            else [ ProvidedParameter("body", typeof<obj>)
                   ProvidedParameter("settings", typeof<XRoad.XRoadHeader option>, optionalValue=None) ]

        let returnType =
            if [for x in op.Output.Extensions -> x] |> List.exists (fun x -> x :? System.Web.Services.Description.MimeMultipartRelatedBinding) then
                typeof<obj * Runtime.AttachmentCollection>
            else
                typeof<obj>

        let operation = ProvidedMethod(op.Name, parameters, returnType)
        operation.InvokeCode <- (fun args ->
            <@@
                use req = new XRoadServiceRequest()
                req.Execute((%%args.[0]: XRoadContext) :> IXRoadContext, (%%args.[1]: obj), None, (%%args.[2]: XRoad.XRoadHeader option))
            @@>)
        operation

    do newType.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = (fun typeName parameterValues ->
            let thisType = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, baseType)
            try
                match parameterValues with
                | [| :? string as uri |] ->
                    let description = uri |> Resolve |> ReadDescription

                    let (|SoapBinding|_|) (e: obj) =
                        match e with
                        | :? System.Web.Services.Description.SoapBinding as b -> Some b
                        | _ -> None
                    
                    let typesType = ProvidedTypeDefinition("ServiceTypes", baseType, HideObjectMethods=true)
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
                    thisType.AddMember(typesType)

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
                            |> List.iter (createXRoadOperation description pt >> portType.AddMember)

                            portType.AddMember(ProvidedLiteralField("BindingStyle",
                                                                       typeof<System.Web.Services.Description.SoapBindingStyle>,
                                                                       match bindingStyle with
                                                                       | Some x -> x
                                                                       | _ -> System.Web.Services.Description.SoapBindingStyle.Document))

                            serviceType.AddMember portType
                            serviceType.AddMember(ProvidedProperty("XRoadContext", typeof<IXRoadContext>, GetterCode=(fun _ -> <@@ null @@>)))
                        thisType.AddMember serviceType
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
