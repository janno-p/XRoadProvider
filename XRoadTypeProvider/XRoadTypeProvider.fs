namespace XRoadTypeProvider

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
        operation.IsStaticMethod <- true
        operation.InvokeCode <- (fun _ ->
            <@@
                let req = System.Net.WebRequest.Create("http://localhost/")
                req.Method <- "POST"

                let writeReq () =
                    use stream = req.GetRequestStream()
                    use writer = XmlWriter.Create(stream)
                    writer.WriteStartDocument()
                    writer.WriteEndDocument()

                writeReq()

                use resp = req.GetResponse()
                use reader = new System.IO.StreamReader(resp.GetResponseStream())
                printfn "%A" (reader.ReadToEnd())

                ()
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

                    let (|SoapAddress|_|) (e: obj) =
                        match e with
                        | :? System.Web.Services.Description.SoapAddressBinding as addr -> Some addr.Location
                        | _ -> None

                    let (|SoapBinding|_|) (e: obj) =
                        match e with
                        | :? System.Web.Services.Description.SoapBinding as b -> Some b
                        | _ -> None
                    
                    let (|Producer|_|) (e: obj) =
                        match e with
                        | :? System.Xml.XmlElement as el ->
                            match el.LocalName, el.NamespaceURI with
                            | "address", XRoad.XRoadOldNamespace
                            | "address", XRoad.XRoadNewNamespace ->
                                match [for a in el.Attributes -> a] |> Seq.tryFind (fun a -> a.LocalName = "producer") with
                                | Some a -> Some a.Value
                                | _ -> None
                            | _ -> None
                        | _ -> None

                    let (|XrdTitle|_|) (e: obj) =
                        match e with
                        | :? System.Xml.XmlElement as el ->
                            match el.LocalName, el.NamespaceURI with
                            | "title", XRoad.XRoadOldNamespace
                            | "title", XRoad.XRoadNewNamespace ->
                                match [for a in el.Attributes -> a] |> Seq.tryFind (fun a -> a.LocalName = "lang" && a.NamespaceURI = "http://www.w3.org/XML/1998/namespace") with
                                | Some a -> Some (a.Value, el.InnerText)
                                | _ -> Some ("et", el.InnerText)
                            | _ -> None
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
                            let portType = ProvidedTypeDefinition(port.Name, baseType, HideObjectMethods=true)
                            for ext in port.Extensions do
                                match ext with
                                | SoapAddress addr ->
                                    portType.AddMember(ProvidedLiteralField("Address", typeof<string>, addr))
                                | Producer producer ->
                                    portType.AddMember(ProvidedLiteralField("Producer", typeof<string>, producer))
                                | XrdTitle ("et", value) ->
                                    portType.AddXmlDoc(value)
                                | _ -> ()
                            let binding =
                                match port.Binding with
                                | qn when qn.Namespace = description.TargetNamespace -> description.Bindings.[qn.Name]
                                | qn -> failwithf "Bindings defined outside the target namespace are not yet supported (%O)!" qn
                            let bindingType = ProvidedTypeDefinition("Operations", baseType, HideObjectMethods=true)
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
                            |> List.iter (createXRoadOperation description pt >> bindingType.AddMember)

                            portType.AddMember(ProvidedLiteralField("BindingStyle",
                                                                       typeof<System.Web.Services.Description.SoapBindingStyle>,
                                                                       match bindingStyle with
                                                                       | Some x -> x
                                                                       | _ -> System.Web.Services.Description.SoapBindingStyle.Document))
                            portType.AddMember bindingType
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
