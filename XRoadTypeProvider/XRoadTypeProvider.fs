namespace XRoadTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open System.Reflection
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
                        let messageType = ProvidedTypeDefinition(message.Name, Some typeof<XteeEntity>, HideObjectMethods=true)
                        messageType.AddMember(ProvidedConstructor([], InvokeCode=(fun _ -> <@@ XteeEntity() @@>)))
                        for part in message.Parts do
                            let tp = match part.Element with
                                     | null -> typeof<obj>
                                     | qn when qn.Name = "" -> typeof<obj>
                                     | qn -> resolveElementType qn description.TargetNamespace
                            let pp = ProvidedProperty(part.Name, tp)
                            pp.GetterCode <- (fun args ->
                                let meth = typeof<XteeEntity>.GetMethod("GetProperty").MakeGenericMethod(tp)
                                Expr.Call(args.[0], meth, [Expr.Value part.Name]))
                            pp.SetterCode <- (fun args ->
                                let meth = typeof<XteeEntity>.GetMethod("SetProperty").MakeGenericMethod(tp)
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
                            for op in binding.Operations do
                                let meth = ProvidedMethod(op.Name, [], typeof<unit>, IsStaticMethod=true)
                                meth.InvokeCode <- (fun _ -> <@@ () @@>)
                                bindingType.AddMember meth
                            portType.AddMember(ProvidedLiteralField("BindingStyle",
                                                                       typeof<System.Web.Services.Description.SoapBindingStyle>,
                                                                       match bindingStyle with
                                                                       | Some x -> x
                                                                       | _ -> System.Web.Services.Description.SoapBindingStyle.Document))
                            portType.AddMember bindingType
                            serviceType.AddMember portType
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
