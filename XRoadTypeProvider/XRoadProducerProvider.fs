namespace XRoad.Providers

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Reflection
open System.Xml
open System.Xml.Linq
open XRoadTypeProvider.Runtime.XRoadRequest
open XRoadTypeProvider.Wsdl

[<TypeProvider>]
type XRoadProducerProvider() as this =
    inherit TypeProviderForNamespaces()

    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "XRoad.Providers"
    let baseTy = typeof<obj>

    let producerProviderType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XRoadProducerProvider", Some baseTy)

    let getListOfProducers uri =
        let request = initRequest(uri)
        (   use stream = request.GetRequestStream() in
            use writer = XmlWriter.Create(stream)
            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnvelope)

            writer.WriteStartElement("Body", XmlNamespace.SoapEnvelope)
            writer.WriteStartElement("listProducers", XmlNamespace.XRoad)
            writer.WriteEndElement()
            writer.WriteEndElement()

            writer.WriteEndElement()
            writer.WriteEndDocument())
        use resp = request.GetResponse()
        use reader = new System.IO.StreamReader(resp.GetResponseStream())
        XDocument.Load(reader)

    let getWsdlPrefix (uri: string) =
        match uri.EndsWith("/consumer_proxy") with
        | true -> uri.Substring(0, uri.LastIndexOf("consumer_proxy")) + "uriproxy?producer="
        | _ -> failwith "Invalid XRoad security server uri!"

    do producerProviderType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("securityServerUri", typeof<string>)],
        instantiationFunction = fun typeName parameterValues ->
            let thisType = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, Some baseTy)
            try
                match parameterValues with
                | [| :? string as uri |] ->
                    let wsdlPrefix = getWsdlPrefix uri

                    let doc = getListOfProducers(uri)
                    let envelope = doc.Elements(XName.Get("Envelope", XmlNamespace.SoapEnvelope)) |> Seq.exactlyOne
                    let body = envelope.Elements(XName.Get("Body", XmlNamespace.SoapEnvelope)) |> Seq.exactlyOne
                    let message = body.Elements(XName.Get("listProducersResponse", XmlNamespace.XRoad)) |> Seq.exactlyOne
                    let response = message.Elements(XName.Get("response"))

                    response.Elements(XName.Get("item"))
                    |> Seq.iter (fun item ->
                        let name = item.Elements(XName.Get("name")) |> Seq.map (fun name -> name.Value) |> Seq.exactlyOne
                        let desc = item.Elements(XName.Get("description")) |> Seq.map (fun desc -> desc.Value) |> Seq.exactlyOne
                        let producerType = ProvidedTypeDefinition(name, Some baseTy, HideObjectMethods=true)
                        producerType.AddXmlDoc(desc)
                        let wsdlLiteral = ProvidedLiteralField("WsdlUri", typeof<string>, (wsdlPrefix+name))
                        wsdlLiteral.AddXmlDoc(desc)
                        producerType.AddMember(wsdlLiteral)
                        let wsdlLiteral = ProvidedLiteralField("ProducerName", typeof<string>, name)
                        producerType.AddMember(wsdlLiteral)
                        thisType.AddMember(producerType))
                | _ -> failwith "Unexpected parameter values!"
            with
            | e ->
                let message = e.ToString()
                let errorProperty = ProvidedProperty("<Exception>",
                                                     typeof<string>,
                                                     IsStatic=true,
                                                     GetterCode=(fun _ -> <@@ message @@>))
                errorProperty.AddXmlDoc(message)
                thisType.AddMember(errorProperty)
            thisType
        )

    do this.AddNamespace(rootNamespace, [producerProviderType])
