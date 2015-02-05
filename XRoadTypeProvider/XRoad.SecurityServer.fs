module internal XRoad.SecurityServer

open System.Xml
open System.Xml.Linq
open XRoadTypeProvider.Runtime
open XRoadTypeProvider.Wsdl

type Producers =
  { Name: string
    WsdlUri: string
    Description: string }

let discoverProducers serverIP =
    let serverUri = sprintf "http://%s/cgi-bin/consumer_proxy" serverIP

    let doc =
        let request = XRoadRequest.initRequest(serverUri)
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

    let wsdlPrefix =
        match serverUri.EndsWith("/consumer_proxy") with
        | true -> serverUri.Substring(0, serverUri.LastIndexOf("consumer_proxy")) + "uriproxy?producer="
        | _ -> failwith "Invalid XRoad security server uri!"

    let envelope = doc.Elements(XName.Get("Envelope", XmlNamespace.SoapEnvelope)) |> Seq.exactlyOne
    let body = envelope.Elements(XName.Get("Body", XmlNamespace.SoapEnvelope)) |> Seq.exactlyOne
    let message = body.Elements(XName.Get("listProducersResponse", XmlNamespace.XRoad)) |> Seq.exactlyOne
    let response = message.Elements(XName.Get("response"))

    response.Elements(XName.Get("item"))
    |> Seq.map (fun item ->
        let oneValue es = es |> Seq.map (fun (e: XElement) -> e.Value) |> Seq.exactlyOne
        let name = oneValue(item.Elements(XName.Get("name")))
        { Name = name
          WsdlUri = sprintf "http://%s/cgi-bin/uriproxy?producer=%s" serverIP name
          Description = oneValue(item.Elements(XName.Get("description"))) })
    |> List.ofSeq
