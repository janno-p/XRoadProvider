module internal XRoad.SecurityServer

open System.Xml
open System.Xml.Linq

open XRoad.Common

/// Represents single producer information acquired from security server.
type Producer =
    { Name: string
      WsdlUri: string
      Description: string }

/// Executes listProducers service call on specified security server.
/// All available producers are deserialized from response message and returned to caller.
let discoverProducers serverIP =
    let doc =
        // Prepare request message.
        let serverUri = sprintf "http://%s/cgi-bin/consumer_proxy" serverIP
        let request = System.Net.WebRequest.Create(serverUri)
        request.Method <- "POST"
        request.ContentType <- sprintf "text/xml; charset=%s" System.Text.Encoding.UTF8.HeaderName
        request.Headers.Set("SOAPAction", "")

        // Serialize request message content.
        let writeRequest () =
            use stream = request.GetRequestStream() in
            use writer = XmlWriter.Create(stream)
            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnv)

            writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
            writer.WriteStartElement("listProducers", XmlNamespace.XRoad31Ee)
            writer.WriteEndElement()
            writer.WriteEndElement()

            writer.WriteEndElement()
            writer.WriteEndDocument()
        writeRequest()

        // Retrieve and load response message.
        use resp = request.GetResponse()
        use reader = new System.IO.StreamReader(resp.GetResponseStream())
        XDocument.Load(reader)

    // Locate response message main part in XDocument object.
    let envelope = doc.Elements(xnsname "Envelope" XmlNamespace.SoapEnv) |> Seq.exactlyOne
    let body = envelope.Elements(xnsname "Body" XmlNamespace.SoapEnv) |> Seq.exactlyOne
    let message = body.Elements(xnsname "listProducersResponse" XmlNamespace.XRoad31Ee) |> Seq.exactlyOne
    let response = message.Elements(xname "response")

    // Parse all producer elements from XDocument object
    response.Elements(xname "item")
    |> Seq.map (fun item ->
        let oneValue es = es |> Seq.map (fun (e: XElement) -> e.Value) |> Seq.exactlyOne
        let name = oneValue(item.Elements(xname "name"))
        { Name = name
          WsdlUri = sprintf "http://%s/cgi-bin/uriproxy?producer=%s" serverIP name
          Description = oneValue(item.Elements(xname "description")) })
    |> List.ofSeq
