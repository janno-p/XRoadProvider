namespace XRoad

open System.Collections.Generic
open System.IO
open System.Net
open System.Xml
open System.Xml.Linq

open XRoad.Wsdl

module internal SecurityServer =
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


module internal SecurityServerV6 =
    // Needs better solution to handle server certificates.
    ServicePointManager.ServerCertificateValidationCallback <- Security.RemoteCertificateValidationCallback(fun _ _ _ _ -> true)

    /// Represents single member and its subsystems.
    type Member =
        { Code: string
          Name: string
          Subsystems: string list }

    /// Represents single member class and all producers belonging to that class.
    type MemberClass =
        { Name: string
          Members: Member list }

    /// Remember previously downloaded content in temporary files.
    let cache = Dictionary<string, FileInfo>()

    /// Downloads producer list if not already downloaded previously.
    /// Can be forced to redownload file by `refresh` parameters.
    let getFile uri refresh =
        if refresh || (not (cache.ContainsKey(uri))) then
            let fileName = Path.GetTempFileName()
            use webClient = new WebClient()
            webClient.DownloadFile(uri, fileName)
            cache.[uri] <- FileInfo(fileName)
        XDocument.Load(cache.[uri].OpenRead())

    /// Downloads and parses producer list for X-Road v6 security server.
    let downloadProducerList host instance refresh useHttps =
        // Read xml document from file and navigate to root element.
        let doc = getFile (sprintf "http%s://%s/listClients?xRoadInstance=%s" (if useHttps then "s" else "") host instance) refresh
        let root = doc.Element(xnsname "clientList" XmlNamespace.XRoad40)
        // Data structures to support recomposition to records.
        let subsystems = Dictionary<string * string, ISet<string>>()
        let members = Dictionary<string, ISet<string * string>>()
        // Collect data about members and subsystems.
        root.Elements(xnsname "member" XmlNamespace.XRoad40)
        |> Seq.iter (fun element ->
            let id = element.Element(xnsname "id" XmlNamespace.XRoad40)
            let memberClass = id.Element(xnsname "memberClass" XmlNamespace.XRoad40Id).Value
            let memberCode = id.Element(xnsname "memberCode" XmlNamespace.XRoad40Id).Value
            match id.Attribute(xnsname "objectType" XmlNamespace.XRoad40Id).Value with
            | "MEMBER" ->
                let name = element.Element(xnsname "name" XmlNamespace.XRoad40).Value
                match members.TryGetValue(memberClass) with
                | true, lst -> lst.Add(name, memberCode) |> ignore
                | false, _ -> members.Add(memberClass, new SortedSet<_>([name, memberCode]))
            | "SUBSYSTEM" ->
                let subsystemCode = id.Element(xnsname "subsystemCode" XmlNamespace.XRoad40Id).Value
                match subsystems.TryGetValue((memberClass, memberCode)) with
                | true, lst -> lst.Add(subsystemCode) |> ignore
                | false, _ -> subsystems.Add((memberClass, memberCode), new SortedSet<_>([subsystemCode]))
            | x -> failwithf "Unexpected object type value `%s`." x)
        // Compose records from previously collected data.
        members
        |> Seq.map (fun kvp ->
            { Name = kvp.Key
              Members = kvp.Value
                        |> Seq.map (fun (name,code) ->
                            { Code = code
                              Name = name
                              Subsystems =
                                match subsystems.TryGetValue((kvp.Key, code)) with
                                | true, lst -> lst |> Seq.toList
                                | false, _ -> [] })
                        |> Seq.toList })
        |> Seq.sortBy (fun x -> x.Name)
        |> Seq.toList

    /// Downloads and parses central service list from X-Road v6 security server.
    let downloadCentralServiceList host instance refresh useHttps =
        // Read xml document from file and navigate to root element.
        let doc = getFile (sprintf "http%s://%s/listCentralServices?xRoadInstance=%s" (if useHttps then "s" else "") host instance) refresh
        let root = doc.Element(xnsname "centralServiceList" XmlNamespace.XRoad40)
        // Collect data about available central services.
        root.Elements(xnsname "centralService" XmlNamespace.XRoad40)
        |> Seq.map (fun element -> element.Element(xnsname "serviceCode" XmlNamespace.XRoad40Id).Value)
        |> Seq.sortBy (id)
        |> Seq.toList
