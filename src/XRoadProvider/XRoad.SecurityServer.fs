namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Security
open System.Text
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
    let utf8WithoutBom = UTF8Encoding(false)

    // Needs better solution to handle server certificates.
    ServicePointManager.ServerCertificateValidationCallback <- Security.RemoteCertificateValidationCallback(fun _ _ _ _ -> true)

    /// Identifies X-Road service provider.
    type ServiceProvider =
        | Member of xRoadInstance: string * memberClass: string * memberCode: string
        | Subsystem of xRoadInstance: string * memberClass: string * memberCode: string * subsystemCode: string
        with
            member this.ObjectId with get() = match this with Member(_) -> "MEMBER" | Subsystem(_) -> "SUBSYSTEM"
            member this.XRoadInstance with get() = match this with Member(v,_,_) | Subsystem(v,_,_,_) -> v
            member this.MemberClass with get() = match this with Member(_,v,_) | Subsystem(_,v,_,_) -> v
            member this.MemberCode with get() = match this with Member(_,_,v) | Subsystem(_,_,v,_) -> v
            member this.SubsystemCode with get() = match this with Member(_) -> None | Subsystem(_,_,_,v) -> Some(v)
            member this.Identifier with get() = sprintf "%s/%s/%s%s" this.XRoadInstance this.MemberClass this.MemberCode (this.SubsystemCode |> Option.fold (fun _ x -> sprintf "/%s" x) "")
            override this.ToString() = sprintf "%s:%s" this.ObjectId this.Identifier
            member this.GetSubsystem(subsystemCode) = match this with Member(a,b,c) -> Subsystem(a,b,c,subsystemCode) | Subsystem(a,b,c,_) -> Subsystem(a,b,c,subsystemCode)

    /// Identifies X-Road service.
    type Service =
        { Provider: ServiceProvider
          ServiceCode: string
          ServiceVersion: string option }
        with
            member this.ObjectId with get() = "SERVICE"
            override this.ToString() = sprintf "%s:%s/%s%s" this.ObjectId this.Provider.Identifier this.ServiceCode (this.ServiceVersion |> Option.fold (fun _ x -> sprintf "/%s" x) "")

    /// Represents single member and its subsystems.
    type Member =
        { Code: string
          Name: string
          Subsystems: string list }

    /// Represents single member class and all producers belonging to that class.
    type MemberClass =
        { Name: string
          Members: Member list }

    type ServiceId =
        { XRoadInstance: string
          MemberClass: string
          MemberCode: string
          SubsystemCode: string option
          ServiceCode: string
          ServiceVersion: string option }

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

    /// High-level function to execute web request against security server.
    let makeWebRequest (serverUri: Uri) writeRequest =
        let request = WebRequest.Create(serverUri) :?> HttpWebRequest
        request.Method <- "POST"
        request.ContentType <- sprintf "text/xml; charset=%s" Encoding.UTF8.HeaderName
        request.Headers.Set("SOAPAction", "")
        writeRequest request
        use response = request.GetResponse()
        use content = response |> MultipartMessage.read |> fst
        content.Position <- 0L
        use reader = new StreamReader(content)
        XDocument.Load(reader)

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

    /// Downloads and parses method list of selected service provider.
    let downloadMethodsList host useHttps (client: ServiceProvider) (service: Service) =
        let serverUri = Uri(sprintf "http%s://%s" (if useHttps then "s" else "") host)
        let doc = makeWebRequest serverUri (fun request ->
            use stream = request.GetRequestStream()
            use streamWriter = new StreamWriter(stream, utf8WithoutBom)
            use writer = XmlWriter.Create(streamWriter)
            writer.WriteStartDocument()
            writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv) // <soapenv:Envelope>
            writer.WriteAttributeString("xmlns", "soapenv", XmlNamespace.Xmlns, XmlNamespace.SoapEnv)
            writer.WriteAttributeString("xmlns", "xrd", XmlNamespace.Xmlns, XmlNamespace.XRoad40)
            writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoad40Id)
            writer.WriteStartElement("Header", XmlNamespace.SoapEnv) // <soapenv:Header>
            writer.WriteStartElement("client", XmlNamespace.XRoad40)
            writer.WriteAttributeString("objectType", XmlNamespace.XRoad40Id, client.ObjectId)
            writer.WriteElementString("xRoadInstance", XmlNamespace.XRoad40Id, client.XRoadInstance)
            writer.WriteElementString("memberClass", XmlNamespace.XRoad40Id, client.MemberClass)
            writer.WriteElementString("memberCode", XmlNamespace.XRoad40Id, client.MemberCode)
            client.SubsystemCode |> Option.iter (fun code -> writer.WriteElementString("subsystemCode", XmlNamespace.XRoad40Id, code))
            writer.WriteEndElement()
            writer.WriteStartElement("service", XmlNamespace.XRoad40)
            writer.WriteAttributeString("objectType", XmlNamespace.XRoad40Id, service.ObjectId)
            writer.WriteElementString("xRoadInstance", XmlNamespace.XRoad40Id, service.Provider.XRoadInstance)
            writer.WriteElementString("memberClass", XmlNamespace.XRoad40Id, service.Provider.MemberClass)
            writer.WriteElementString("memberCode", XmlNamespace.XRoad40Id, service.Provider.MemberCode)
            service.Provider.SubsystemCode |> Option.iter (fun code -> writer.WriteElementString("subsystemCode", XmlNamespace.XRoad40Id, code))
            writer.WriteElementString("serviceCode", XmlNamespace.XRoad40Id, service.ServiceCode)
            service.ServiceVersion |> Option.iter (fun vers -> writer.WriteElementString("serviceVersion", XmlNamespace.XRoad40Id, vers))
            writer.WriteEndElement()
            writer.WriteElementString("id", XmlNamespace.XRoad40, XRoadHelper.generateNonce())
            writer.WriteElementString("protocolVersion", XmlNamespace.XRoad40, "4.0")
            writer.WriteEndElement() // </soapenv:Header>
            writer.WriteStartElement("Body", XmlNamespace.SoapEnv) // <soapenv:Body>
            writer.WriteStartElement("listMethods", XmlNamespace.XRoad40)
            writer.WriteEndElement()
            writer.WriteEndElement() // </soapenv:Body>
            writer.WriteEndElement() // </soapenv:Envelope>
            writer.WriteEndDocument()
            writer.Flush())
        let envelope = doc.Element(xnsname "Envelope" XmlNamespace.SoapEnv)
        let body = envelope.Element(xnsname "Body" XmlNamespace.SoapEnv)
        let fault = body.Element(xnsname "Fault" XmlNamespace.SoapEnv)
        if fault <> null then
            let code = fault.Element(xname "faultcode") |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
            let text = fault.Element(xname "faultstring") |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
            failwithf "Opration resulted with error: FaultCode: %s; FaultString: %s" code text
        body.Element(xnsname "listMethodsResponse" XmlNamespace.XRoad40).Elements(xnsname "service" XmlNamespace.XRoad40)
        |> Seq.map (fun service ->
            { XRoadInstance = service.Element(xnsname "xRoadInstance" XmlNamespace.XRoad40Id).Value
              MemberClass = service.Element(xnsname "memberClass" XmlNamespace.XRoad40Id).Value
              MemberCode = service.Element(xnsname "memberCode" XmlNamespace.XRoad40Id).Value
              SubsystemCode = service.Element(xnsname "subsystemCode" XmlNamespace.XRoad40Id) |> Option.ofObj |> Option.map (fun x -> x.Value)
              ServiceCode = service.Element(xnsname "serviceCode" XmlNamespace.XRoad40Id).Value
              ServiceVersion = service.Element(xnsname "serviceVersion" XmlNamespace.XRoad40Id) |> Option.ofObj |> Option.map (fun x -> x.Value) })
        |> Seq.toList
