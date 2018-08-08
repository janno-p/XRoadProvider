namespace XRoad

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text
open System.Xml
open System.Xml.Linq

module internal SecurityServer =
    /// Represents single producer information acquired from security server.
    type Producer =
        { Name: string
          WsdlUri: string
          Description: string }

    /// Executes listProducers service call on specified security server.
    /// All available producers are deserialized from response message and returned to caller.
    let discoverProducers serverIP =
        let uri = Uri(sprintf "http://%s/cgi-bin/consumer_proxy" serverIP)

        let doc =
            // Serialize request message content.
            use stream = new MemoryStream()
            use writer = XmlWriter.Create(stream)

            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnv)

            writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
            writer.WriteStartElement("listProducers", XmlNamespace.XRoad31Ee)
            writer.WriteEndElement()
            writer.WriteEndElement()

            writer.WriteEndElement()
            writer.WriteEndDocument()
            writer.Flush()

            // Retrieve and load response message.
            uri |> Http.post stream

        // Locate response message main part in XDocument object.
        let envelope = doc.Elements(X.name "Envelope" XmlNamespace.SoapEnv) |> Seq.exactlyOne
        let body = envelope.Elements(X.name "Body" XmlNamespace.SoapEnv) |> Seq.exactlyOne
        let message = body.Elements(X.name "listProducersResponse" XmlNamespace.XRoad31Ee) |> Seq.exactlyOne
        let response = message.Elements(X.lname "response")

        // Parse all producer elements from XDocument object
        response.Elements(X.lname "item")
        |> Seq.map (fun item ->
            let oneValue es = es |> Seq.map (fun (e: XElement) -> e.Value) |> Seq.exactlyOne
            let name = oneValue(item.Elements(X.lname "name"))
            { Name = name
              WsdlUri = sprintf "http://%s/cgi-bin/uriproxy?producer=%s" serverIP name
              Description = oneValue(item.Elements(X.lname "description")) })
        |> List.ofSeq


module internal SecurityServerV6 =
    let utf8WithoutBom = UTF8Encoding(false)

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
            member __.ObjectId with get() = "SERVICE"
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
        with
            member this.WsdlPath =
                StringBuilder().Append(sprintf "wsdl?xRoadInstance=%s" this.XRoadInstance)
                               .Append(sprintf "&memberClass=%s" this.MemberClass)
                               .Append(sprintf "&memberCode=%s" this.MemberCode)
                               .Append(this.SubsystemCode |> Option.fold (fun _ -> sprintf "&subsystemCode=%s") "")
                               .Append(sprintf "&serviceCode=%s" this.ServiceCode)
                               .Append(this.ServiceVersion |> Option.fold (fun _ -> sprintf "&version=%s") "")
                               .ToString()

    /// Remember previously downloaded content in temporary files.
    let cache = ConcurrentDictionary<Uri, FileInfo>()

    /// Downloads producer list if not already downloaded previously.
    /// Can be forced to redownload file by `refresh` parameters.
    let getFile refresh uri =
        let f uri =
            let fileName = Path.GetTempFileName()
            uri |> Http.downloadFile fileName
            FileInfo(fileName)
        let file = if not refresh then cache.GetOrAdd(uri, f) else cache.AddOrUpdate(uri, f, (fun uri _ -> f uri))
        XDocument.Load(file.OpenRead())

    /// Downloads and parses producer list for X-Road v6 security server.
    let downloadProducerList uri instance refresh =
        // Read xml document from file and navigate to root element.
        let doc = Uri(uri, sprintf "listClients?xRoadInstance=%s" instance) |> getFile refresh
        let root = doc.Element(X.name "clientList" XmlNamespace.XRoad40)
        // Data structures to support recomposition to records.
        let subsystems = Dictionary<string * string, ISet<string>>()
        let members = Dictionary<string, ISet<string * string>>()
        // Collect data about members and subsystems.
        root.Elements(X.name "member" XmlNamespace.XRoad40)
        |> Seq.iter (fun element ->
            let id = element.Element(X.name "id" XmlNamespace.XRoad40)
            let memberClass = id.Element(X.name "memberClass" XmlNamespace.XRoad40Id).Value
            let memberCode = id.Element(X.name "memberCode" XmlNamespace.XRoad40Id).Value
            match id.Attribute(X.name "objectType" XmlNamespace.XRoad40Id).Value with
            | "MEMBER" ->
                let name = element.Element(X.name "name" XmlNamespace.XRoad40).Value
                match members.TryGetValue(memberClass) with
                | true, lst -> lst.Add(name, memberCode) |> ignore
                | false, _ -> members.Add(memberClass, new SortedSet<_>([name, memberCode]))
            | "SUBSYSTEM" ->
                let subsystemCode = id.Element(X.name "subsystemCode" XmlNamespace.XRoad40Id).Value
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
    let downloadCentralServiceList uri instance refresh =
        // Read xml document from file and navigate to root element.
        let doc = Uri(uri, sprintf "listCentralServices?xRoadInstance=%s" instance) |> getFile refresh
        let root = doc.Element(X.name "centralServiceList" XmlNamespace.XRoad40)
        // Collect data about available central services.
        root.Elements(X.name "centralService" XmlNamespace.XRoad40)
        |> Seq.map (fun element -> element.Element(X.name "serviceCode" XmlNamespace.XRoad40Id).Value)
        |> Seq.sortBy (id)
        |> Seq.toList

    /// Downloads and parses method list of selected service provider.
    let downloadMethodsList uri (client: ServiceProvider) (service: Service) =
        let doc =
            use stream = new MemoryStream()
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
            writer.WriteElementString("id", XmlNamespace.XRoad40, XRoadHelper.getUUID())
            writer.WriteElementString("protocolVersion", XmlNamespace.XRoad40, "4.0")
            writer.WriteEndElement() // </soapenv:Header>
            writer.WriteStartElement("Body", XmlNamespace.SoapEnv) // <soapenv:Body>
            writer.WriteStartElement("listMethods", XmlNamespace.XRoad40)
            writer.WriteEndElement()
            writer.WriteEndElement() // </soapenv:Body>
            writer.WriteEndElement() // </soapenv:Envelope>
            writer.WriteEndDocument()
            writer.Flush()
            uri |> Http.post stream
        let envelope = doc.Element(X.name "Envelope" XmlNamespace.SoapEnv)
        let body = envelope.Element(X.name "Body" XmlNamespace.SoapEnv)
        let fault = body.Element(X.name "Fault" XmlNamespace.SoapEnv)
        if not (isNull fault) then
            let code = fault.Element(X.lname "faultcode") |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
            let text = fault.Element(X.lname "faultstring") |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
            failwithf "Opration resulted with error: FaultCode: %s; FaultString: %s" code text
        body.Element(X.name "listMethodsResponse" XmlNamespace.XRoad40).Elements(X.name "service" XmlNamespace.XRoad40)
        |> Seq.map (fun service ->
            { XRoadInstance = service.Element(X.name "xRoadInstance" XmlNamespace.XRoad40Id).Value
              MemberClass = service.Element(X.name "memberClass" XmlNamespace.XRoad40Id).Value
              MemberCode = service.Element(X.name "memberCode" XmlNamespace.XRoad40Id).Value
              SubsystemCode = service.Element(X.name "subsystemCode" XmlNamespace.XRoad40Id) |> Option.ofObj |> Option.map (fun x -> x.Value)
              ServiceCode = service.Element(X.name "serviceCode" XmlNamespace.XRoad40Id).Value
              ServiceVersion = service.Element(X.name "serviceVersion" XmlNamespace.XRoad40Id) |> Option.ofObj |> Option.map (fun x -> x.Value) })
        |> Seq.toList
