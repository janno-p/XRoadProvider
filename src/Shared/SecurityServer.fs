namespace XRoad

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text
open System.Xml
open System.Xml.Linq
open Wsdl

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

        doc.Element(xnsname "Envelope" XmlNamespace.SoapEnv)
        |> Option.ofObj
        |> Option.bind (fun x -> x.Element(xnsname "Body" XmlNamespace.SoapEnv) |> Option.ofObj)
        |> Option.bind (fun x -> x.Element(xnsname "Fault" XmlNamespace.SoapEnv) |> Option.ofObj)
        |> Option.map (fun x -> x.Element(xname "faultstring") |> Option.ofObj |> Option.map (fun u -> u.Value) |> MyOption.defaultValue "Could not download producer list from security server")
        |> Option.iter failwith

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
    let downloadCentralServiceList uri instance refresh =
        // Read xml document from file and navigate to root element.
        let doc = Uri(uri, sprintf "listCentralServices?xRoadInstance=%s" instance) |> getFile refresh
        let root = doc.Element(xnsname "centralServiceList" XmlNamespace.XRoad40)
        // Collect data about available central services.
        root.Elements(xnsname "centralService" XmlNamespace.XRoad40)
        |> Seq.map (fun element -> element.Element(xnsname "serviceCode" XmlNamespace.XRoad40Id).Value)
        |> Seq.sortBy (id)
        |> Seq.toList

    let buildRequest (writer: XmlWriter) (writeBody: unit -> unit) (client: ServiceProvider) (service: Service) =
        writer.WriteStartDocument()
        writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "soapenv", XmlNamespace.Xmlns, XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "xro", XmlNamespace.Xmlns, XmlNamespace.XRoad40)
        writer.WriteAttributeString("xmlns", "iden", XmlNamespace.Xmlns, XmlNamespace.XRoad40Id)
        writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
        writer.WriteElementString("protocolVersion", XmlNamespace.XRoad40, "4.0")
        writer.WriteElementString("id", XmlNamespace.XRoad40, XRoadHelper.getUUID())
        writer.WriteStartElement("service", XmlNamespace.XRoad40)
        writer.WriteAttributeString("objectType", XmlNamespace.XRoad40Id, service.ObjectId)
        writer.WriteElementString("xRoadInstance", XmlNamespace.XRoad40Id, service.Provider.XRoadInstance)
        writer.WriteElementString("memberClass", XmlNamespace.XRoad40Id, service.Provider.MemberClass)
        writer.WriteElementString("memberCode", XmlNamespace.XRoad40Id, service.Provider.MemberCode)
        service.Provider.SubsystemCode |> Option.iter (fun code -> writer.WriteElementString("subsystemCode", XmlNamespace.XRoad40Id, code))
        writer.WriteElementString("serviceCode", XmlNamespace.XRoad40Id, service.ServiceCode)
        service.ServiceVersion |> Option.iter (fun version -> writer.WriteElementString("serviceVersion", XmlNamespace.XRoad40Id, version))
        writer.WriteEndElement()
        writer.WriteStartElement("client", XmlNamespace.XRoad40)
        writer.WriteAttributeString("objectType", XmlNamespace.XRoad40Id, client.ObjectId)
        writer.WriteElementString("xRoadInstance", XmlNamespace.XRoad40Id, client.XRoadInstance)
        writer.WriteElementString("memberClass", XmlNamespace.XRoad40Id, client.MemberClass)
        writer.WriteElementString("memberCode", XmlNamespace.XRoad40Id, client.MemberCode)
        client.SubsystemCode |> Option.iter (fun code -> writer.WriteElementString("subsystemCode", XmlNamespace.XRoad40Id, code))
        writer.WriteEndElement()
        writer.WriteEndElement()
        writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
        writeBody()
        writer.WriteEndElement()
        writer.WriteEndElement()
        writer.WriteEndDocument()
        writer.Flush()

    /// Downloads and parses method list of selected service provider.
    let downloadMethodsList uri (client: ServiceProvider) (service: Service) =
        let doc =
            use stream = new MemoryStream()
            use streamWriter = new StreamWriter(stream, utf8WithoutBom)
            use writer = XmlWriter.Create(streamWriter)
            (client, service) ||> buildRequest writer (fun _ -> writer.WriteElementString("listMethods", XmlNamespace.XRoad40))
            Http.post stream uri
        let envelope = doc.Element(xnsname "Envelope" XmlNamespace.SoapEnv)
        let body = envelope.Element(xnsname "Body" XmlNamespace.SoapEnv)
        let fault = body.Element(xnsname "Fault" XmlNamespace.SoapEnv)
        if not (isNull fault) then
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

module MetaServices =
    open XRoad.Serialization.Attributes

    [<XRoadType(LayoutKind.Sequence, IsAnonymous = true)>]
    type GetWsdl () =
        [<XRoadElement("serviceCode", Namespace = XmlNamespace.XRoad40)>] member val ServiceCode = Unchecked.defaultof<string> with get, set
        [<XRoadElement("serviceVersion", Namespace = XmlNamespace.XRoad40)>] member val ServiceVersion = Optional.Option.None<string>() with get, set

    [<XRoadType(LayoutKind.Sequence, IsAnonymous = true)>]
    type GetWsdlResponse () =
        [<XRoadElement("serviceCode", Namespace = XmlNamespace.XRoad40)>] member val ServiceCode = Unchecked.defaultof<string> with get, set
        [<XRoadElement("serviceVersion", Namespace = XmlNamespace.XRoad40)>] member val ServiceVersion = Optional.Option.None<string>() with get, set

    type MetaServicesEndpoint (uri) =
        inherit AbstractEndpointDeclaration (uri)

        [<XRoadOperation("getWsdl", null, XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
        [<XRoadRequiredHeaders(XmlNamespace.XRoad40, "client", "service", "userId", "id", "protocolVersion")>]
        [<XRoadRequest("getWsdl", XmlNamespace.XRoad40)>]
        [<XRoadResponse("getWsdlResponse", XmlNamespace.XRoad40, ReturnType = typeof<GetWsdlResponse>)>]
        member this.GetWsdl(header: XRoadHeader, [<XRoadElementAttribute(MergeContent = true)>] request: GetWsdl) : MultipartResponse<GetWsdlResponse> =
            XRoadUtil.MakeServiceCall(this, "GetWsdl", header, [| request |]) |> unbox

module ForTypes =
    open MetaServices

    let downloadWsdl uri (client: string*string*string*string) (service: string*string*string*string*string*string) : Stream =
        let (clientInstance, clientMemberClass, clientMemberCode, clientSubsystem) = client
        let (serviceInstance, serviceMemberClass, serviceMemberCode, serviceSubsystem, serviceCode, serviceVersion) = service
        let client = XRoadMemberIdentifier(clientInstance, clientMemberClass, clientMemberCode, clientSubsystem)
        let producer = XRoadMemberIdentifier(serviceInstance, serviceMemberClass, serviceMemberCode, serviceSubsystem)
        let serviceVersion = match serviceVersion with "" -> Optional.Option.None<_>() | value -> Optional.Option.Some<_>(value)
        let header = XRoadHeader(Client = client, Producer = producer, ProtocolVersion = "4.0", UserId = "")
        let request = GetWsdl(ServiceCode = serviceCode, ServiceVersion = serviceVersion)
        let endpoint = MetaServicesEndpoint(Uri(uri))
        let response = endpoint.GetWsdl(header, request)
        response.Parts.[0].OpenStream()

    let downloadWsdlString uri (client: string*string*string*string) (service: string*string*string*string*string*string) =
        use stream = downloadWsdl uri client service
        use reader = new System.IO.StreamReader(stream)
        reader.ReadToEnd()

module internal Identifiers =
    let parseMemberIdentifier (value: string) =
        match value.Split([| ':' |], 2) with
        | [| "MEMBER"; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode |] -> XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode)
            | _ -> failwithf "Invalid member identifier: %s" value
        | [| "SUBSYSTEM" ; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode; subsystemCode |] -> XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode)
            | _ -> failwithf "Invalid subsystem identifier: %s" value
        | _ -> failwithf "Invalid owner identifier: %s" value

    let toServiceProvider (value: XRoadMemberIdentifier) =
        match value.SubsystemCode with
        | null | "" -> SecurityServerV6.Member(value.XRoadInstance, value.MemberClass, value.MemberCode)
        | code -> SecurityServerV6.Subsystem(value.XRoadInstance, value.MemberClass, value.MemberCode, code)

    let (|Regex|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some(input) else None

    let parseServiceIdentifier (value: string) =
        match value.Split([| ':' |], 2) with
        | [| "SERVICE"; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode; serviceCode |] ->
                XRoadServiceIdentifier(XRoadInstance = xRoadInstance, MemberClass = memberClass, MemberCode = memberCode, ServiceCode = serviceCode)
            | [| xRoadInstance; memberClass; memberCode; serviceCode; Regex @"^v{\d+}$" serviceVersion |] ->
                XRoadServiceIdentifier(XRoadInstance = xRoadInstance, MemberClass = memberClass, MemberCode = memberCode, ServiceCode = serviceCode, ServiceVersion = serviceVersion)
            | [| xRoadInstance; memberClass; memberCode; subsystemCode; serviceCode |] ->
                XRoadServiceIdentifier(XRoadInstance = xRoadInstance, MemberClass = memberClass, MemberCode = memberCode, SubsystemCode = subsystemCode, ServiceCode = serviceCode)
            | [| xRoadInstance; memberClass; memberCode; subsystemCode; serviceCode; serviceVersion |] ->
                XRoadServiceIdentifier(XRoadInstance = xRoadInstance, MemberClass = memberClass, MemberCode = memberCode, SubsystemCode = subsystemCode, ServiceCode = serviceCode, ServiceVersion = serviceVersion)
            | _ -> failwithf "Invalid member identifier: %s" value
        | _ -> failwithf "Invalid owner identifier: %s" value

    let formatMemberIdentifier (value: XRoadMemberIdentifier) =
        let prefix, subsystem =
            match value.SubsystemCode with
            | null | "" -> "MEMBER", ""
            | code -> "SUBSYSTEM", sprintf "/%s" code
        sprintf "%s:%s/%s/%s%s" prefix value.XRoadInstance value.MemberClass value.MemberCode subsystem

    let formatServiceIdentifier (value: XRoadServiceIdentifier) =
        let subsystem = match value.SubsystemCode with null | "" -> "" | code -> sprintf "/%s" code
        let serviceVersion = match value.ServiceVersion with null | "" -> "" | vers -> sprintf "/%s" vers
        sprintf "SERVICE:%s/%s/%s%s/%s%s" value.XRoadInstance value.MemberClass value.MemberCode subsystem value.ServiceCode serviceVersion
