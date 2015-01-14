module internal XRoadTypeProvider.DesignTime

open System.Collections.Generic
open System.Web.Services.Description
open System.Xml

module XmlNamespace =
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnvelope = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"

let (|XRoadVersion|) (binding: OperationBinding) =
    let versions =
        [for x in binding.Extensions -> x]
        |> List.choose (fun ext -> match ext with | :? XmlElement as e -> Some e | _ -> None)
        |> List.filter (fun e -> e.NamespaceURI = XmlNamespace.XRoad && e.LocalName = "version")
        |> List.map (fun e -> e.InnerText)
    match versions with
    | [] -> None
    | [v] -> Some v
    | _ -> failwithf "Multiple version elements were not expected."

let findMessageDesc (messageName: XmlQualifiedName) (schema: ServiceDescription) =
    if messageName.Namespace <> schema.TargetNamespace then
        failwithf "External messages are not supported yet! [%O]" messageName
    [for x in schema.Messages -> x]
    |> Seq.filter (fun msg -> msg.Name = messageName.Name)
    |> Seq.exactlyOne

let findOperationDesc (port: PortType) (binding: OperationBinding) =
    [for x in port.Operations -> x]
        |> Seq.filter (fun op -> op.Name = binding.Name)
        |> Seq.exactlyOne

let readDocumentation (operationDesc: Operation) =
    [for n in operationDesc.DocumentationElement.ChildNodes -> n]
    |> Seq.filter (fun n -> n.NamespaceURI = XmlNamespace.XRoad && n.LocalName = "title")
    |> Seq.map (fun n ->
        let langAttr = [for a in n.Attributes -> a]
                        |> List.tryFind (fun a -> a.LocalName = "lang" && a.NamespaceURI = "http://www.w3.org/XML/1998/namespace")
        match langAttr with
        | Some a -> (a.Value, n.InnerText)
        | _ -> ("", n.InnerText))
    |> dict

type MessagePart =
    | ElementPart of XmlQualifiedName
    | TypePart of XmlQualifiedName

let getMessageParts (messageDesc: Message) =
    [for p in messageDesc.Parts -> p]
    |> List.map (fun p -> match p.Element, p.Type with
                          | x, y when x.IsEmpty && y.IsEmpty -> failwithf "Unknown element or type for message %s part %s" messageDesc.Name p.Name
                          | x, _ when not x.IsEmpty -> p.Name, ElementPart x
                          | _, x -> p.Name, TypePart x)
    |> List.fold (fun (d: Dictionary<string, MessagePart>) (k, v) -> d.Add(k, v); d) (Dictionary<string, MessagePart>())

type RequestParts =
  { Body: obj list
    Header: obj list
    MultipartContent: obj list }
    static member Empty = { Body = []; Header = []; MultipartContent = [] }

let parseRequest (input: MessageBinding) (message: Message) (schema: ServiceDescription) =
    let rec parseRequestParts rq (exts: obj list) (parts: System.Collections.Generic.IDictionary<string,MessagePart>) =
        match exts with
        | [] -> rq
        | ext::exts ->
            match ext with
            | :? SoapBodyBinding as sbb ->
                match sbb.Parts with
                | null | [||] -> parseRequestParts rq exts parts
                | _ ->
                    let rq = sbb.Parts
                             |> Array.fold (fun rq p ->
                                match parts.TryGetValue p with
                                | false, _ -> failwithf "Message %s does not contain part %s" message.Name p
                                | _, v ->
                                    parts.Remove p |> ignore
                                    { rq with Body = box (p, v) :: rq.Body }) rq
                    parseRequestParts rq exts parts
            | :? XmlElement as e when e.NamespaceURI = XmlNamespace.Soap && e.LocalName = "header" ->
                let msgNameNode = [for a in e.Attributes -> a] |> List.find (fun a -> a.Name = "message")
                let msgName = match msgNameNode.Value.Split(':') with
                              | [|prefix; name|] -> XmlQualifiedName(name, schema.TargetNamespace) // TODO: Cannot find namespace for prefix
                              | [|name|] -> XmlQualifiedName(name, msgNameNode.GetNamespaceOfPrefix(""))
                              | _ -> failwith "never"
                let msg = schema |> findMessageDesc msgName
                let ps = msg |> getMessageParts
                let partNameNode = [for a in e.Attributes -> a] |> List.find (fun a -> a.Name = "part")
                match ps.TryGetValue partNameNode.Value with
                | false, _ -> failwithf "Message %s does not contain part %s" msg.Name partNameNode.Value
                | _, v -> parseRequestParts { rq with Header = box (partNameNode.Value, v) :: rq.Header } exts parts
            | :? SoapHeaderBinding as shb ->
                let msg = schema |> findMessageDesc shb.Message
                let ps = msg |> getMessageParts
                match ps.TryGetValue shb.Part with
                | false, _ -> failwithf "Message %s does not contain part %s" msg.Name shb.Part
                | _, v -> parseRequestParts { rq with Header = box (shb.Part, v) :: rq.Header } exts parts
            | :? MimeMultipartRelatedBinding as mp ->
                let rq = parseRequestParts rq [for p in mp.Parts -> box p] parts
                parseRequestParts rq exts parts
            | :? MimePart as mp ->
                let rq = parseRequestParts rq [for ext in mp.Extensions -> ext] parts
                parseRequestParts rq exts parts
            | :? MimeContentBinding as mc ->
                match parts.TryGetValue mc.Part with
                | false, _ -> failwithf "Message %s does not contain part %s" message.Name mc.Part
                | _, v ->
                    parts.Remove mc.Part |> ignore
                    parseRequestParts { rq with MultipartContent = box (mc.Part, v) :: rq.MultipartContent } exts parts
            | _ -> parseRequestParts rq exts parts
    let parts = getMessageParts message
    let rq = parseRequestParts RequestParts.Empty [for ext in input.Extensions -> ext] parts
    parts |> Seq.fold (fun rq p -> { rq with Body = box (p.Key, p.Value) :: rq.Body }) rq

let parseOperationDetails schema port binding =
    // Abstract
    let operationDesc = findOperationDesc port binding
    let inputDesc = schema |> findMessageDesc operationDesc.Messages.Input.Message
    let outputDesc = schema |> findMessageDesc operationDesc.Messages.Output.Message
    let doc = readDocumentation operationDesc
    let inputParts = getMessageParts inputDesc
    let outputParts = getMessageParts outputDesc

    // Implementation
    let (XRoadVersion operationVersion) = binding
    
    let request = schema |> parseRequest binding.Input inputDesc
    let response = schema |> parseRequest binding.Output outputDesc

    binding.Input



(*

        // Multipart messages have AttachmentCollection as optional member

        let extensions = [for x in op.Input.Extensions -> x]
        if extensions |> List.exists (fun x -> x :? MimeMultipartRelatedBinding) then
            let parameters = [ ProvidedParameter("body", typeof<obj>)
                               ProvidedParameter("file", typeof<Runtime.AttachmentCollection>)
                               ProvidedParameter("settings", typeof<XRoad.XRoadHeader option>, optionalValue=None) ]
            let returnType = typeof<obj * Runtime.AttachmentCollection>
            let operation = ProvidedMethod(op.Name, parameters, returnType)
            operation.InvokeCode <- (fun args ->
//                let op1 () = { BindingStyle = XRoadBindingStyle.DocumentLiteral
//                               QualifiedName = XmlQualifiedName(op.Name, XmlNamespace.XRoad)
//                               Version = "v1" }
                let name = op.Name
                <@@
                    use req = new XRoadServiceRequest()
                    req.Execute((%%args.[0]: XRoadContext) :> IXRoadContext, (name, XmlNamespace.XRoad, "v1"), (%%args.[1]: obj), Some (%%args.[2]: Runtime.AttachmentCollection), (%%args.[3]: XRoad.XRoadHeader option))
                @@>)
            operation
        else
            let parameters = [ ProvidedParameter("body", typeof<obj>)
                               ProvidedParameter("settings", typeof<XRoad.XRoadHeader option>, optionalValue=None) ]
            let returnType = typeof<obj>
            let operation = ProvidedMethod(op.Name, parameters, returnType)
            operation.InvokeCode <- (fun args ->
//                let op1 () = { BindingStyle = XRoadBindingStyle.DocumentLiteral
//                               QualifiedName = XmlQualifiedName(op.Name, XmlNamespace.XRoad)
//                               Version = "v1" }
                let name = op.Name
                <@@
                    use req = new XRoadServiceRequest()
                    req.Execute((%%args.[0]: XRoadContext) :> IXRoadContext, (name, XmlNamespace.XRoad, "v1"), (%%args.[1]: obj), None, (%%args.[2]: XRoad.XRoadHeader option))
                @@>)
            operation
*)
