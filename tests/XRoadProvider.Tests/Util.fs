namespace XRoadProvider.Tests

module SoapUtil =
    open XRoad

    let makeSoapResponse content =
        sprintf @"<?xml version=""1.0"" encoding=""utf-8""?>
<soapenv:Envelope xmlns:soapenv=""%s"">
    <soapenv:Body>
        %s
    </soapenv:Body>
</soapenv:Envelope>" XmlNamespace.SoapEnv content

module SerializationUtil =
    open Expecto
    open System
    open System.IO
    open System.Text
    open System.Xml
    open XRoad
    open XRoad.Emitter

    let deserialize (serviceType: Type) context (nm: string) (xml: string) =
        let map = serviceType.GetMethod(nm) |> getMethodMap
        use textReader = new StringReader(xml)
        use reader = XmlReader.Create(textReader)
        while reader.Read() && reader.NodeType <> XmlNodeType.Element do ()
        map.Deserializer.Invoke(reader, context)

    let serialize (serviceType: Type) producerNamespace context nm value =
        let map = serviceType.GetMethod(nm) |> getMethodMap
        use stream = new MemoryStream()
        use sw = new StreamWriter(stream, Encoding.UTF8)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("Body")
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", "tns", XmlNamespace.Xmlns, producerNamespace)
        writer.WriteAttributeString("xmlns", "test", XmlNamespace.Xmlns, "testns")
        map.Serializer.Invoke(writer, value, context)
        writer.WriteEndElement()
        writer.WriteEndDocument()
        writer.Flush()
        stream.Position <- 0L
        use reader = new StreamReader(stream, Encoding.UTF8)
        reader.ReadToEnd()

    let getResponse<'T> serviceType context name xml =
        let response = deserialize serviceType context name xml
        Expect.isTrue (response :? 'T) "wrong result type"
        response |> unbox<'T>
