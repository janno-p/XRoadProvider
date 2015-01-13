namespace XRoadTypeProvider

open System
open System.Xml
open XRoadTypeProvider.Runtime
open XRoadTypeProvider.Wsdl

type XRoadServiceRequest () =
    member __.Execute(context: IXRoadContext, operation, body: obj, attachments: Runtime.AttachmentCollection option, settings: XRoad.XRoadHeader option) =
        let settings = defaultArg settings (XRoad.XRoadHeader())

        let req = System.Net.WebRequest.Create(context.Address)
        req.Method <- "POST"

        let fst (x,_,_) = x
        let trd (_,_,x) = x

        let producer = defaultArg settings.Producer context.Producer
        let serviceName = sprintf "%s.%s.%s" producer (fst operation) (trd operation)

        let writeReq () =
            use stream = req.GetRequestStream()
            use writer = XmlWriter.Create(stream)

            let writerXRoadProperty name value =
                writer.WriteStartElement(name, XmlNamespace.XRoad)
                match value with
                | Some value -> writer.WriteString(value)
                | _ -> ()
                writer.WriteEndElement()

            let writeSoapHeader () =
                writer.WriteStartElement("Header", XmlNamespace.SoapEnvelope)
                writer.WriteAttributeString("xmlns", "xrd", null, XmlNamespace.XRoad)
                writerXRoadProperty "consumer" (Some (defaultArg settings.Consumer "10239452"))
                writerXRoadProperty "producer" (Some producer)
                writerXRoadProperty "userId" (Some (defaultArg settings.UserId "EE30101010007"))
                writerXRoadProperty "id" (Some (defaultArg settings.Id "3aed1ae3813eb7fbed9396fda70ca1215d3f3fe1"))
                writerXRoadProperty "service" (Some serviceName)
                writerXRoadProperty "issue" None
                writer.WriteEndElement()

            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnvelope)
            writeSoapHeader()
            writer.WriteStartElement("Body", XmlNamespace.SoapEnvelope)
            writer.WriteEndElement()
            writer.WriteEndElement()
            writer.WriteEndDocument()

        writeReq()

        use resp = req.GetResponse()
        use reader = new System.IO.StreamReader(resp.GetResponseStream())
        printfn "%A" (reader.ReadToEnd())
        obj()
    interface IDisposable with
        override __.Dispose() = ()
