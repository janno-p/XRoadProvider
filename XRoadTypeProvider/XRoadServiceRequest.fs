﻿namespace XRoadTypeProvider

open System
open System.Xml

module XmlNamespace =
    let [<Literal>] SoapEnvelope = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"

type XRoadServiceRequest () =
    member __.Execute(body: obj, attachments: Runtime.AttachmentCollection option, settings: XRoad.XRoadHeader option) =
        let settings = defaultArg settings (XRoad.XRoadHeader())

        let hostName =
            match settings.Producer with
            | Some producer -> producer
            | _ -> "http://localhost/"

        let consumer =
            match settings.Consumer with
            | Some consumer -> consumer
            | _ -> "10239452"

        let req = System.Net.WebRequest.Create(hostName)
        req.Method <- "POST"

        let writeReq () =
            use stream = req.GetRequestStream()
            use writer = XmlWriter.Create(stream)

            let writeHeader name value =
                writer.WriteStartElement(name, XmlNamespace.XRoad)
                match value with
                | Some value -> writer.WriteString(value)
                | _ -> ()
                writer.WriteEndElement()

            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnvelope)
            writer.WriteStartElement("Header", XmlNamespace.SoapEnvelope)
            writer.WriteAttributeString("xmlns", "xrd", null, XmlNamespace.XRoad)
            writeHeader "consumer" (Some (defaultArg settings.Consumer "10239452"))
            writeHeader "producer" (Some (defaultArg settings.Producer "land-cadastre"))
            writeHeader "userId" (Some (defaultArg settings.UserId "EE30101010007"))
            writeHeader "id" (Some (defaultArg settings.Id "3aed1ae3813eb7fbed9396fda70ca1215d3f3fe1"))
            writeHeader "service" (Some (defaultArg settings.Service "land-cadastre.cuAddres.v1"))
            writeHeader "issue" None
            writer.WriteEndElement()
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