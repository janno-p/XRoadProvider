module internal XRoadProvider.Http

open System
open System.IO
open System.Net
open System.Security.Cryptography.X509Certificates
open System.Text
open System.Xml.Linq
open XRoad

let contentType = sprintf "text/xml; charset=%s" Encoding.UTF8.HeaderName

let loadCertificate uri =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls
    let request = WebRequest.Create(uri: Uri) |> unbox<HttpWebRequest>
    request.Accept <- "application/xml"
    use response = request.GetResponse()
    response.Close()
    request.ServicePoint.Certificate |> Option.ofObj |> Option.map (fun o -> new X509Certificate2(o))

let createRequest (uri, cert) =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls
    let request = WebRequest.Create(uri: Uri) |> unbox<HttpWebRequest>
    request.Accept <- "application/xml"
    cert |> Option.iter (request.ClientCertificates.Add >> ignore)
    request

let downloadFile args path =
    let request = createRequest args
    use response = request.GetResponse()
    use responseStream = response.GetResponseStream()
    use file = File.OpenWrite(path)
    file.SetLength(0L)
    responseStream.CopyTo(file)

let post args (stream: Stream) =
    let request = createRequest args
    request.Method <- "POST"
    request.ContentType <- contentType
    request.Headers.Set("SOAPAction", "")
    use requestStream = request.GetRequestStream()
    stream.Position <- 0L
    stream.CopyTo(requestStream)
    requestStream.Flush()
    requestStream.Close()
    use response = request.GetResponse()
    use contentStream = response |> MultipartMessage.read |> fst
    contentStream.Position <- 0L
    XDocument.Load(contentStream)
