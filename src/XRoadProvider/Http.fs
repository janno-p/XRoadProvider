module internal XRoadProvider.Http

open System
open System.IO
open System.Net
open System.Text
open System.Xml.Linq
open XRoad

let contentType = sprintf "text/xml; charset=%s" Encoding.UTF8.HeaderName

#if NET40
let private acceptServerCertificate () =
    let originalCallback = ServicePointManager.ServerCertificateValidationCallback
    ServicePointManager.ServerCertificateValidationCallback <- (fun _ _ _ _ -> true)
    { new IDisposable with member __.Dispose() = ServicePointManager.ServerCertificateValidationCallback <- originalCallback }
#endif

let createRequest (uri: Uri)  =
#if NET40
    ServicePointManager.SecurityProtocol <- (3072 |> unbox<SecurityProtocolType>) ||| (786 |> unbox<SecurityProtocolType>) ||| SecurityProtocolType.Tls
#else
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls
#endif
    let request = WebRequest.Create(uri: Uri) |> unbox<HttpWebRequest>
    request.Accept <- "application/xml"
#if !NET40
    if uri.Scheme = "https" then request.ServerCertificateValidationCallback <- (fun _ _ _ _ -> true)
#endif
    request

let downloadFile path uri =
    let request = uri |> createRequest
#if NET40
    use _d = acceptServerCertificate()
#endif
    use response = request.GetResponse()
    use responseStream = response.GetResponseStream()
    use file = File.OpenWrite(path)
    file.SetLength(0L)
    responseStream.CopyTo(file)

let post (stream: Stream) uri =
    let request = uri |> createRequest
#if NET40
    use _d = acceptServerCertificate()
#endif
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

let getXDocument (uri: Uri) =
    if uri.Scheme.ToLower() = "https" then
        let request = uri |> createRequest
#if NET40
        use _d = acceptServerCertificate()
#endif
        use response = request.GetResponse()
        use responseStream = response.GetResponseStream()
        XDocument.Load(responseStream)
    else XDocument.Load(uri.ToString())
