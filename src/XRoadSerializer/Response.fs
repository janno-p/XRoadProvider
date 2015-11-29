namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text
open System.Xml

module private Response =
    type ChunkState = Limit | NewLine | EndOfStream

    type PeekStream(stream: Stream) =
        let mutable borrow = None : int option
        member __.Read() =
            match borrow with
            | Some(x) ->
                borrow <- None
                x
            | None -> stream.ReadByte()
        member __.Peek() =
            match borrow with
            | None ->
                let x = stream.ReadByte()
                borrow <- Some(x)
                x
            | Some(x) -> x
        member __.Flush() = stream.Flush()
        interface IDisposable with
            member __.Dispose() =
                stream.Dispose()

    let getBoundaryMarker (response: WebResponse) =
        let parseMultipartContentType (contentType: string) =
            let parts = contentType.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
                        |> List.ofArray
                        |> List.map (fun x -> x.Trim())
            match parts with
            | "multipart/related" :: parts ->
                parts |> List.tryFind (fun x -> x.StartsWith("boundary="))
                      |> Option.map (fun x -> x.Substring(9).Trim('"'))
            | _ -> None
        response
        |> Option.ofObj
        |> Option.map (fun r -> r.ContentType)
        |> Option.bind (parseMultipartContentType)

    let chunkSize = 4096
    let cr = Convert.ToInt32('\r')
    let lf = Convert.ToInt32('\n')

    let readChunkOrLine (buffer: byte []) (stream: PeekStream) =
        let rec addByte pos =
            if pos >= chunkSize then (ChunkState.Limit, pos)
            else
                match stream.Read() with
                | -1 -> (ChunkState.EndOfStream, pos)
                | byt ->
                    if byt = cr && stream.Peek() = lf then
                        stream.Read() |> ignore
                        (ChunkState.NewLine, pos)
                    else
                        buffer.[pos] <- Convert.ToByte(byt)
                        addByte (pos + 1)
        let result = addByte 0
        stream.Flush()
        result

    let readLine stream =
        let mutable line = [| |] : byte []
        let buffer = Array.zeroCreate<byte>(chunkSize)
        let rec readChunk () =
            let (state, chunkSize) = stream |> readChunkOrLine buffer
            Array.Resize(&line, line.Length + chunkSize)
            match state with
            | ChunkState.Limit -> readChunk()
            | ChunkState.EndOfStream
            | ChunkState.NewLine -> ()
        line

    let extractMultipartContentHeaders (stream: PeekStream) =
        let rec getHeaders () = seq {
            match Encoding.ASCII.GetString(stream |> readLine).Trim() with
            | null | "" -> ()
            | line ->
                match line.Split([| ':' |], 2) with
                | [| name |] -> yield (name.Trim(), "")
                | [| name; content |] -> yield (name.Trim(), content.Trim())
                | _ -> failwith "never"
                yield! getHeaders() }
        getHeaders() |> Map.ofSeq

    let base64Decoder (encoding: Encoding) (encodedBytes: byte []) =
        match encodedBytes with
        | null | [| |] -> [| |]
        | _ ->
            let chars = encoding.GetChars(encodedBytes)
            Convert.FromBase64CharArray(chars, 0, chars.Length)

    let getDecoder (contentEncoding: string) =
        match contentEncoding.ToLower() with
        | "base64" -> Some(base64Decoder)
        | "quoted-printable" | "7bit" | "8bit" | "binary" | _ -> None
        | _ -> failwithf "No decoder implemented for content transfer encoding `%s`." contentEncoding

    let startsWith (value: byte []) (buffer: byte []) =
        let rec compare i =
            if value.[i] = buffer.[i] then
                if i = 0 then true else compare (i - 1)
            else false
        if buffer |> isNull || value |> isNull || value.Length > buffer.Length then false
        else compare (value.Length - 1)

    let parseMessage (response: WebResponse) =
        match response |> getBoundaryMarker with
        | Some(boundaryMarker) ->
            use stream = new PeekStream(response.GetResponseStream())
            let contents = List<string option * Stream>()
            let contentMarker = Encoding.ASCII.GetBytes(sprintf "--%s" boundaryMarker)
            let endMarker = Encoding.ASCII.GetBytes(sprintf "--%s--" boundaryMarker)
            let (|Content|End|Separator|) line =
                if line |> startsWith endMarker then End
                elif line |> startsWith contentMarker then Content
                else Separator
            let parseContentPart () =
                let headers = stream |> extractMultipartContentHeaders
                let contentId = headers |> Map.tryFind("content-id") |> Option.map (fun x -> x.Trim().Trim('<', '>'))
                let decoder = headers |> Map.tryFind("content-transfer-encoding") |> Option.bind (getDecoder)
                let contentStream = new MemoryStream()
                contents.Add(contentId, upcast contentStream)

                ()
            let rec parseContent () =
                match stream |> readLine with
                | Content -> parseContentPart()
                             parseContent()
                | End -> ()
                | Separator -> parseContent()
            parseContent()
            match contents |> Seq.toList with
            | xml::attachments ->
                XmlReader.Create(snd xml)
            | _ -> failwith "Invalid multipart response message: no content."
        | None ->
            XmlReader.Create(response.GetResponseStream())

type XRoadResponse(response: WebResponse) =
    member __.RetrieveMessage(): XRoadMessage =
        XRoadMessage()
    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()
