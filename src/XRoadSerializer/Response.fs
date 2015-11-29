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
        | "quoted-printable" | "7bit" | "8bit" | "binary" -> None
        | _ -> failwithf "No decoder implemented for content transfer encoding `%s`." contentEncoding

    let startsWith (value: byte []) (buffer: byte []) =
        let rec compare i =
            if value.[i] = buffer.[i] then
                if i = 0 then true else compare (i - 1)
            else false
        if buffer |> isNull || value |> isNull || value.Length > buffer.Length then false
        else compare (value.Length - 1)

    let parseResponse (response: WebResponse) =
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
            let buffer = Array.zeroCreate<byte>(chunkSize)
            let rec copyChunk addNewLine encoding (decoder: (Encoding -> byte[] -> byte[]) option) (contentStream: Stream) =
                let (state,size) = stream |> readChunkOrLine buffer
                if buffer |> startsWith endMarker then false
                elif buffer |> startsWith contentMarker then true
                elif state = ChunkState.EndOfStream then failwith "Unexpected end of multipart stream."
                else
                    if decoder.IsNone && addNewLine then contentStream.Write([| 13uy; 10uy |], 0, 2)
                    let (decodedBuffer,size) = decoder |> Option.fold (fun (buf,_) func -> let buf = buf |> func encoding
                                                                                           (buf,buf.Length)) (buffer,size)
                    contentStream.Write(decodedBuffer, 0, size)
                    match state with EndOfStream -> false | _ -> copyChunk (state = ChunkState.NewLine) encoding decoder contentStream
            let parseContentPart () =
                let headers = stream |> extractMultipartContentHeaders
                let contentId = headers |> Map.tryFind("content-id") |> Option.map (fun x -> x.Trim().Trim('<', '>'))
                let decoder = headers |> Map.tryFind("content-transfer-encoding") |> Option.bind (getDecoder)
                let contentStream = new MemoryStream()
                contents.Add(contentId, upcast contentStream)
                copyChunk false Encoding.UTF8 decoder contentStream
            let rec parseContent () =
                match stream |> readLine with
                | Content -> if parseContentPart() then parseContent() else ()
                | End -> ()
                | Separator -> parseContent()
            parseContent()
            contents |> Seq.toList
        | None -> [(None, response.GetResponseStream())]

    type XmlReader with
        member this.MoveToElement(depth, name, ns) =
            let isElement () = this.Depth = depth && this.LocalName = name && this.NamespaceURI = ns
            let rec findElement () =
                if isElement() then true
                elif this.Read() then
                    if this.Depth < depth then false
                    else findElement()
                else false
            isElement() || findElement()

open Response

type XRoadResponse(response: WebResponse) =
    member __.RetrieveMessage(): XRoadMessage =
        let message = XRoadMessage()
        let reader =
            match Response.parseResponse(response) with
            | [(_,xml)] -> xml
            | (_,xml)::attachments ->
                attachments |> List.iter (fun (id,stream) -> message.Attachments.Add(id.Value, stream))
                xml
            | _ -> failwith "Invalid multipart response message: no content."
            |> XmlReader.Create
        if not (reader.MoveToElement(0, "Envelope", XmlNamespace.SoapEnv)) then
            failwith "Soap envelope element was not found in response message."
        if not (reader.MoveToElement(1, "Body", XmlNamespace.SoapEnv)) then
            failwith "Soap body element was not found in response message."
        message
    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()
