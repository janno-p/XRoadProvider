namespace XRoad

open System
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

    let readChunkOrLine (buffer: byte array) (stream: PeekStream) =
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
        let mutable line = [| |] : byte array
        let buffer = Array.zeroCreate<byte>(chunkSize)
        let rec readChunk () =
            let (state, chunkSize) = stream |> readChunkOrLine buffer
            Array.Resize(&line, line.Length + chunkSize)
            match state with
            | ChunkState.Limit -> readChunk()
            | ChunkState.EndOfStream
            | ChunkState.NewLine -> ()
        line

    let rec extractMultipartContentHeader () =
        ()

    let parseMessage (response: WebResponse) =
        let stream = response.GetResponseStream()
        match response |> getBoundaryMarker with
        | Some(boundaryMarker) ->
            let contentMarker = Encoding.ASCII.GetBytes(sprintf "--%s" boundaryMarker)
            let endMarker = Encoding.ASCII.GetBytes(sprintf "--%s" boundaryMarker)
            let rec parseContent () =
                ()
            match parseContent() with
//            | xml::attachments ->
//                XmlReader.Create("")
            | _ -> failwith "Invalid multipart response message: no content."
        | None -> XmlReader.Create(stream)

type XRoadResponse(response: WebResponse) =
    member __.RetrieveMessage(): XRoadMessage =
        XRoadMessage()
    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()
