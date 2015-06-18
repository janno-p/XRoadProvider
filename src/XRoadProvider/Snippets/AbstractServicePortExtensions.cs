/*
 * This file contains static helper methods which do not depend on conditional logic
 * implemented in type provider.
 * For that reason it is more convenient to implement these method in literal code
 * instead of CodeDom.
 * Following methods and types are added to AbstractServicePort type.
 */

/// <summary>
/// Enumeration to notify about current state of parsing the stream.
/// </summary>
private enum ChunkState
{
    BufferLimit,
    Marker,
    EndOfStream,
}

/// <summary>
/// Generates random nonce value.
/// Nonce value is sent in X-Road <id /> header element, when user has not provided its own value.
/// </summary>
private static string GenerateNonce()
{
    var nonce = new byte[42];
    var rng = System.Security.Cryptography.RNGCryptoServiceProvider.Create();
    rng.GetNonZeroBytes(nonce);
    return System.Convert.ToBase64String(nonce);
}

/// <summary>
/// Extracts boundary marker value from MIME/multipart header.
/// Returns null if response is not MIME/multipart content-type.
/// </summary>
private static string GetBoundaryMarker(System.Net.WebResponse response)
{
    if (response == null || response.ContentType == null)
        return null;

    // Multipart content type is a string value which starts with `multipart/related`
    // and contains ';'-separated values describing current message.

    var parts = response.ContentType.Split(';');
    if (parts[0].Trim() != "multipart/related")
        return null;

    // Boundary marker is given in format `boundary="boundary-marker-value"`

    for (var i = 1; i < parts.Length; i++)
        if (parts[i].Trim().StartsWith("boundary="))
            return parts[i].Trim().Substring(9).Trim('"');

    return null;
}

/// <summary>
/// Helper method to compare byte buffer content against value.
/// </summary>
private static bool BufferStartsWith(byte[] buffer, byte[] value)
{
    if (buffer == null || value == null || value.Length > buffer.Length)
        return false;

    var position = 0;
    while (value.Length > position && buffer[position] == value[position])
        position += 1;

    return value.Length == position;
}

/// <summary>
/// Helper method to compare byte buffer content against value.
/// </summary>
private static bool BufferEndsWith(byte[] buffer, byte[] value, int? offset = null)
{
    if (buffer == null || value == null)
        return false;

    var bufferPos = offset.GetValueOrDefault(buffer.Length - 1);
    var valuePos = value.Length - 1;

    while (valuePos > -1 && bufferPos >= valuePos && buffer[bufferPos] == value[valuePos])
    {
        bufferPos -= 1;
        valuePos -= 1;
    }

    return valuePos == -1;
}

/// <summary>
/// Helper method to seek certain elements in response messages.
/// </summary>
protected static bool MoveToElement(System.Xml.XmlReader reader, string name, string ns, int depth)
{
    while (true)
    {
        if (reader.Depth == depth && reader.NodeType == System.Xml.XmlNodeType.Element && (name == null || (reader.LocalName == name && reader.NamespaceURI == ns)))
            return true;

        if (!reader.Read() || reader.Depth < depth)
            return false;
    }
}

/// <summary>
/// Splits response message into multipart contents (if present) and returns reader for main content.
/// </summary>
private static System.Xml.XmlReader GetResponseReader(System.Net.WebResponse response, System.Collections.Generic.IDictionary<string,System.IO.Stream> attachments)
{
    var boundaryMarker = GetBoundaryMarker(response);
    var responseStream = response.GetResponseStream();

    // When boundary marker is not present parse as regular post response.
    if (string.IsNullOrEmpty(boundaryMarker))
        return System.Xml.XmlReader.Create(responseStream);

    var encoding = System.Text.Encoding.UTF8;
    var currentLine = (byte[])null;
    var contentMarker = encoding.GetBytes("--" + boundaryMarker);
    var endMarker = encoding.GetBytes("--" + boundaryMarker + "--");

    // Holds main xml message content.
    System.IO.Stream contentStream = null;

    // Extract all multipart content parts into separate streams.
    do
    {
        if (BufferStartsWith(currentLine, contentMarker))
        {
            string id = null;
            System.Func<byte[], System.Text.Encoding, byte[]> contentDecoder = null;

            ExtractMultipartContentHeader(responseStream, encoding, (key, value) => {
                switch (key.ToLower())
                {
                    case "content-id":
                        id = value.Trim().Trim('<', '>');
                        break;
                    case "content-transfer-encoding":
                        contentDecoder = GetContentDecoder(value);
                        break;
                }
            });

            // First part should be content of XML message, according to X-Road specification.
            var targetStream = new System.IO.MemoryStream();
            if (contentStream == null)
                contentStream = targetStream;
            else attachments.Add(id, targetStream);

            currentLine = CopyContent(responseStream, targetStream, contentDecoder, encoding, contentMarker);

            // Rewind streams to the beginning for user convenience.
            targetStream.Position = 0;
        }
        else currentLine = ReadLineFrom(responseStream, encoding);
    } while (!BufferStartsWith(currentLine, endMarker));

    return System.Xml.XmlReader.Create(contentStream);
}

/// <summary>
/// Reads next line from the stream using specified encoding.
/// </summary>
private static byte[] ReadLineFrom(System.IO.Stream stream, System.Text.Encoding encoding)
{
    var marker = encoding.GetBytes("\r\n");

    const int bufferSize = 1024;
    var chunk = new byte[0];

    while (true)
    {
        byte[] buffer;
        var chunkState = ReadChunkOrMarker(out buffer, stream, bufferSize, marker);

        System.Array.Resize(ref chunk, chunk.Length + buffer.Length);
        System.Array.Copy(buffer, chunk, buffer.Length);

        if (chunkState == ChunkState.EndOfStream || chunkState == ChunkState.Marker)
            break;
    }

    return chunk;
}

/// <summary>
/// Reads given number of bytes from the stream.
/// Can return smaller buffer if specified marker is reached.
/// </summary>
private static ChunkState ReadChunkOrMarker(out byte[] chunk, System.IO.Stream stream, int chunkSize, byte[] marker)
{
    var result = ChunkState.BufferLimit;
    var markerLength = marker != null ? marker.Length : 0;
    var buffer = new byte[chunkSize + markerLength];

    var position = -1;
    var markerPos = -1;

    while (true)
    {
        var lastByte = stream.ReadByte();
        if (lastByte == -1)
        {
            result = ChunkState.EndOfStream;
            break;
        }

        position += 1;
        buffer[position] = (byte)lastByte;

        if (marker != null && markerLength > 0)
        {
            markerPos += 1;
            if (lastByte != marker[markerPos])
                markerPos = RewindMarker(markerPos, position, buffer, marker);
        }

        if (position >= chunkSize - 1 && markerPos < 0)
            break;

        if (markerLength < 1 || markerPos != (markerLength - 1))
            continue;

        result = ChunkState.Marker;
        break;
    }

    if (result == ChunkState.Marker)
        position -= markerLength;

    chunk = new byte[position + 1];
    System.Array.Copy(buffer, chunk, position + 1);

    stream.Flush();

    return result;
}

/// <summary>
/// Rewind the marker start position in case new marker started while
/// parsing marker from previous position.
/// </summary>
private static int RewindMarker(int markerPos, int maxPos, byte[] buffer, byte[] marker)
{
    var markerStart = maxPos - markerPos + 1;
    while (true)
    {
        for (; markerStart <= maxPos; markerStart++)
            if (marker[0] == buffer[markerStart])
                break;

        if (markerStart > maxPos)
            return -1;

        var pos = markerStart + 1;
        for (; pos <= maxPos; pos++)
            if (marker[pos - markerStart] != buffer[pos])
                break;

        if (pos > maxPos)
            return maxPos - markerStart;

        markerStart += 1;
    }
}

/// <summary>
/// Read all multipart content header values and return them to caller using action callback.
/// </summary>
private static void ExtractMultipartContentHeader(System.IO.Stream stream, System.Text.Encoding encoding, System.Action<string, string> setValue)
{
    while (true)
    {
        var currentLine = encoding.GetString(ReadLineFrom(stream, encoding)).Replace("\r\n", "").Trim();

        // Empty line marks the end of headers.
        if (string.IsNullOrEmpty(currentLine))
            break;

        var parts = currentLine.Split(new[] { ':' }, 2);
        setValue(parts[0].Trim(), parts.Length > 1 ? parts[1].Trim() : string.Empty);
    }
}

/// <summary>
/// Depending on Content-Transfer-Encoding the content has to be decoded accordingly.
/// </summary>
private static System.Func<byte[], System.Text.Encoding, byte[]> GetContentDecoder(string transferEncoding)
{
    if (string.IsNullOrEmpty(transferEncoding))
        return null;

    switch (transferEncoding.ToLower())
    {
        // Use default decoder (presuming content is not encoded).
        case "quoted-printable":
        case "7bit":
        case "8bit":
        case "binary":
            return null;

        // Support base64 encoded content.
        case "base64":
            return Base64Decoder;

        default:
            throw new System.NotImplementedException("No decoder implemented for `" + transferEncoding + "`.");
    }
}

/// <summary>
/// Decodes base64 encoded byte buffer.
/// </summary>
private static byte[] Base64Decoder(byte[] encodedBytes, System.Text.Encoding encoding)
{
    if (encodedBytes == null || encodedBytes.Length == 0)
        return new byte[0];

    // New line is not part of encoded content, but provides visual appeal.
    var newLine = encoding.GetBytes("\r\n");
    if (BufferEndsWith(encodedBytes, newLine))
        System.Array.Resize(ref encodedBytes, encodedBytes.Length - newLine.Length);

    return encodedBytes.Length == 0 ? new byte[0] : System.Convert.FromBase64CharArray(encoding.GetChars(encodedBytes), 0, encoding.GetCharCount(encodedBytes));
}

/// <summary>
/// Copy multipart content into target stream.
/// Method seeks for multipart content marker to detect end of content part.
/// </summary>
private static byte[] CopyContent(System.IO.Stream responseStream, System.IO.Stream targetStream, System.Func<byte[], System.Text.Encoding, byte[]> contentDecoder, System.Text.Encoding encoding, byte[] marker)
{
    var newLine = encoding.GetBytes("\r\n");

    // Last new line is not part of the content, belongs to content marker.
    // It has to be ignored or files become corrupt for certain encodings.
    var addNewLine = false;

    while (true)
    {
        byte[] buffer;
        var chunkState = ReadChunkOrMarker(out buffer, responseStream, 1024, newLine);

        if (marker != null && BufferStartsWith(buffer, marker))
            return buffer;

        if (marker != null && chunkState == ChunkState.EndOfStream)
            throw new System.IO.EndOfStreamException("Unexpected end of MIME/multipart stream.");

        if (contentDecoder != null)
            buffer = contentDecoder(buffer, encoding);

        if (contentDecoder == null && addNewLine)
            targetStream.Write(newLine, 0, newLine.Length);

        targetStream.Write(buffer, 0, buffer.Length);

        if (chunkState == ChunkState.EndOfStream)
            return buffer;

        addNewLine = chunkState == ChunkState.Marker;
    }
}

private static void SerializeXRoadMessage(System.Net.WebRequest request, System.IO.Stream xml, System.Collections.Generic.IDictionary<string, System.IO.Stream> attachments)
{
    var stream = request.GetRequestStream();
    using (var writer = new System.IO.StreamWriter(stream))
    {
        string boundaryMarker = null;

        // If there are any attachments present, MIME/multipart message has to be sent.
        if (attachments.Count > 0)
        {
            boundaryMarker = System.Guid.NewGuid().ToString();
            request.ContentType = string.Format(@"multipart/related; type=""text/xml""; boundary=""{0}""", boundaryMarker);
            request.Headers.Add("MIME-Version", "1.0");

            writer.WriteLine();
            writer.WriteLine("--{0}", boundaryMarker);
            writer.WriteLine("Content-Type: text/xml; charset=UTF-8");
            writer.WriteLine("Content-Transfer-Encoding: 8bit");
            writer.WriteLine("Content-ID: <XML-{0}>", boundaryMarker);
            writer.WriteLine();
        }

        // Writes XML document itself.
        // In case of MIME/multipart message it's in the first MIME container.
        writer.Flush();
        WriteContent(stream, xml);

        // Write all attachments in separate containers using binary encoding.
        foreach (var attachment in attachments)
        {
            writer.WriteLine();
            writer.WriteLine("--{0}", boundaryMarker);
            writer.WriteLine("Content-Disposition: attachment; filename=notAnswering");
            writer.WriteLine("Content-Type: application/octet-stream");
            writer.WriteLine("Content-Transfer-Encoding: binary");
            writer.WriteLine("Content-ID: <{0}>", attachment.Key);
            writer.WriteLine();

            writer.Flush();
            WriteContent(stream, attachment.Value);
            writer.WriteLine();
        }

        if (attachments.Count > 0)
            writer.WriteLine("--{0}--", boundaryMarker);
    }
}

private static void WriteContent(System.IO.Stream stream, System.IO.Stream content)
{
    int bytesRead = 1000;
    var buffer = new byte[1000];

    content.Position = 0;
    while (bytesRead >= 1000)
    {
        bytesRead = content.Read(buffer, 0, 1000);
        stream.Write(buffer, 0, bytesRead);
    }
}
