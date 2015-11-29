/*
 * This file contains static helper methods which do not depend on conditional logic
 * implemented in type provider.
 * For that reason it is more convenient to implement these method in literal code
 * instead of CodeDom.
 * Following methods and types are added to AbstractServicePort type.
 */

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
private static XRoad.XRoadXmlReader GetResponseReader(System.Net.WebResponse response)
{
    var boundaryMarker = GetBoundaryMarker(response);
    var responseStream = response.GetResponseStream();
    var context = new XRoad.XRoadSerializerContext();

    // When boundary marker is not present parse as regular post response.
    if (string.IsNullOrEmpty(boundaryMarker))
        return new XRoad.XRoadXmlReader(responseStream, context);

    context.IsMultipart = true;

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
            else context.Attachments.Add(id, targetStream);

            currentLine = CopyContent(responseStream, targetStream, contentDecoder, encoding, contentMarker);

            // Rewind streams to the beginning for user convenience.
            targetStream.Position = 0;
        }
        else currentLine = ReadLineFrom(responseStream, encoding);
    } while (!BufferStartsWith(currentLine, endMarker));

    return new XRoad.XRoadXmlReader(contentStream, context);
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
