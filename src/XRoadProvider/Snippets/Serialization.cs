
public class XRoadSerializerContext
{
    private bool @__isMultipart;

    private readonly System.Collections.Generic.IDictionary<string, System.IO.Stream> @__attachments = new System.Collections.Generic.Dictionary<string, System.IO.Stream>();

    public System.Collections.Generic.IDictionary<string, System.IO.Stream> Attachments { get { return @__attachments; } }

    public bool IsMultipart { get { return @__isMultipart; } set { @__isMultipart = value; } }

    public void AddAttachments(System.Collections.Generic.IDictionary<string, System.IO.Stream> attachments)
    {
        if (attachments == null)
            return;

        foreach (var kvp in attachments)
            @__attachments.Add(kvp.Key, kvp.Value);
    }
}

public class XRoadXmlWriter : System.Xml.XmlTextWriter
{
    private readonly XRoadSerializerContext @__context;

    public XRoadSerializerContext Context { get { return @__context; } }

    public XRoadXmlWriter(System.IO.TextWriter textWriter, XRoadSerializerContext context)
        : base(textWriter)
    {
        this.@__context = context;
    }
}

public class BinaryContent : System.Xml.Serialization.IXmlSerializable
{
    private string @__contentId;
    private System.IO.Stream @__content;

    public string ContentId { get { return this.@__contentId; } set { this.@__contentId = value; } }

    public System.IO.Stream Content { get { return this.@__content; } set { this.@__content = value; } }

    public BinaryContent()
    { }

    public BinaryContent(System.IO.Stream content) :
        this(null, content)
    { }

    public BinaryContent(string contentId, System.IO.Stream content)
    {
        this.@__contentId = contentId;
        this.@__content = content;
    }

    System.Xml.Schema.XmlSchema System.Xml.Serialization.IXmlSerializable.GetSchema()
    {
        return null;
    }

    void System.Xml.Serialization.IXmlSerializable.ReadXml(System.Xml.XmlReader reader)
    {
        throw new System.NotImplementedException(reader.LocalName);
    }

    void System.Xml.Serialization.IXmlSerializable.WriteXml(System.Xml.XmlWriter w)
    {
        var writer = w as XRoadXmlWriter;
        if (writer == null)
            throw new System.NotImplementedException();

        if (@__content == null)
        {
            writer.WriteAttributeString("nil", "http://www.w3.org/2001/XMLSchema-instance", "true");
            return;
        }

        @__content.Position = 0;

        if (writer.Context.IsMultipart)
        {
            var contentIdBase = @__contentId ?? System.Convert.ToBase64String(System.Security.Cryptography.SHA1.Create().ComputeHash(@__content));
            var contentId = contentIdBase;
            var counter = 0;

            while (writer.Context.Attachments.ContainsKey(contentId))
            {
                counter++;
                contentId = string.Format("{0}{1}", contentIdBase, counter);
            }

            writer.WriteAttributeString("href", string.Format("cid:{0}", contentId));

            writer.Context.Attachments.Add(contentId, @__content);
        }
        else
        {
            const int BUFFER_SIZE = 1000;
            var buffer = new byte[BUFFER_SIZE];
            int bytesRead = 0;

            do
            {
                bytesRead = @__content.Read(buffer, 0, BUFFER_SIZE);
                writer.WriteBase64(buffer, 0, bytesRead);
            } while (BUFFER_SIZE <= bytesRead);
        }
    }
}
