
[System.Xml.Serialization.XmlTypeAttribute("base64Binary", Namespace = "http://schemas.xmlsoap.org/soap/encoding/")]
public class BinaryContent
{
    private string @__contentId;
    private System.IO.Stream @__content;

    [System.Xml.Serialization.XmlAttributeAttribute("href", Form = System.Xml.Schema.XmlSchemaForm.Unqualified)]
    public string ContentId { get { return this.@__contentId; } set { this.@__contentId = value; } }

    [System.Xml.Serialization.XmlIgnoreAttribute]
    public System.IO.Stream Content { get { return this.@__content; } set { this.@__content = value; } }

    [System.Xml.Serialization.XmlIgnoreAttribute]
    public string Id { get { return this.@__contentId.StartsWith("cid:") ? this.@__contentId.Substring(4) : this.@__contentId; } }

    [System.Xml.Serialization.XmlTextAttribute]
    public byte[] Value
    {
        get
        {
            using (var temp = new System.IO.MemoryStream())
            {
                this.@__content.Position = 0;
                this.@__content.CopyTo(temp);
                return temp.ToArray();
            }
        }
        set { this.@__content = new System.IO.MemoryStream(value); }
    }

    public BinaryContent()
    { }

    public BinaryContent(System.Collections.Generic.IDictionary<string, System.IO.Stream> attachments, string id)
    {
        this.@__contentId = string.Format("cid:{0}", id);
        this.@__content = attachments[id];
    }
}
