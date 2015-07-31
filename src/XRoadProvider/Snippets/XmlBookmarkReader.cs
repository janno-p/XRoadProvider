/*
 * https://msdn.microsoft.com/en-us/library/aa302292.aspx
 * Helena Kupkova
 * Microsoft Corporation
 */

private class XmlBookmarkReader : System.Xml.XmlReader, XRoad.IXmlBookmarkReader
{
    //
    // Private types
    //
    class CachedXmlNode
    {
        internal System.Xml.XmlNodeType nodeType;
        internal string name;
        internal string localName;
        internal string prefix;
        internal string namespaceUri;
        internal string value;
        internal int depth;
        internal bool isDefaultOrEmpty;
        internal CachedXmlNode next;
        internal CachedXmlNode attributes;
        internal NamespaceDecl namespacesInScope;

        internal CachedXmlNode(System.Xml.XmlNodeType nodeType, string name, string localName, string prefix, string namespaceUri, string value, int depth, NamespaceDecl namespacesInScope, CachedXmlNode next)
        {
            this.nodeType = nodeType;
            this.name = name;
            this.localName = localName;
            this.prefix = prefix;
            this.namespaceUri = namespaceUri;
            this.value = value;
            this.depth = depth;
            this.namespacesInScope = namespacesInScope;
            this.next = next;

            isDefaultOrEmpty = false;
            this.attributes = null;
        }

        internal CachedXmlNode(System.Xml.XmlReader reader, NamespaceDecl namespacesInScope, CachedXmlNode next) :
            this(reader.NodeType, reader.Name, reader.LocalName, reader.Prefix, reader.NamespaceURI, reader.Value, reader.Depth, namespacesInScope, next)
        {
        }
    }

    class NamespaceDecl
    {
        internal string prefix;
        internal string namespaceUri;
        internal int scopeCount;
        internal NamespaceDecl previousDecl;

        internal NamespaceDecl(string prefix, string namespaceUri, NamespaceDecl previousDecl)
        {
            this.prefix = prefix;
            this.namespaceUri = namespaceUri;
            this.previousDecl = previousDecl;
        }
    }

    //
    // Members
    //
    XRoad.XRoadXmlReader reader;

    CachedXmlNode cachedNodes;

    CachedXmlNode curNode;
    CachedXmlNode curAttrParent;
    CachedXmlNode attributeTextValue;

    System.Collections.Hashtable bookmarks;

    NamespaceDecl currentNamespacesInScope;
    NamespaceDecl nextNamespacesInScope;

    public XRoad.XRoadSerializerContext Context { get { return reader.Context; } }
    public XRoad.XRoadXmlReader Reader { get { return reader; } }

    //
    // Constants
    //
    internal const int MaxNodePoolCount = 128;

    //
    // Constructor
    //
    public XmlBookmarkReader(XRoad.XRoadXmlReader reader)
    {
        this.reader = reader;

        // initialize namespaces
        System.Xml.XmlNameTable nt = reader.NameTable;
        currentNamespacesInScope = new NamespaceDecl(nt.Add("xml"), nt.Add("http://www.w3.org/XML/1998/namespace"), null);
        currentNamespacesInScope = new NamespaceDecl(nt.Add("xmlns"), nt.Add("http://www.w3.org/2000/xmlns/"), currentNamespacesInScope);
        currentNamespacesInScope = new NamespaceDecl(nt.Add(string.Empty), nt.Add(string.Empty), currentNamespacesInScope);
        currentNamespacesInScope.scopeCount++;
        nextNamespacesInScope = currentNamespacesInScope;

        bookmarks = new System.Collections.Hashtable();
    }

    //
    // XmlReader methods and properties
    //

    public override System.Xml.XmlNodeType NodeType
    {
        get
        {
            return curNode != null ? curNode.nodeType : reader.NodeType;
        }
    }

    public override string Name
    {
        get
        {
            if (curNode == null)
            {
                return reader.Name;
            }
            else
            {
                return curNode.name;
            }
        }
    }

    public override string LocalName
    {
        get
        {
            return curNode != null ? curNode.localName : reader.LocalName;
        }
    }

    public override string NamespaceURI
    {
        get
        {
            return curNode != null ? curNode.namespaceUri : reader.NamespaceURI;
        }
    }

    public override string Prefix
    {
        get
        {
            return curNode != null ? curNode.prefix : reader.Prefix;
        }
    }

    public override bool HasValue
    {
        get
        {
            if (curNode == null)
            {
                return reader.HasValue;
            }
            else
            {
                return curNode.nodeType != System.Xml.XmlNodeType.Element &&
                        curNode.nodeType != System.Xml.XmlNodeType.EntityReference &&
                        curNode.nodeType != System.Xml.XmlNodeType.EndEntity;
            }
        }
    }

    public override string Value
    {
        get
        {
            return curNode != null ? curNode.value : reader.Value;
        }
    }

    public override int Depth
    {
        get
        {
            return curNode != null ? curNode.depth : reader.Depth;
        }
    }

    public override string BaseURI
    {
        get
        {
            return reader.BaseURI;
        }
    }

    public override bool IsEmptyElement
    {
        get
        {
            if (curNode == null)
            {
                return reader.IsEmptyElement;
            }
            else
            {
                return curNode.nodeType == System.Xml.XmlNodeType.Element && curNode.isDefaultOrEmpty;
            }
        }
    }

    public override bool IsDefault
    {
        get
        {
            if (curNode == null)
            {
                return reader.IsDefault;
            }
            else
            {
                return curNode.nodeType == System.Xml.XmlNodeType.Attribute && curNode.isDefaultOrEmpty;
            }
        }
    }

    public override char QuoteChar
    {
        get
        {
            return reader.QuoteChar;
        }
    }

    public override System.Xml.XmlSpace XmlSpace
    {
        get
        {
            return reader.XmlSpace;
        }
    }

    public override string XmlLang
    {
        get
        {
            return reader.XmlLang;
        }
    }

    public override int AttributeCount
    {
        get
        {
            if (curNode == null)
            {
                return reader.AttributeCount;
            }
            CachedXmlNode attr = curNode.attributes;
            int attrCount = 0;
            while (attr != null)
            {
                attr = attr.next;
                attrCount++;
            }
            return attrCount;
        }
    }

    public override string GetAttribute(string name)
    {
        // forward to base reader if replaying
        if (curNode == null)
        {
            return reader.GetAttribute(name);
        }
        // check that there are some cached attributes
        if (curAttrParent != null && curAttrParent.attributes != null)
        {
            // atomize the name
            name = reader.NameTable.Get(name);

            // if the name is not in name table, there is no attribute with such name -> return 
            if (name == null)
            {
                return null;
            }
            // iterate through attributes fo find the one with the given name
            CachedXmlNode attr = curAttrParent.attributes;
            do
            {
                if ((object)name == (object)attr.name)
                {
                    // found one -> return value
                    return attr.value;
                }
                attr = attr.next;
            } while (attr != null);
        }
        // attribute not found
        return null;
    }

    public override string GetAttribute(string localName, string namespaceUri)
    {
        // forward to base reader if replaying
        if (curNode == null)
        {
            return reader.GetAttribute(localName, namespaceUri);
        }
        // check that there are some cached attributes
        if (curAttrParent != null && curAttrParent.attributes != null)
        {
            // atomize the names
            localName = reader.NameTable.Get(localName);
            namespaceUri = reader.NameTable.Get(namespaceUri);

            // if the name is not in name table, there is no attribute with such name -> return 
            if (localName == null || namespaceUri == null)
            {
                return null;
            }
            // iterate through attributes fo find the one with the given name
            CachedXmlNode attr = curAttrParent.attributes;
            do
            {
                if ((object)localName == (object)attr.localName && (object)namespaceUri == (object)attr.namespaceUri)
                {
                    // found one -> return value
                    return attr.value;
                }
                attr = attr.next;
            } while (attr != null);
        }
        // attribute not found
        return null;
    }

    public override string GetAttribute(int i)
    {
        // forward to base reader if replaying
        if (curNode == null)
        {
            return reader.GetAttribute(i);
        }
        // check that there are some cached attributes
        if (curAttrParent != null && curAttrParent.attributes != null)
        {
            // iterate through attributes fo find one with the given index
            CachedXmlNode attr = curAttrParent.attributes;
            int index = 0;
            do
            {
                if (i == index)
                {
                    // found one -> return value
                    return attr.value;
                }
                attr = attr.next;
                index++;
            } while (attr != null);
        }
        throw new System.ArgumentOutOfRangeException("i");
    }

    public override string this[int i]
    {
        get
        {
            return GetAttribute(i);
        }
    }

    public override string this[string name]
    {
        get
        {
            return GetAttribute(name);
        }
    }

    public override string this[string name, string namespaceUri]
    {
        get
        {
            return GetAttribute(name, namespaceUri);
        }
    }

    public override bool MoveToAttribute(string name)
    {
        // forward to reader if not replaying
        if (curNode == null)
        {
            return reader.MoveToAttribute(name);
        }
        // check that there are some cached attributes
        if (curAttrParent != null && curAttrParent.attributes != null)
        {
            // atomize names
            name = reader.NameTable.Get(name);

            // if the name is not in name table, there is no attribute with such name 
            if (name == null)
            {
                return false;
            }
            // iterate through attributes fo find one with the given name
            CachedXmlNode attr = curAttrParent.attributes;
            do
            {
                if ((object)name == (object)attr.name)
                {
                    curNode = attr;
                    return true;
                }
                attr = attr.next;
            } while (attr != null);
        }
        // attribute not found
        return false;
    }

    public override bool MoveToAttribute(string localName, string namespaceUri)
    {
        // forward to reader if not replaying
        if (curNode == null)
        {
            return reader.MoveToAttribute(localName, namespaceUri);
        }
        // check that there are some cached attributes
        if (curAttrParent != null && curAttrParent.attributes != null)
        {
            // atomize names
            localName = reader.NameTable.Get(localName);
            namespaceUri = reader.NameTable.Get(namespaceUri);

            // if the name is not in name table, there is no attribute with such name 
            if (localName == null || namespaceUri == null)
            {
                return false;
            }
            // iterate through attributes fo find one with the given name
            CachedXmlNode attr = curAttrParent.attributes;
            do
            {
                if ((object)localName == (object)attr.localName && (object)namespaceUri == (object)attr.namespaceUri)
                {
                    curNode = attr;
                    return true;
                }
                attr = attr.next;
            } while (attr != null);
        }
        // attribute not found
        return false;
    }

    public override void MoveToAttribute(int i)
    {
        // forward to reader if not replaying
        if (curNode == null)
        {
            reader.MoveToAttribute(i);
            return;
        }
        // check that there are some cached attributes
        if (curAttrParent != null && curAttrParent.attributes != null)
        {
            // iterate through attributes fo find one with the given index
            CachedXmlNode attr = curAttrParent.attributes;
            int index = 0;
            do
            {
                if (i == index)
                {
                    curNode = attr;
                    return;
                }
                attr = attr.next;
                index++;
            } while (attr != null);
        }
        // not found -> invalid index
        throw new System.ArgumentOutOfRangeException("i");
    }

    public override bool MoveToFirstAttribute()
    {
        // forward to base reader if not replaying
        if (curNode == null)
        {
            return reader.MoveToFirstAttribute();
        }
        // return false if there are no attributes
        if (curAttrParent == null || curAttrParent.attributes == null)
        {
            return false;
        }
        // move to the first attribute
        curNode = curAttrParent.attributes;
        return true;
    }

    public override bool MoveToNextAttribute()
    {
        // forward to base reader if not replaying
        if (curNode == null)
        {
            return reader.MoveToNextAttribute();
        }
        // return false if there are no attributes
        if (curAttrParent == null || curAttrParent.attributes == null)
        {
            return false;
        }
        if (curNode.nodeType != System.Xml.XmlNodeType.Attribute)
        {
            // if on attribute parent, move to the first attribute
            if (curNode == curAttrParent)
            {
                if (curAttrParent.attributes != null)
                {
                    curNode = curAttrParent.attributes;
                    return true;
                }
            }
            // if on attribute text value, move to the next attribute
            else if (curNode == attributeTextValue)
            {
                CachedXmlNode nextAttr = attributeTextValue.next.next;
                if (nextAttr != null)
                {
                    curNode = nextAttr;
                    attributeTextValue.next = null;
                    return true;
                }
            }
            return false;
        }
        // otherwise move to the next attribute, if one exists
        if (curNode.next != null)
        {
            curNode = curNode.next;
            return true;
        }
        return false;
    }

    public override bool MoveToElement()
    {
        // forward to base reader if replaying
        if (curNode == null)
        {
            reader.MoveToElement();
        }
        // move to attribute parent
        if (curAttrParent != null)
        {
            if (curNode != curAttrParent)
            {
                curNode = curAttrParent;
                return true;
            }
        }
        return false;
    }

    public override bool ReadAttributeValue()
    {
        // forward to base reader if replaying
        if (curNode == null)
        {
            return reader.ReadAttributeValue();
        }

        // return false on node type type other than attribute
        if (curNode.nodeType != System.Xml.XmlNodeType.Attribute)
        {
            return false;
        }

        // setup a cached node for attribute value
        if (attributeTextValue == null)
        {
            attributeTextValue = new CachedXmlNode(System.Xml.XmlNodeType.Text, string.Empty, string.Empty, string.Empty, string.Empty, string.Empty, 0, null, null);
        }

        attributeTextValue.value = curNode.value;
        attributeTextValue.depth = curNode.depth + 1;
        attributeTextValue.namespacesInScope = currentNamespacesInScope;
        attributeTextValue.next = curNode;

        curNode = attributeTextValue;
        return true;
    }

    public override bool Read()
    {
        // have current node -> we are replaying
        if (curNode != null)
        {
            // recover from iterating over attributes
            if (curAttrParent != null)
            {
                curNode = curAttrParent;
                if (attributeTextValue != null)
                {
                    attributeTextValue.next = null;
                }
            }
            // move to next node in the list
            if (curNode.next != null)
            {
                SetCurrentNode(curNode.next);
                return true;
            }
            // end of cached nodes
            else
            {
                curNode = null;
            }
        }

        // pop namespace scope if previous node was an end element or an empty element
        currentNamespacesInScope = nextNamespacesInScope;

        // read next node from the reader
        if (!reader.Read())
        {
            return false;
        }
        // save namespaces
        ProcessNamespaces();

        // have some bookmark need to cache the node
        if (bookmarks.Count > 0)
        {
            CacheCurrentNode();
        }
        return true;
    }

    public override bool EOF
    {
        get
        {
            return curNode == null && reader.EOF;
        }
    }

    public override void Close()
    {
        reader.Close();
        curNode = null;
        curAttrParent = null;
    }

    public override System.Xml.ReadState ReadState
    {
        get
        {
            if (curNode == null)
            {
                return reader.ReadState;
            }
            else
            {
                return System.Xml.ReadState.Interactive;
            }
        }
    }

    public override System.Xml.XmlNameTable NameTable
    {
        get
        {
            return reader.NameTable;
        }
    }

    public override string LookupNamespace(string prefix)
    {
        if (curNode == null)
        {
            return LookupNamespace(prefix, currentNamespacesInScope);
        }
        else
        {
            return LookupNamespace(prefix, curNode.namespacesInScope);
        }
    }

    public override bool CanResolveEntity
    {
        get
        {
            return reader.CanResolveEntity;
        }
    }

    public override void ResolveEntity()
    {
        reader.ResolveEntity();
    }

    //
    // Bookmarking methods
    //
    // Sets a bookmark with the given name on the current node.
    public void SetBookmark(string bookmarkName)
    {
        if (reader.ReadState != System.Xml.ReadState.Interactive)
        {
            throw new System.InvalidOperationException("A bookmark can be set only when the reader is in ReadState.Interactive.");
        }
        if (reader.NodeType == System.Xml.XmlNodeType.Attribute)
        {
            throw new System.InvalidOperationException("A bookmark cannot be set when the reader on an attribute");
        }
        // check that the bookmark name is unique
        if (bookmarks[bookmarkName] != null)
        {
            throw new System.ArgumentException("Duplicate bookmark name.", "bookmarkName");
        }
        // figure out the first node of the bookmark, start caching if we are not already doing that
        CachedXmlNode bookmarkNode = curNode;
        if (curNode == null)
        {
            if (cachedNodes != null)
            {
                bookmarkNode = cachedNodes;
            }
            else
            {
                System.Diagnostics.Debug.Assert(bookmarks.Count == 0, "There should be no bookmarks and no cached nodes.");
                CacheCurrentNode();
                bookmarkNode = cachedNodes;
            }
        }

        // create a bookmark
        bookmarks.Add(bookmarkName, bookmarkNode);
    }

    // Moves the reader to a node that has a bookmark with the given name.
    public void ReturnToBookmark(string bookmarkName)
    {
        ReturnToBookmark(bookmarkName, false);
    }

    // This method is a combination of ReturnToBookmark and RemoveBookmark. It moves the reader back to a node 
    // that has a bookmark with the given name and then removes the bookmark. As the reader moves ahead, the nodes 
    // cached for this bookmark will be released for garbage collection, unless they are needed by a preceding bookmark.
    public void ReturnToAndRemoveBookmark(string bookmarkName)
    {
        ReturnToBookmark(bookmarkName, true);
    }

    void ReturnToBookmark(string bookmarkName, bool remove)
    {
        // find the bookmark
        CachedXmlNode bookmarkedNode = (CachedXmlNode)bookmarks[bookmarkName];
        while (bookmarkedNode != null)
        {
            // found it -> restart the reader to replay from this point
            SetCurrentNode(bookmarkedNode);
            // remove it from the list of bookmarks
            if (remove)
            {
                bookmarks.Remove(bookmarkName);
            }
            cachedNodes = null;
            return;
        }
        throw new System.ArgumentException("Bookmark \"" + bookmarkName + "\" does not exist.", "bookmarkName");
    }

    // Removes a bookmark with the given name. The XML nodes cached for this bookmark will be released 
    // for garbage collection unless they are needed by a preceding bookmark.
    public void RemoveBookmark(string bookmarkName)
    {
        bookmarks.Remove(bookmarkName);
        if (bookmarks.Count == 0)
        {
            cachedNodes = null;
        }
    }

    // Removes all bookmarks. All cached XML nodes will be released for garbage collection.
    public void RemoveAllBookmarks()
    {
        bookmarks.Clear();
        cachedNodes = null;
    }

    //
    // Private implementation methods
    //
    // Copies the properties of the current XmlReader node into a CachedXmlNode record and adds it to the list of cachedNodes
    private void CacheCurrentNode()
    {
        System.Xml.XmlNodeType nt = reader.NodeType;

        // cache the node
        CachedXmlNode node = new CachedXmlNode(reader, currentNamespacesInScope, null);
        if (nt == System.Xml.XmlNodeType.Element)
        {
            node.isDefaultOrEmpty = reader.IsEmptyElement;
        }
        if (cachedNodes != null)
        {
            cachedNodes.next = node;
        }
        cachedNodes = node;

        // cache its attributes
        if (reader.MoveToFirstAttribute())
        {
            CachedXmlNode lastAttr = null;
            do
            {
                CachedXmlNode attr = new CachedXmlNode(reader, currentNamespacesInScope, null);
                if (lastAttr == null)
                {
                    node.attributes = attr;
                }
                else
                {
                    lastAttr.next = attr;
                }
                lastAttr = attr;
            } while (reader.MoveToNextAttribute());
            reader.MoveToElement();
        }
    }

    // Sets the current node and current attribute parent
    void SetCurrentNode(CachedXmlNode node)
    {
        curNode = node;
        curAttrParent = (node.attributes != null) ? node : null;
    }

    // On element start tag it walks though the attributes of an element to find all namespace declaration and adds them to the namespace tree.
    // On End tag it adjusts the reference to the next namespace scope which will be applies on the next Read
    void ProcessNamespaces()
    {
        NamespaceDecl originalNamespaces = currentNamespacesInScope;
        System.Xml.XmlNodeType nodeType = reader.NodeType;

        switch (nodeType)
        {
            case System.Xml.XmlNodeType.Element:
                if (reader.MoveToFirstAttribute())
                {
                    do
                    {
                        if (reader.NamespaceURI == "http://www.w3.org/2000/xmlns/")
                        {
                            // add namespace
                            string prefix = (reader.Prefix.Length == 0) ? string.Empty : reader.LocalName;
                            currentNamespacesInScope = new NamespaceDecl(prefix, reader.NameTable.Add(reader.Value), currentNamespacesInScope);
                        }
                    } while (reader.MoveToNextAttribute());
                    reader.MoveToElement();
                }
                if (reader.IsEmptyElement)
                {
                    nextNamespacesInScope = originalNamespaces;
                }
                else
                {
                    nextNamespacesInScope = currentNamespacesInScope;
                    // push scope
                    currentNamespacesInScope.scopeCount++;
                }
                break;
            case System.Xml.XmlNodeType.EndElement:
                // pop scope
                NamespaceDecl decl = currentNamespacesInScope;
                decl.scopeCount--;
                while (decl.scopeCount == 0)
                {
                    decl = decl.previousDecl;
                }
                nextNamespacesInScope = decl;
                break;
        }
    }

    // Looks up a namespace in the list of current namespaces in scope
    string LookupNamespace(string prefix, NamespaceDecl namespacesInScope)
    {
        while (namespacesInScope != null)
        {
            if (prefix == namespacesInScope.prefix)
            {
                return namespacesInScope.namespaceUri;
            }
            namespacesInScope = namespacesInScope.previousDecl;
        }
        return null;
    }
}
