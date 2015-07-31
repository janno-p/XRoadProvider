namespace XRoad

(*
 * https://msdn.microsoft.com/en-us/library/aa302292.aspx
 * Helena Kupkova
 * Microsoft Corporation
 *
 * Modified and converted to F# by Janno Põldma.
 *)

open System.Xml
open System.Collections

module Option =
    let orDefault value opt =
        opt |> Option.fold (fun _ x -> x) value

type NamespaceDecl(prefix: string, namespaceURI: string, previousDecl: NamespaceDecl option) =
    member val Prefix = prefix with get
    member val NamespaceURI = namespaceURI with get
    member val PreviousDecl = previousDecl with get
    member val ScopeCount = 0 with get, set

type CachedXmlNode(nodeType: XmlNodeType, name: string, localName: string, prefix: string, nsURI: string, value: string, depth: int, next: CachedXmlNode option, ns: NamespaceDecl option) =
    new(reader: XmlReader, namespacesInScope, next) = CachedXmlNode(reader.NodeType, reader.Name, reader.LocalName, reader.Prefix, reader.NamespaceURI, reader.Value, reader.Depth, next, namespacesInScope)
    member val NodeType = nodeType with get
    member val Name = name with get
    member val LocalName = localName with get
    member val Prefix = prefix with get
    member val NamespaceURI = nsURI with get
    member val Value = value with get, set
    member val Depth = depth with get, set
    member val NamespacesInScope = ns with get, set
    member val Next = next with get, set
    member val IsDefaultOrEmpty = false with get, set
    member val Attributes = Option<CachedXmlNode>.None with get, set

type public XmlBookmarkReader(reader: XRoadXmlReader) as this =
    inherit XmlReader()

    let bookmarks = Hashtable()

    let mutable currentNamespacesInScope, nextNamespacesInScope =
        let nt = reader.NameTable
        let cnsisc = NamespaceDecl(nt.Add("xml"), nt.Add("http://www.w3.org/XML/1998/namespace"), None)
        let cnsisc = NamespaceDecl(nt.Add("xmlns"), nt.Add("http://www.w3.org/2000/xmlns/"), Some(cnsisc))
        let cnsisc = NamespaceDecl(nt.Add(""), nt.Add(""), Some(cnsisc))
        cnsisc.ScopeCount <- 1
        Some(cnsisc), Some(cnsisc)

    let mutable currentNode: CachedXmlNode option = None
    let mutable currentAttributeParent: CachedXmlNode option = None
    let mutable attributeTextValue: CachedXmlNode option = None
    let mutable cachedNodes: CachedXmlNode option = None

    let setCurrentNode node =
        currentNode <- Some(node)
        currentAttributeParent <- node.Attributes |> Option.map (fun _ -> node)

    let lookupNamespace prefix namespacesInScope =
        let rec walkNamespacesInScope (ns: NamespaceDecl option) =
            match ns with
            | Some(ns) ->
                if prefix = ns.Prefix then ns.NamespaceURI
                else walkNamespacesInScope ns.PreviousDecl
            | None -> null
        walkNamespacesInScope namespacesInScope

    let cacheCurrentNode () =
        let node =
            let nodeType = reader.NodeType
            let n = CachedXmlNode(reader, currentNamespacesInScope, None)
            if nodeType = XmlNodeType.Element then
                n.IsDefaultOrEmpty <- reader.IsEmptyElement
            n
        cachedNodes |> Option.iter (fun n -> n.Next <- Some(node))
        cachedNodes <- Some(node);
        if reader.MoveToFirstAttribute() then
            let rec walkAttributes (lastAttr: CachedXmlNode option) =
                let attr = CachedXmlNode(reader, currentNamespacesInScope, None)
                match lastAttr with
                | Some(lastAttr) -> lastAttr.Next <- Some(attr)
                | None -> node.Attributes <- Some(attr)
                if reader.MoveToNextAttribute() then
                    walkAttributes (Some(attr))
            walkAttributes None
            reader.MoveToElement() |> ignore

    let processNamespaces() =
        let originalNamespaces = currentNamespacesInScope
        match reader.NodeType with
        | XmlNodeType.Element ->
            if reader.MoveToFirstAttribute() then
                let rec walkAttributes() =
                    if reader.NamespaceURI = "http://www.w3.org/2000/xmlns/" then
                        let prefix = if reader.Prefix.Length = 0 then "" else reader.LocalName
                        currentNamespacesInScope <- Some(NamespaceDecl(prefix, reader.NameTable.Add(reader.Value), currentNamespacesInScope))
                    if reader.MoveToNextAttribute() then
                        walkAttributes()
                reader.MoveToElement() |> ignore
            if reader.IsEmptyElement then
                nextNamespacesInScope <- originalNamespaces
            else
                nextNamespacesInScope <- currentNamespacesInScope
                currentNamespacesInScope.Value.ScopeCount <- currentNamespacesInScope.Value.ScopeCount + 1
        | XmlNodeType.EndElement ->
            currentNamespacesInScope.Value.ScopeCount <- currentNamespacesInScope.Value.ScopeCount - 1
            let rec findDecl (decl: NamespaceDecl option) =
                if decl.Value.ScopeCount = 0 then findDecl decl.Value.PreviousDecl else decl
            nextNamespacesInScope <- findDecl currentNamespacesInScope
        | _ -> ()

    member __.Context with get() = reader.Context

    override __.NodeType with get() = currentNode |> Option.map (fun x -> x.NodeType) |> Option.orDefault reader.NodeType
    override __.Name with get() = currentNode |> Option.map (fun x -> x.Name) |> Option.orDefault reader.Name
    override __.LocalName with get() = currentNode |> Option.map (fun x -> x.LocalName) |> Option.orDefault reader.LocalName
    override __.NamespaceURI with get() = currentNode |> Option.map (fun x -> x.NamespaceURI) |> Option.orDefault reader.NamespaceURI
    override __.Prefix with get() = currentNode |> Option.map (fun x -> x.Prefix) |> Option.orDefault reader.Prefix
    override __.Value with get() = currentNode |> Option.map (fun x -> x.Value) |> Option.orDefault reader.Value
    override __.Depth with get() = currentNode |> Option.map (fun x -> x.Depth) |> Option.orDefault reader.Depth
    override __.BaseURI with get() = reader.BaseURI
    override __.IsEmptyElement with get() = currentNode |> Option.map (fun x -> x.NodeType = XmlNodeType.Element && x.IsDefaultOrEmpty) |> Option.orDefault reader.IsEmptyElement

    override __.AttributeCount
        with get() =
            let rec countAttributes (attr: CachedXmlNode option) =
                match attr with
                | Some(n) -> 1 + countAttributes n.Next
                | None -> 0
            currentNode |> Option.map (fun x -> countAttributes x.Attributes) |> Option.orDefault reader.AttributeCount

    override __.GetAttribute(name: string) =
        currentNode
        |> Option.map (fun _ ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(attr) ->
                match reader.NameTable.Get(name) with
                | null -> null
                | name ->
                    let rec check (attr: CachedXmlNode) =
                        if name = attr.Name then attr.Value
                        else match attr.Next with Some(n) -> check n | None -> null
                    check attr
            | None -> null)
        |> Option.orDefault (reader.GetAttribute(name))

    override __.HasValue with get() = currentNode |> Option.map (fun x -> x.NodeType <> XmlNodeType.Element && x.NodeType <> XmlNodeType.EntityReference && x.NodeType <> XmlNodeType.EndEntity) |> Option.orDefault reader.HasValue
    override __.IsDefault with get() = currentNode |> Option.map (fun x -> x.NodeType = XmlNodeType.Attribute && x.IsDefaultOrEmpty) |> Option.orDefault reader.IsDefault
    override __.QuoteChar with get() = reader.QuoteChar
    override __.XmlSpace with get() = reader.XmlSpace
    override __.XmlLang with get() = reader.XmlLang
    override __.EOF with get() = currentNode.IsNone && reader.EOF
    override __.Item with get(i: int) = this.GetAttribute(i)
    override __.Item with get(name: string) = this.GetAttribute(name)
    override __.Item with get(name, namespaceURI) = this.GetAttribute(name, namespaceURI)

    override __.Close() =
        reader.Close()
        currentNode <- None
        currentAttributeParent <- None

    override __.ReadState with get() = currentNode |> Option.map (fun _ -> ReadState.Interactive) |> Option.orDefault reader.ReadState
    override __.NameTable with get() = reader.NameTable
    override __.LookupNamespace(prefix) = currentNode |> Option.map (fun x -> lookupNamespace prefix x.NamespacesInScope) |> Option.orDefault (lookupNamespace prefix currentNamespacesInScope)
    override __.CanResolveEntity with get() = reader.CanResolveEntity
    override __.ResolveEntity() = reader.ResolveEntity()

    override __.Read() =
        let isDone =
            if currentNode.IsSome then
                if currentAttributeParent.IsSome then
                    currentNode <- currentAttributeParent
                    if attributeTextValue.IsSome then
                        attributeTextValue.Value.Next <- None
                if currentNode.Value.Next.IsSome then
                    setCurrentNode(currentNode.Value.Next.Value); true
                else currentNode <- None; false
            else false
        if isDone then true
        else
            currentNamespacesInScope <- nextNamespacesInScope
            if not(reader.Read()) then
                false
            else
                processNamespaces()
                if bookmarks.Count > 0 then cacheCurrentNode()
                true

    override __.ReadAttributeValue() =
        currentNode
        |> Option.map (fun x ->
            match x.NodeType with
            | XmlNodeType.Attribute -> false
            | _ ->
                if attributeTextValue.IsNone then
                    attributeTextValue <- Some(CachedXmlNode(XmlNodeType.Text, "", "", "", "", "", 0, None, None))
                attributeTextValue.Value.Value <- x.Value
                attributeTextValue.Value.Depth <- x.Depth + 1
                attributeTextValue.Value.NamespacesInScope <- currentNamespacesInScope
                attributeTextValue.Value.Next <- currentNode
                currentNode <- attributeTextValue
                true)
        |> Option.orDefault (reader.ReadAttributeValue())

    override __.MoveToElement() =
        if currentNode.IsNone then
            reader.MoveToElement() |> ignore
        match currentAttributeParent with
        | Some(_) ->
            if currentNode.Value <> currentAttributeParent.Value then
                currentNode <- currentAttributeParent
                true
            else false
        | None -> false

    override __.GetAttribute(localName, namespaceUri) =
        currentNode
        |> Option.map (fun _ ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(attr) ->
                match reader.NameTable.Get(localName), reader.NameTable.Get(namespaceUri) with
                | null, _ | _, null -> null
                | localName, namespaceUri ->
                    let rec walkAttributes (attr: CachedXmlNode) =
                        if localName = attr.LocalName && namespaceUri = attr.NamespaceURI then attr.Value
                        else match attr.Next with Some(a) -> walkAttributes a | None -> null
                    walkAttributes attr
            | None -> null)
        |> Option.orDefault (reader.GetAttribute(localName, namespaceUri))

    override __.GetAttribute(i: int) =
        currentNode
        |> Option.map (fun _ ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(attr) ->
                let rec walkAttributes (attr: CachedXmlNode) index =
                    if i = index then attr.Value
                    else match attr.Next with Some(a) -> walkAttributes a (index + 1) | None -> failwith "Argument i out of range."
                walkAttributes attr 0
            | None -> failwith "Argument i out of range.")
        |> Option.orDefault (reader.GetAttribute(i))

    override __.MoveToAttribute(name: string) =
        currentNode
        |> Option.map (fun _ ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(attr) ->
                match reader.NameTable.Get(name) with
                | null -> false
                | name ->
                    let rec walkAttributes (attr: CachedXmlNode) =
                        if name = attr.Name then currentNode <- Some(attr); true
                        else match attr.Next with Some(a) -> walkAttributes a | None -> false
                    walkAttributes attr
            | None -> false)
        |> Option.orDefault (reader.MoveToAttribute(name))

    override __.MoveToAttribute(localName, namespaceUri) =
        currentNode
        |> Option.map (fun _ ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(attr) ->
                match reader.NameTable.Get(localName), reader.NameTable.Get(namespaceUri) with
                | null, _ | _, null -> false
                | localName, namespaceUri ->
                    let rec walkAttributes (attr: CachedXmlNode) =
                        if localName = attr.LocalName && namespaceUri = attr.NamespaceURI then currentNode <- Some(attr); true
                        else match attr.Next with Some(a) -> walkAttributes(a) | None -> false
                    walkAttributes attr
            | None -> false)
        |> Option.orDefault (reader.MoveToAttribute(localName, namespaceUri))

    override __.MoveToAttribute(i: int) =
        currentNode
        |> Option.map (fun _ ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(attr) ->
                let rec walkAttributes (attr: CachedXmlNode) index =
                    if i = index then attr
                    else match attr.Next with Some(a) -> walkAttributes a (index + 1) | None -> failwith "Argument i out of range."
                currentNode <- Some(walkAttributes attr 0)
            | None -> failwith "Argument i out of range.")
        |> Option.orDefault (reader.MoveToAttribute(i) |> ignore)

    override __.MoveToFirstAttribute() =
        currentNode
        |> Option.map (fun _ ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(_) as attr -> currentNode <- attr; true
            | None -> false)
        |> Option.orDefault (reader.MoveToFirstAttribute())

    override __.MoveToNextAttribute() =
        currentNode
        |> Option.map (fun node ->
            match currentAttributeParent |> Option.bind (fun x -> x.Attributes) with
            | Some(_) as attr ->
                match node.NodeType with
                | XmlNodeType.Attribute ->
                    if node.Next.IsSome then currentNode <- currentNode.Value.Next; true
                    else false
                | _ ->
                    if currentNode.Value = currentAttributeParent.Value then currentNode <- attr; true
                    elif currentNode.Value = attributeTextValue.Value && attributeTextValue.Value.Next.Value.Next.IsSome then
                        currentNode <- attributeTextValue.Value.Next.Value.Next
                        attributeTextValue.Value.Next <- None
                        true
                    else false
            | None -> false)
        |> Option.orDefault (reader.MoveToNextAttribute())

    member __.SetBookmark(bookmarkName) =
        if reader.ReadState <> ReadState.Interactive then
            failwith "A bookmark can be set only when the reader is in ReadState.Interactive."
        if reader.NodeType = XmlNodeType.Attribute then
            failwith "A bookmark cannot be set when the reader on an attribute"
        if bookmarks.[bookmarkName] <> null then
            failwith "Duplicate bookmark name."
        let bookmarkNode =
            match currentNode with
            | None ->
                match cachedNodes with
                | Some(_) -> cachedNodes
                | None ->
                    assert (bookmarks.Count = 0)
                    cacheCurrentNode()
                    cachedNodes
            | _ -> currentNode
        bookmarks.Add(bookmarkName, bookmarkNode)

    member __.ReturnToBookmark(bookmarkName) =
        this.ReturnToBookmark(bookmarkName, false)

    member __.ReturnToAndRemoveBookmark(bookmarkName) =
        this.ReturnToBookmark(bookmarkName, true)

    member __.ReturnToBookmark(bookmarkName, remove) =
        match bookmarks.[bookmarkName] with
        | :? CachedXmlNode as node ->
            setCurrentNode node
            if remove then bookmarks.Remove(bookmarkName)
            cachedNodes <- None
        | _ -> failwithf @"Bookmark ""%s"" does not exist." bookmarkName

    member __.RemoveBookmark(bookmarkName) =
        bookmarks.Remove(bookmarkName)
        if bookmarks.Count = 0 then cachedNodes <- None

    member __.RemoveAllBookmarks() =
        bookmarks.Clear()
        cachedNodes <- None

    interface IXmlBookmarkReader with
        member val Reader = reader :> XmlReader with get
