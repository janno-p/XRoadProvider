module XRoad.XmlExtensions

open System
open System.Xml
open XRoad

type XmlReader with
    member this.ReadXsiNullAttribute() =
        let nilValue = (this.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> MyOption.defaultValue "").ToLower()
        nilValue.Equals("1") || nilValue.Equals("true")

    member this.ReadXsiTypeAttribute() =
        match this.GetAttribute("type", XmlNamespace.Xsi) with
        | null -> null
        | typeValue ->
            let qualifiedName = typeValue.Split([| ':' |], 2)
            let nsprefix, nm =
                match qualifiedName with
                | [| nm |] -> "", nm
                | [| nsprefix; nm |] -> nsprefix, nm
                | _ -> failwith "never"
            let ns = this.LookupNamespace(nsprefix)
            XmlQualifiedName(nm, ns)

    member this.IsQualifiedTypeName(qualifiedName: XmlQualifiedName, nm: string, ns: string, isAnonymous, isDefault) =
        if qualifiedName |> isNull then isAnonymous || isDefault else qualifiedName.Name.Equals(nm) && qualifiedName.Namespace.Equals(ns)

    member this.ReadToEndElement(name, ns, depth, allowsAny) =
        if this.Depth = depth - 1 && (this.IsEmptyElement || this.NodeType = XmlNodeType.EndElement) then () else
        while this.Read() && this.Depth >= depth do
            if this.NodeType = XmlNodeType.Element && this.Depth = depth && not allowsAny then
                failwithf "Expected end element of type `%s%s`, but element `%s` was found instead." (match ns with "" -> "" | n -> sprintf "%s:" n) name this.LocalName
