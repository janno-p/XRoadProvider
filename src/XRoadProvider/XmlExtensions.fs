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

    member this.ReadToEndElement(name, ns, depth, allowsAny, inlineContent) =
        let depth = if inlineContent then depth + 1 else depth
        if this.Depth = depth - 1 && this.IsEmptyElement then this.Read() |> ignore
        elif this.Depth = depth - 1 && this.NodeType = XmlNodeType.EndElement then ()
        else
            while this.Depth >= depth do
                if this.NodeType = XmlNodeType.Element && this.Depth = depth && not allowsAny then
                    failwithf "Expected end element of type `%s%s`, but element `%s` was found instead." (match ns with "" -> "" | n -> sprintf "%s:" n) name this.LocalName
                this.Read() |> ignore

    member this.ReadToContent(depth, wrapper, reqName) =
        if this.Depth = depth - 1 then
            if this.IsEmptyElement then
                if reqName.Equals("") then false else
                failwithf "Element `%s` was expected in content of %s, but no content was found." reqName wrapper
            else this.Read()
        elif this.Depth >= depth then true
        else failwith "never"

    member this.FindNextStartElement(depth) =
        let rec findNextStartElement () =
            if this.Depth < depth then false
            elif this.Depth = depth && this.NodeType = XmlNodeType.Element then true
            else this.Read() |> ignore; findNextStartElement()
        findNextStartElement()

    member this.IsMatchingElement(name: string, ns: string) =
        name.Equals(this.LocalName) && ns.Equals(this.NamespaceURI)

(*
emit' {
    set_marker startLabel
    define_label (fun markSuccess2 -> emit' {
        merge (emitMoveToEndOrNextElement markSuccess2 (skipRead, depthVar) prop.PropertyName)
        if_some_none requiredProp
            (fun p ->
                let expectedName =
                    match p with
                    | Individual { Element = Some(name,_,_) } | Array { Element = Some(name,_,_) } | Array { ItemElement = Some(name,_,_) } -> safe name
                    | _ -> "<end of sequence>"
                emitWrongElementException expectedName prop.Wrapper)
            (emit' { br returnLabel })
        set_marker markSuccess2
        nop
        // reader.Depth != depth
        ldarg_0
        callvirt_expr <@ (null: XmlReader).Depth @>
        ldloc depthVar
        ceq
        brfalse startLabel
        // reader.NodeType != XmlNodeType.Element
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NodeType @>
        ldc_node_type XmlNodeType.Element
        ceq
        brfalse startLabel
        if_else xs.IsEmpty
            (emitContent returnLabel)
            (emit' { define_label emitContent }) 
    })
}*)

(*
emit' {
    define_labels 2 (fun (List2(markDeserialize, markError)) -> emit' {
        // reader.LocalName != property.Name
        ldarg_0
        callvirt_expr <@ (null: XmlReader).LocalName @>
        ldstr name.LocalName
        string_equals
        brfalse markError
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NamespaceURI @>
        ldstr name.NamespaceName
        string_equals
        brtrue markDeserialize
        set_marker markError
        if_else isOptional
            (emit' {
                ldc_i4_1
                stloc skipRead
                br nextLabel
            })
            (emitWrongElementException (safe name) prop.Wrapper)
        set_marker markDeserialize
    })
}*)