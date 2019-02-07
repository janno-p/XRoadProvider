namespace XRoad

open CodeDom
open System
open System.CodeDom
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq
open XRoad.Serialization.Attributes
open TypeSchema
open Wsdl

[<AutoOpen>]
module internal Pattern =
    /// Helper function to create full name for given type name.
    let providedTypeFullName nsname name = sprintf "DefinedTypes.%s.%s" nsname name

    /// Active pattern which checks type definition against collection characteristics.
    /// Returns match if given type should be treated as CollectionType.
    let (|ArrayContent|_|) (schemaType: SchemaTypeDefinition) =
        // SOAP-encoded array-s use special attribute for array type definition.
        let (|ArrayType|_|) (attributes: AttributeSpec list) =
            attributes |> List.tryFind (fun a -> a.Name = Some("arrayType") || a.RefOrType = Reference(XName.Get("arrayType", XmlNamespace.SoapEnc)))
        // Extracts information about array item type.
        let getArrayItemElement contentParticle =
            match contentParticle with
            | Some(All(all)) ->
                if all.MaxOccurs > 1u then failwith "Not implemented: array of anonymous all types."
                elif all.MaxOccurs < 1u then None
                else match all.Elements with
                     | [ single ] when single.MaxOccurs > 1u -> Some(single)
                     | _ -> None
            | Some(ComplexTypeParticle.Choice(choice)) ->
                if choice.MaxOccurs > 1u then failwith "Not implemented: array of anonymous choice types."
                elif choice.MaxOccurs < 1u then None
                elif (choice.Content
                      |> List.fold (fun state ch ->
                            match state, ch with
                            | true, Element(e) when e.MaxOccurs < 2u -> true
                            | true, Sequence(s) when s.MaxOccurs < 2u -> true
                            | _ -> false) true)
                     then None
                else failwith "Not implemented: array of varying choice types."
            | Some(ComplexTypeParticle.Sequence(sequence)) ->
                if sequence.MaxOccurs > 1u then
                    match sequence.Content with
                    | [] -> None
                    | [ Element(single) ] -> Some(single)
                    | _ -> failwith "Not implemented: array of anonymous sequence types."
                elif sequence.MaxOccurs < 1u then None
                else match sequence.Content with
                     | [ Element(single) ] when single.MaxOccurs > 1u -> Some(single)
                     | _ -> None
            | Some(ComplexTypeParticle.Group) -> failwith "group not implemented."
            | None -> None
        // Test type definitions for collection characteristics.
        match schemaType with
        | ComplexDefinition(spec) ->
            match spec.Content with
            // SOAP-encoded array-s inherit soapenc:Array type.
            | ComplexContent(Restriction(rstr)) when rstr.Base.LocalName = "Array" && rstr.Base.NamespaceName = XmlNamespace.SoapEnc ->
                match rstr.Content.Attributes with
                | ArrayType(attrSpec) ->
                    match attrSpec.ArrayType with
                    | Some(_, rank) when rank <> 1 -> failwith "Multidimensional SOAP encoding arrays are not supported."
                    | Some(typeName, _) ->
                        match getArrayItemElement(rstr.Content.Content) with
                        | Some(element) -> Some({ element with Definition = Explicit(Name(typeName)) })
                        | None -> Some({ Name = Some("item"); MinOccurs = 0u; MaxOccurs = UInt32.MaxValue; IsNillable = true; Definition = Explicit(Name(typeName)); Annotation = None; ExpectedContentTypes = None })
                    | None -> failwith "Array underlying type specification is missing."
                | _ ->
                    match getArrayItemElement(rstr.Content.Content) with
                    | Some(_) as element -> element
                    | None -> failwith "Unsupported SOAP encoding array definition."
            // Multiplicity my constrain to using collection type.
            | Particle(content) -> getArrayItemElement(content.Content)
            | _ -> None
        | EmptyDefinition
        | SimpleDefinition(_) -> None

/// Combines operations and types documented in producer definitions.
type internal ProducerDescription =
    { TypeSchemas: Map<string,SchemaNode>
      Services: Service list }
    /// Load producer definition from given uri location.
    static member Load(uri: Uri, languageCode, operationFilter) =
        let document = Http.getXDocument uri
        match document.Element(xnsname "definitions" XmlNamespace.Wsdl) with
        | null -> failwithf "Uri `%A` refers to invalid WSDL document (`definitions` element not found)." uri
        | definitions ->
            { Services = definitions |> ServiceDescription.parseServices languageCode operationFilter
              TypeSchemas = definitions |> Parser.parseSchema (uri.ToString()) }

/// Context keeps track of already generated types for provided types and namespaces
/// to simplify reuse and resolve mutual dependencies between types.
type internal TypeBuilderContext =
    { /// Provided types generated from type schema definitions.
      CachedTypes: Dictionary<SchemaName,RuntimeType>
      /// Provided types generated to group types from same namespace.
      CachedNamespaces: Dictionary<XNamespace,CodeTypeDeclaration>
      /// Schema level attribute definition lookup.
      Attributes: Map<string,AttributeSpec>
      /// Schema level element definition lookup.
      Elements: Map<string,ElementSpec>
      /// Schema level type definition lookup.
      Types: Map<string,SchemaTypeDefinition>
      /// X-Road protocol used by this producer.
      MessageProtocol: XRoadMessageProtocolVersion
      /// Language code preferred for code comments.
      LanguageCode: string }
    with
        /// Find generated type that corresponds to given namespace name.
        /// If type exists, the existing instance is used; otherwise new type is generated.
        member this.GetOrCreateNamespace(nsname: XNamespace) =
            /// Extract producer name from namespace for simpler class name.
            let (|Producer|_|) ns =
                match Regex.Match(ns, @"^http://(((?<producer>\w+)\.x-road\.ee/producer(/(?<path>.+)?)?)|(producers\.\w+\.xtee\.riik\.ee/producer/(?<producer>\w+)(/(?<path>.+))?))$") with
                | m when m.Success ->
                    let suffix =
                        if m.Groups.["path"].Success
                        then sprintf "_%s" <| m.Groups.["path"].Value.ToClassName()
                        else ""
                    Some(sprintf "%s%s" m.Groups.["producer"].Value suffix)
                | _ -> None
            match this.CachedNamespaces.TryGetValue(nsname) with
            | false, _ ->
                let producerName =
                    match nsname.NamespaceName with
                    | Producer(producerName) -> producerName
                    | XmlNamespace.XRoad20 -> "xtee"
                    | XmlNamespace.XRoad31Ee -> "xroad"
                    | ns -> ns.ToClassName()
                let typ = Cls.create(producerName) |> Cls.addAttr TypeAttributes.Public
                Fld.create<string> "__TargetNamespace__"
                |> Fld.init (!^ nsname.NamespaceName)
                |> Fld.setAttr (MemberAttributes.Public ||| MemberAttributes.Const)
                |> Fld.addTo typ
                |> ignore
                this.CachedNamespaces.Add(nsname, typ)
                typ
            | true, typ -> typ

        /// Get runtime type from cached types if exists; otherwise create the type.
        member this.GetOrCreateType(name: SchemaName) =
            match this.CachedTypes.TryGetValue(name) with
            | true, info -> info
            | _ -> let info = this.CreateType(name)
                   this.CachedTypes.Add(name, info)
                   info

        /// Get runtime type from cached types if exists.
        member this.GetRuntimeType(name: SchemaName) =
            let resolvedName =
                match name with
                | SchemaElement(xname) ->
                    match this.GetElementSpec(xname) with
                    | ({ Definition = Explicit(Name(typeName)) } : ElementSpec) -> SchemaType(typeName)
                    | _ -> name
                | _ -> name
            match this.CachedTypes.TryGetValue(resolvedName) with
            | true, typeInfo -> typeInfo
            | _ -> match resolvedName.XName with
                   | BinaryType(_) -> ContentType
                   | SystemType(args) -> PrimitiveType(args)
                   | _ -> failwithf "Invalid type name `%A`: type not found in cache." resolvedName

        /// Generates new RuntimeType instance depending on given type:
        /// xsd:base64Binary and xsd:hexBinary types represent ContentType.
        /// Types that are mapped to system types represent PrimitiveType value.
        /// Types that have multiplicity larger than 1 are defined as CollectionTypes.
        /// Other types will define separate ProvidedType in generated assembly.
        member private this.CreateType(name: SchemaName) =
            match name.XName with
            | BinaryType(_) -> ContentType
            | SystemType(args) -> PrimitiveType(args)
            | _ ->
                let nstyp = this.GetOrCreateNamespace(name.XName.Namespace)
                let schemaType =
                    match name with
                    | SchemaElement(xn) ->
                        this.GetElementSpec(xn)
                        |> this.DereferenceElementSpec
                        |> snd
                        |> this.GetSchemaTypeDefinition
                    | SchemaType(xn) -> this.GetSchemaType(xn)
                match schemaType with
                | ArrayContent element ->
                    match this.DereferenceElementSpec(element) with
                    | dspec, Name(xn) ->
                        let itemName = dspec.Name |> Option.get
                        CollectionType(this.GetRuntimeType(SchemaType(xn)), itemName, None)
                    | dspec, Definition(def) ->
                        let itemName = dspec.Name |> Option.get
                        let suffix = itemName.ToClassName()
                        let typ = Cls.create(name.XName.LocalName + suffix) |> Cls.addAttr TypeAttributes.Public |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Sequence)
                        nstyp |> Cls.addMember typ |> ignore
                        CollectionType(ProvidedType(typ, providedTypeFullName nstyp.Name typ.Name), itemName, Some(def))
                | _ ->
                    let attr =
                        match name with
                        | SchemaElement(_) -> Attributes.xrdAnonymousType LayoutKind.Sequence
                        | SchemaType(_) -> Attributes.xrdType name.XName LayoutKind.Sequence
                    let typ = Cls.create(name.XName.LocalName) |> Cls.addAttr TypeAttributes.Public |> Cls.describe attr
                    nstyp |> Cls.addMember typ |> ignore
                    ProvidedType(typ, providedTypeFullName nstyp.Name typ.Name)

        /// Finds element specification from schema-level element lookup.
        member this.GetElementSpec(name: XName) =
            match this.Elements.TryFind(name.ToString()) with
            | Some(elementSpec) -> elementSpec
            | None -> failwithf "Invalid reference: global element %A was not found in current context." name

        /// Finds element specification from schema-level type lookup.
        member this.GetSchemaType(name: XName) =
            match this.Types.TryFind(name.ToString()) with
            | Some(schemaType) -> schemaType
            | None -> failwithf "Invalid reference: global type `%A` was not found in current context." name

        /// Resolves real type definition from lookup by following the XML schema references if present.
        /// Returns value of type definitions which actually contains definition, not references other definition.
        member this.GetSchemaTypeDefinition typeDefinition =
            let rec findSchemaTypeDefinition typeDefinition =
                match typeDefinition with
                | Definition(spec) -> spec
                | Name(xn) -> match this.Types.TryFind(xn.ToString()) with
                              | Some(schemaType) -> schemaType
                              | None -> failwithf "Missing referenced schema type `%A`." xn
            findSchemaTypeDefinition typeDefinition

        /// Resolves real atrribute definition from lookup by following the XML schema references if present.
        /// Returns value of attribute definitions which actually contains definition, not references other definition.
        member this.GetAttributeDefinition(spec) =
            let rec findAttributeDefinition (spec: AttributeSpec) =
                match spec.RefOrType with
                | Explicit(typeDefinition) ->
                    match spec.Name with
                    | Some(name) -> name, typeDefinition
                    | None -> failwithf "Attribute has no name."
                | Reference(ref) ->
                    match this.Attributes.TryFind(ref.ToString()) with
                    | Some(spec) -> findAttributeDefinition(spec)
                    | None ->
                        match ref with
                        | XmlName "lang" -> "lang", Name(XName.Get("string", XmlNamespace.Xsd))
                        | _ -> failwithf "Missing referenced attribute %A." ref
            findAttributeDefinition(spec)

        /// Resolves real element definition from lookup by following the XML schema references if present.
        /// Returns value of element definitions which actually contains definition, not references other definition.
        member this.DereferenceElementSpec(spec): ElementSpec * TypeDefinition<SchemaTypeDefinition> =
            let rec findElementDefinition (spec: ElementSpec) =
                match spec.Definition with
                | Explicit(typeDefinition) ->
                    match spec.Name with
                    | Some(_) -> spec, typeDefinition
                    | None -> failwithf "Attribute has no name."
                | Reference(ref) ->
                    match this.Elements.TryFind(ref.ToString()) with
                    | Some(spec) -> findElementDefinition(spec)
                    | None -> failwithf "Missing referenced attribute %A." ref
            findElementDefinition(spec)

        /// Initializes new context object from given schema definition.
        static member FromSchema(schema, languageCode) =
            // Validates that schema contains single operation style, as required by X-Road specification.
            let messageProtocol =
                let reduceStyle s1 s2 =
                    if s1 <> s2 then failwith "Mixing services implementing different X-Road message protocol versions is not accepted!"
                    s1
                schema.Services
                |> List.map (fun svc -> svc.Ports |> List.map (fun p -> p.MessageProtocol) |> List.reduce reduceStyle)
                |> List.reduce reduceStyle
            // Initialize type builder context.
            { CachedNamespaces = Dictionary<_,_>()
              CachedTypes = Dictionary<_,_>()
              Attributes =
                  schema.TypeSchemas
                  |> Map.toSeq
                  |> Seq.collect (fun (_,typ) -> typ.Attributes |> Seq.map (fun x -> x.Key.ToString(), x.Value))
                  |> Map.ofSeq
              Elements =
                  schema.TypeSchemas
                  |> Map.toSeq
                  |> Seq.collect (fun (_,typ) -> typ.Elements |> Seq.map (fun x -> x.Key.ToString(), x.Value))
                  |> Map.ofSeq
              Types =
                  schema.TypeSchemas
                  |> Map.toSeq
                  |> Seq.collect (fun (_,typ) -> typ.Types |> Seq.map (fun x -> x.Key.ToString(), x.Value))
                  |> Map.ofSeq
              MessageProtocol = messageProtocol
              LanguageCode = languageCode }
