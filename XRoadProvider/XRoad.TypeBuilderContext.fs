namespace XRoad

open System
open System.CodeDom
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq

open XRoad.CodeDom.Common
open XRoad.Common
open XRoad.ServiceDescription
open XRoad.TypeSchema

[<AutoOpen>]
module internal Pattern =
    /// Helper function to create full name for given type name.
    let providedTypeFullName nsname name = sprintf "DefinedTypes.%s.%s" nsname name

    /// Active pattern which checks type definition against collection characteristics.
    /// Returns match if given type should be treated as CollectionType.
    let (|ArrayContent|_|) (schemaType: SchemaType) =
        // SOAP-encoded array-s use special attribute for array type definition.
        let (|ArrayType|_|) (attributes: AttributeSpec list) =
            attributes |> List.tryFind (fun a -> a.Name = Some("arrayType") || a.RefOrType = Reference(XName.Get("arrayType", XmlNamespace.SoapEnc)))
        // Extracts information about array item type.
        let getArrayItemElement contentParticle =
            match contentParticle with
            | Some(ComplexTypeParticle.All(all)) ->
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
                            | true, ChoiceContent.Element(e) when e.MaxOccurs < 2u -> true
                            | true, ChoiceContent.Sequence(s) when s.MaxOccurs < 2u -> true
                            | _ -> false) true)
                     then None
                else failwith "Not implemented: array of varying choice types."
            | Some(ComplexTypeParticle.Sequence(sequence)) ->
                if sequence.MaxOccurs > 1u then failwith "Not implemented: array of anonymous sequence types."
                elif sequence.MaxOccurs < 1u then None
                else match sequence.Content with
                     | [ SequenceContent.Element(single) ] when single.MaxOccurs > 1u -> Some(single)
                     | _ -> None
            | Some(ComplexTypeParticle.Group) -> failwith "group not implemented."
            | None -> None
        // Test type definitions for collection characteristics.
        match schemaType with
        | ComplexType(spec) ->
            match spec.Content with
            // SOAP-encoded array-s inherit soapenc:Array type.
            | ComplexTypeContent.ComplexContent(Restriction(rstr)) when rstr.Base.LocalName = "Array" && rstr.Base.NamespaceName = XmlNamespace.SoapEnc ->
                match rstr.Content.Attributes with
                | ArrayType(attrSpec) ->
                    match attrSpec.ArrayType with
                    | Some(_, rank) when rank <> 1 -> failwith "Multidimensional SOAP encoding arrays are not supported."
                    | Some(typeName, _) ->
                        match getArrayItemElement(rstr.Content.Content) with
                        | Some(element) -> Some({ element with Type = Name(typeName) })
                        | None -> Some({ Name = Some("item"); MinOccurs = 0u; MaxOccurs = UInt32.MaxValue; IsNillable = true; Type = Name(typeName) })
                    | None -> failwith "Array underlying type specification is missing."
                | _ ->
                    match getArrayItemElement(rstr.Content.Content) with
                    | Some(_) as element -> element
                    | None -> failwith "Unsupported SOAP encoding array definition."
            // Multiplicity my constrain to using collection type.
            | ComplexTypeContent.Particle(content) -> getArrayItemElement(content.Content)
            | _ -> None
        | EmptyType
        | SimpleType(_) -> None

/// Combines operations and types documented in producer definitions.
type internal ProducerDescription =
    { TypeSchemas: Map<string,SchemaNode>
      Services: Service list }
    /// Load producer definition from given uri location.
    static member Load(uri: string) =
        let document = XDocument.Load(uri)
        let definitions = document.Element(xnsname "definitions" XmlNamespace.Wsdl)
        { Services = definitions |> ServiceDescription.Parser.parseServices
          TypeSchemas = definitions |> TypeSchema.Parser.parseSchema }

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
      Types: Map<string,SchemaType>
      /// Operation style used by this producer.
      Style: OperationStyle }
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
                        then sprintf "_%s" <| m.Groups.["path"].Value.toClassName()
                        else ""
                    Some(sprintf "%s%s" m.Groups.["producer"].Value suffix)
                | _ -> None
            match this.CachedNamespaces.TryGetValue(nsname) with
            | false, _ ->
                let producerName =
                    match nsname.NamespaceName with
                    | Producer(producerName) -> producerName
                    | XmlNamespace.Xtee -> "xtee"
                    | XmlNamespace.XRoad -> "xroad"
                    | ns -> ns.toClassName()
                let typ = Cls.create(producerName) |> Cls.addAttr TypeAttributes.Public
                this.CachedNamespaces.Add(nsname, typ)
                typ
            | true, typ -> typ

        /// Generates new RuntimeType instance depending on given type:
        /// xsd:base64Binary and xsd:hexBinary types represent ContentType.
        /// Types that are mapped to system types represent PrimitiveType value.
        /// Types that have multiplicity larger than 1 are defined as CollectionTypes.
        /// Other types will define separate ProvidedType in generated assembly.
        member private this.CreateType(name: SchemaName) =
            match name.XName with
            | BinaryType(_) -> ContentType
            | SystemType(typ) -> PrimitiveType(typ)
            | _ ->
                let nstyp = this.GetOrCreateNamespace(name.XName.Namespace)
                let schemaType =
                    match name with
                    | SchemaElement(xn) ->
                        let elementSpec = this.GetElementSpec(xn)
                        this.GetTypeDefinition(elementSpec.Type)
                    | SchemaType(xn) -> this.GetSchemaType(xn)
                match schemaType with
                | ArrayContent element ->
                    match this.GetElementDefinition(element) with
                    | itemName, Name(xn) -> CollectionType(this.GetRuntimeType(SchemaType(xn)), itemName, None)
                    | itemName, Definition(def) ->
                        let suffix = itemName.toClassName()
                        let typ = Cls.create(name.XName.LocalName + suffix) |> Cls.addAttr TypeAttributes.Public
                        nstyp |> Cls.addMember typ |> ignore
                        CollectionType(ProvidedType(typ, providedTypeFullName nstyp.Name typ.Name), itemName, Some(def))
                    | _, Reference(_) -> failwith "never"
                | _ ->
                    let attr =
                        match name with
                        | SchemaElement(_) -> Attributes.XmlRoot name.XName.LocalName name.XName.NamespaceName
                        | SchemaType(_) -> Attributes.XmlType name.XName
                    let typ = Cls.create(name.XName.LocalName) |> Cls.addAttr TypeAttributes.Public |> Cls.describe attr
                    nstyp |> Cls.addMember typ |> ignore
                    ProvidedType(typ, providedTypeFullName nstyp.Name typ.Name)

        /// Get runtime type from cached types if exists; otherwise create the type.
        member this.GetRuntimeType(name: SchemaName) =
            match this.CachedTypes.TryGetValue(name) with
            | true, info -> info
            | _ -> let info = this.CreateType(name)
                   this.CachedTypes.Add(name, info)
                   info

        /// Finds element specification from schema-level element lookup.
        member this.GetElementSpec(name: XName) =
            match this.Elements.TryFind(name.ToString()) with
            | Some(elementSpec) -> elementSpec
            | None -> failwithf "Invalid reference: global element %A was not found in current context." name

        /// Finds element specification from schema-level type lookup.
        member this.GetSchemaType(name: XName) =
            match this.Types.TryFind(name.ToString()) with
            | Some(schemaType) -> schemaType
            | None -> failwith "Invalid reference: global type `%A` was not found in current context." name

        /// Resolves real type definition from lookup by following the XML schema references if present.
        /// Returns value of type definitions which actually contains definition, not references other definition.
        member this.GetTypeDefinition(schemaObj) =
            let rec findTypeDefinition (schemaObj: SchemaObject<_>) =
                match schemaObj with
                | Name(xn)
                | Reference(xn) ->
                    match this.Types.TryFind(xn.ToString()) with
                    | Some(schemaType) -> schemaType
                    | None -> failwithf "Missing referenced schema type `%A`." xn
                | Definition(spec) -> spec
            findTypeDefinition schemaObj

        /// Resolves real atrribute definition from lookup by following the XML schema references if present.
        /// Returns value of attribute definitions which actually contains definition, not references other definition.
        member this.GetAttributeDefinition(spec) =
            let rec findAttributeDefinition (spec: AttributeSpec) =
                match spec.RefOrType with
                | Reference(ref) ->
                    match this.Attributes.TryFind(ref.ToString()) with
                    | Some(spec) -> findAttributeDefinition(spec)
                    | None ->
                        match ref with
                        | XmlName "lang" -> "lang", Name(XName.Get("string", XmlNamespace.Xsd))
                        | _ -> failwithf "Missing referenced attribute %A." ref
                | _ ->
                    match spec.Name with
                    | Some(name) -> name, spec.RefOrType
                    | None -> failwithf "Attribute has no name."
            findAttributeDefinition(spec)

        /// Resolves real element definition from lookup by following the XML schema references if present.
        /// Returns value of element definitions which actually contains definition, not references other definition.
        member this.GetElementDefinition(spec) =
            let rec findElementDefinition (spec: ElementSpec) =
                match spec.Type with
                | Reference(ref) ->
                    match this.Elements.TryFind(ref.ToString()) with
                    | Some(spec) -> findElementDefinition(spec)
                    | None -> failwithf "Missing referenced attribute %A." ref
                | _ ->
                    match spec.Name with
                    | Some(name) -> name, spec.Type
                    | None -> failwithf "Attribute has no name."
            findElementDefinition(spec)

        /// Initializes new context object from given schema definition.
        static member FromSchema(schema) =
            // Validates that schema contains single operation style, as required by X-Road specification.
            let style =
                let reduceStyle s1 s2 =
                    if s1 <> s2 then failwith "Mixing services of different style is not accepted!"
                    s1
                schema.Services
                |> List.map (fun svc -> svc.Ports |> List.map (fun p -> p.Style) |> List.reduce reduceStyle)
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
              Style = style }
