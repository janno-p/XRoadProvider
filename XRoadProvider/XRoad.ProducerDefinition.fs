module private XRoad.ProducerDefinition

open System
open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Net
open System.Reflection
open System.Security.Cryptography
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open System.Xml.Serialization

open XRoad.CodeDom
open XRoad.Parser
open XRoad.Parser.XsdSchema

type RuntimeType =
    | PrimitiveType of Type
    | ProvidedType of CodeTypeDeclaration * string
    | CollectionType of RuntimeType * string * SchemaType option
    | ContentType
    member this.AsCodeTypeReference() = match this with
                                        | PrimitiveType(typ) -> CodeTypeReference(typ)
                                        | ProvidedType(_,name) -> CodeTypeReference(name)
                                        | CollectionType(typ,_,_) -> CodeTypeReference(typ.AsCodeTypeReference(), 1)
                                        | ContentType -> CodeTypeReference("BinaryContent")

let providedTypeFullName nsname name =
    sprintf "DefinedTypes.%s.%s" nsname name

let parseArrayContent (schemaType: SchemaType) =
    let (|ArrayType|_|) (attributes: AttributeSpec list) =
        attributes |> List.tryFind (fun a -> a.Name = Some("arrayType") || a.RefOrType = Reference(XName.Get("arrayType", XmlNamespace.SoapEncoding)))
    let getArrayItemElement contentParticle =
        match contentParticle with
        | Some(ComplexTypeParticle.All(all)) ->
            if all.MaxOccurs > 1u then failwith "Not implemented: array of anonymous all types."
            elif all.MaxOccurs < 1u then None
            else match all.Elements with
                 | [ single ] when single.MaxOccurs > 1u -> Some(single)
                 | _ -> None
        | Some(ComplexTypeParticle.Choice(_)) ->
            failwith "Not implemented: array of choice types."
        | Some(ComplexTypeParticle.Sequence(sequence)) ->
            if sequence.MaxOccurs > 1u then failwith "Not implemented: array of anonymous sequence types."
            elif sequence.MaxOccurs < 1u then None
            else match sequence.Content with
                 | [ SequenceContent.Element(single) ] when single.MaxOccurs > 1u -> Some(single)
                 | _ -> None
        | None -> None
    match schemaType with
    | ComplexType(spec) ->
        match spec.Content with
        | ComplexTypeContent.ComplexContent(Restriction(rstr)) when rstr.Base.LocalName = "Array" && rstr.Base.NamespaceName = XmlNamespace.SoapEncoding ->
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
        | ComplexTypeContent.Particle(content) -> getArrayItemElement(content.Content)
        | _ -> None
    | EmptyType
    | SimpleType(_) -> None

type TypeBuilderContext =
    { CachedTypes: Dictionary<XmlReference,RuntimeType>
      CachedNamespaces: Dictionary<XNamespace,CodeTypeDeclaration>
      Attributes: Map<string,AttributeSpec>
      Elements: Map<string,ElementSpec>
      Types: Map<string,SchemaType>
      Style: OperationStyle }
    with
        member this.GetOrCreateNamespace(nsname: XNamespace) =
            let (|Producer|_|) ns =
                match Regex.Match(ns, @"^http://(((?<producer>\w+)\.x-road\.ee/producer/(?<path>.*)?)|(producers\.\w+\.xtee\.riik\.ee/producer/(?<producer>\w+)(/(?<path>.*))?))$") with
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
        member private this.CreateType(name: XmlReference) =
            match name.XName with
            | XsdType "hexBinary"
            | XsdType "base64Binary"
            | SoapEncType "base64Binary" -> ContentType
            | SystemType(typ) -> PrimitiveType(typ)
            | _ ->
                let nstyp = this.GetOrCreateNamespace(name.XName.Namespace)
                let schemaType =
                    match name with
                    | SchemaElement(xn) ->
                        let elementSpec = this.GetElementSpec(xn)
                        this.GetTypeDefinition(elementSpec.Type)
                    | SchemaType(xn) -> this.GetSchemaType(xn)
                match parseArrayContent(schemaType) with
                | Some(element) ->
                    match this.GetElementDefinition(element) with
                    | itemName, Name(xn) -> CollectionType(this.GetRuntimeType(SchemaType(xn)), itemName, None)
                    | itemName, Definition(def) ->
                        let suffix = itemName.toClassName()
                        let typ = Cls.create(name.XName.LocalName + suffix) |> Cls.addAttr TypeAttributes.Public
                        nstyp |> Cls.addMember typ |> ignore
                        CollectionType(ProvidedType(typ, providedTypeFullName nstyp.Name typ.Name), itemName, Some(def))
                    | _, Reference(_) -> failwith "never"
                | None ->
                    let attr =
                        match name with
                        | SchemaElement(_) -> Attributes.XmlRoot name.XName.LocalName name.XName.NamespaceName
                        | SchemaType(_) -> Attributes.XmlType name.XName
                    let typ = Cls.create(name.XName.LocalName) |> Cls.addAttr TypeAttributes.Public |> Cls.describe attr
                    nstyp |> Cls.addMember typ |> ignore
                    ProvidedType(typ, providedTypeFullName nstyp.Name typ.Name)
        member this.GetRuntimeType(name: XmlReference) =
            match this.CachedTypes.TryGetValue(name) with
            | true, info -> info
            | _ -> let info = this.CreateType(name)
                   this.CachedTypes.Add(name, info)
                   info
        member this.GetElementSpec(name: XName) =
            match this.Elements.TryFind(name.ToString()) with
            | Some(elementSpec) -> elementSpec
            | None -> failwithf "Invalid reference: global element %A was not found in current context." name
        member this.GetSchemaType(name: XName) =
            match this.Types.TryFind(name.ToString()) with
            | Some(schemaType) -> schemaType
            | None -> failwith "Invalid reference: global type `%A` was not found in current context." name
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
        static member FromSchema(schema) =
            let style =
                let reduceStyle s1 s2 =
                    if s1 <> s2 then failwith "Mixing services of different style is not accepted!"
                    s1
                schema.Services
                |> List.map (fun svc -> svc.Ports |> List.map (fun p -> p.Style) |> List.reduce reduceStyle)
                |> List.reduce reduceStyle
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

// Define X-Road SOAP header element name and description values depending on operation style used in WSDL binding:
// First tuple contains RPC/Encoded style values and second one values for Document/Literal style.
let headerMapping = function
    | "asutus"                    -> ("asutus", "Asutus", "Asutuse DNS-nimi."),
                                     ("consumer", "Consumer", "DNS-name of the institution")
    | "andmekogu"                 -> ("andmekogu", "Andmekogu", "Andmekogu DNS-nimi."),
                                     ("producer", "Producer", "DNS-name of the database")
    | "isikukood"                 -> ("isikukood", "Isikukood", "Teenuse kasutaja isikukood, millele eelneb kahekohaline maa kood. Näiteks EE37702026518."),
                                     ("userId", "UserId", "ID code of the person invoking the service, preceded by a two-letter country code. For example: EE37702026518")
    | "ametnik"                   -> ("ametnik", "Ametnik", "Teenuse kasutaja Eesti isikukood (ei ole kasutusel alates versioonist 5.0)."),
                                     ("", "", "")
    | "id"                        -> ("id", "Id", "Teenuse väljakutse nonss (unikaalne identifikaator)."),
                                     ("id", "Id", "Service invocation nonce (unique identifier)")
    | "nimi"                      -> ("nimi", "Nimi", "Kutsutava teenuse nimi."),
                                     ("service", "Service", "Name of the service to be invoked")
    | "toimik"                    -> ("toimik", "Toimik", "Teenuse väljakutsega seonduva toimiku number (mittekohustuslik)."),
                                     ("issue", "Issue", "Name of file or document related to the service invocation")
    | "allasutus"                 -> ("allasutus", "Allasutus", "Asutuse registrikood, mille nimel teenust kasutatakse (kasutusel juriidilise isiku portaalis)."),
                                     ("unit", "Unit", "Registration code of the institution or its unit on whose behalf the service is used (applied in the legal entity portal)")
    | "amet"                      -> ("amet", "Amet", "Teenuse kasutaja ametikoht."),
                                     ("position", "Position", "Organizational position or role of the person invoking the service")
    | "ametniknimi"               -> ("ametniknimi", "Ametniknimi", "Teenuse kasutaja nimi."),
                                     ("userName", "UserName", "Name of the person invoking the service")
    | "asynkroonne"               -> ("asynkroonne", "Asynkroonne", "Teenuse kasutamise asünkroonsus. Kui väärtus on 'true', siis sooritab turvaserver päringu asünkroonselt."),
                                     ("async", "Async", "Specifies asynchronous service. If the value is \"true\", then the security server performs the service call asynchronously.")
    | "autentija"                 -> ("autentija", "Autentija", "Teenuse kasutaja autentimise viis. Võimalikud variandid on: ID - ID-kaardiga autenditud; SERT - muu sertifikaadiga autenditud; PANK - panga kaudu autenditud; PAROOL - kasutajatunnuse ja parooliga autenditud. Autentimise viisi järel võib sulgudes olla täpsustus (näiteks panga kaudu autentimisel panga tunnus infosüsteemis)."),
                                     ("authenticator", "Authenticator", "Authentication method, one of the following: ID-CARD - with a certificate of identity; CERT - with another certificate; EXTERNAL - through a third-party service; PASSWORD - with user ID and a password. Details of the authentication (e.g. the identification of a bank for external authentication) can be given in brackets after the authentication method.")
    | "makstud"                   -> ("makstud", "Makstud", "Teenuse kasutamise eest makstud summa."),
                                     ("paid", "Paid", "The amount of money paid for invoking the service")
    | "salastada"                 -> ("salastada", "Salastada", "Kui asutusele on X-tee keskuse poolt antud päringute salastamise õigus ja andmekogu on nõus päringut salastama, siis selle elemendi olemasolul päringu päises andmekogu turvaserver krüpteerib päringu logi, kasutades selleks X-tee keskuse salastusvõtit."),
                                     ("encrypt", "Encrypt", "If an organization has got the right from the X-Road Center to hide queries, with the database agreeing to hide the query, the occurrence of this tag in the query header makes the database security server to encrypt the query log, using the encryption key of the X-Road Center")
    | "salastada_sertifikaadiga"  -> ("salastada_sertifikaadiga", "SalastadaSertifikaadiga", "Päringu sooritaja ID-kaardi autentimissertifikaat DERkujul base64 kodeerituna. Selle elemendi olemasolu päringu päises väljendab soovi päringu logi salastamiseks asutuse turvaserveris päringu sooritaja ID-kaardi autentimisvõtmega. Seda välja kasutatakse ainult kodaniku päringute portaalis."),
                                     ("encryptCert", "EncryptCert", "Authentication certificate of the query invokers ID Card, in the base64-encoded DER format. Occurrence of this tag in the query header represents the wish to encrypt the query log in the organizations security server, using authentication key of the query invokers ID Card. This field is used in the Citizen Query Portal only.")
    | "salastatud"                -> ("salastatud", "Salastatud", "Kui päringu välja päises oli element salastada ja päringulogi salastamine õnnestus, siis vastuse päisesse lisatakse tühi element salastatud."),
                                     ("encrypted", "Encrypted", "If the query header contains the encrypt tag and the query log as been successfully encrypted, an empty encrypted tag will be inserted in the reply header.")
    | "salastatud_sertifikaadiga" -> ("salastatud_sertifikaadiga", "SalastatudSertifikaadiga", "Kui päringu päises oli element salastada_sertifikaadiga ja päringulogi salastamine õnnestus, siis vastuse päisesesse lisatakse tühi element salastatud_sertifikaadiga."),
                                     ("encryptedCert", "EncryptedCert", "If the query header contains the encryptedCert tag and the query log has been successfully encrypted, an empty encryptedCert tag will accordingly be inserted in the reply header.")
    | name                        -> failwithf "Invalid header name '%s'" name

// Functions to help extracting values from previously defined mappings.
let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let trd3 (_, _, x) = x

// Description of method which generates random nonce value.
// Nonce value is sent in X-Road <id /> header element, when user has not given its own value.
let defaultNonceMethod =
    Meth.create "GenerateNonce"
    |> Meth.setAttr MemberAttributes.Private
    |> Meth.returns<string>
    |> Meth.addStmt (Stmt.declVarWith<byte[]> "nonce" (Arr.createOfSize<byte> 42))
    |> Meth.addStmt (Stmt.declVarWith<RandomNumberGenerator> "rng" ((Expr.typeRefOf<RNGCryptoServiceProvider> @-> "Create") @% []))
    |> Meth.addExpr ((Expr.var "rng" @-> "GetNonZeroBytes") @% [Expr.var("nonce")])
    |> Meth.addStmt (Stmt.ret ((Expr.typeRefOf<Convert> @-> "ToBase64String") @% [Expr.var "nonce"]))

let writeXRoadHeaderMethod (style) =
    let hdrns, hdrName, propName, nsprefix =
        match style with
        | DocLiteral -> XmlNamespace.XRoad, headerMapping >> snd >> fst3, headerMapping >> snd >> snd3, "xrd"
        | RpcEncoded -> XmlNamespace.Xtee, headerMapping >> fst >> fst3, headerMapping >> fst >> snd3, "xtee"
    let writerVar = Expr.var "writer"
    let writeHeaderElement name propVar methodName =
        let reqHeaderContainsNameExpr = ((Expr.var "requiredHeaders") @-> "Contains") @% [Expr.value name]
        let propNotNullExpr = Op.isNotNull propVar
        Stmt.condIf (Op.boolOr reqHeaderContainsNameExpr
                               propNotNullExpr)
                    [ Stmt.ofExpr ((writerVar @-> "WriteStartElement") @% [Expr.value name; Expr.value hdrns])
                      Stmt.condIf propNotNullExpr [ Stmt.ofExpr ((writerVar @-> methodName) @% [propVar]) ]
                      Stmt.ofExpr ((writerVar @-> "WriteEndElement") @% []) ]
    let declareWithDefaultValue name xtname defExpr (m: CodeMemberMethod) =
        m |> Meth.addStmt (Stmt.declVar<string> name)
          |> Meth.addStmt (Stmt.condIfElse (Op.isNull (Expr.this @=> (propName xtname)))
                                           [Stmt.assign (Expr.var name) defExpr]
                                           [Stmt.assign (Expr.var name) (Expr.this @=> (propName xtname))])
    Meth.create "WriteHeader"
    |> Meth.setAttr (MemberAttributes.Family ||| MemberAttributes.Final)
    |> Meth.addParam<XmlWriter> "writer"
    |> Meth.addParam<string> "serviceName"
    |> Meth.addParam<IList<string>> "requiredHeaders"
    |> Meth.addExpr ((writerVar @-> "WriteAttributeString") @% [Expr.value "xmlns"; Expr.value nsprefix; Expr.value null; Expr.value hdrns])
    |> declareWithDefaultValue "producerValue" "andmekogu" (Expr.var "producerName")
    |> declareWithDefaultValue "requestId" "id" ((Expr.this @-> "GenerateNonce") @% [])
    |> declareWithDefaultValue "fullServiceName" "nimi" ((Expr.typeRefOf<string> @-> "Format") @% [Expr.value "{0}.{1}"; Expr.var "producerValue"; Expr.var "serviceName"])
    |> Meth.addStmt (writeHeaderElement (hdrName "asutus") (Expr.this @=> (propName "asutus")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "andmekogu") (Expr.var "producerValue") "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "isikukood") (Expr.this @=> (propName "isikukood")) "WriteString")
    |> iif (style = RpcEncoded) (fun m -> m |> Meth.addStmt (writeHeaderElement (hdrName "ametnik") (Expr.this @=> (propName "ametnik")) "WriteString"))
    |> Meth.addStmt (writeHeaderElement (hdrName "id") (Expr.var "requestId") "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "nimi") (Expr.var "fullServiceName") "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "toimik") (Expr.this @=> (propName "toimik")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "allasutus") (Expr.this @=> (propName "allasutus")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "amet") (Expr.this @=> (propName "amet")) "WriteRaw")
    |> Meth.addStmt (writeHeaderElement (hdrName "ametniknimi") (Expr.this @=> (propName "ametniknimi")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "asynkroonne") (Expr.this @=> (propName "asynkroonne")) "WriteValue")
    |> Meth.addStmt (writeHeaderElement (hdrName "autentija") (Expr.this @=> (propName "autentija")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "makstud") (Expr.this @=> (propName "makstud")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "salastada") (Expr.this @=> (propName "salastada")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "salastada_sertifikaadiga") (Expr.this @=> (propName "salastada_sertifikaadiga")) "WriteString")

let inheritBinaryContent (typ: CodeTypeDeclaration) =
    typ
    |> Cls.setParent (ContentType.AsCodeTypeReference())
    |> Cls.addMember (Ctor.create()
                      |> Ctor.setAttr MemberAttributes.Public
                      |> Ctor.addParam<IDictionary<string,Stream>> "attachments"
                      |> Ctor.addParam<string> "id"
                      |> Ctor.addBaseArg (Expr.var "attachments")
                      |> Ctor.addBaseArg (Expr.var "id"))
    |> Cls.addMember (Ctor.create() |> Ctor.setAttr MemberAttributes.Public)
    |> ignore

let createBinaryContentType () =
    let contentField = Fld.create<Stream> "content__backing"
    let contentProp =
        Prop.create<Stream> "Content"
        |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        |> Prop.addGetStmt (Stmt.ret (Expr.this @=> contentField.Name))
        |> Prop.addSetStmt (Stmt.assign (Expr.this @=> contentField.Name) Prop.setValue)
    contentProp.CustomAttributes.Add(Attributes.XmlIgnore) |> ignore
    let idField = Fld.create<string> "id__backing"
    let idProp =
        Prop.create<string> "Id"
        |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        |> Prop.addGetStmt (Expr.this @=> idField.Name |> Stmt.ret)
        |> Prop.addSetStmt (Stmt.assign (Expr.this @=> idField.Name) Prop.setValue)
    idProp.CustomAttributes.Add(Attributes.XmlAttributeWithName "href") |> ignore
    let valueProp =
        Prop.create<byte[]> "Value"
        |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        |> Prop.addGetStmt (Stmt.declVarWith<MemoryStream> "temp" Expr.nil)
        |> Prop.addGetStmt (Stmt.tryFinally
                                [ Stmt.assign (Expr.var "temp") (Expr.inst<MemoryStream> [])
                                  Stmt.assign (Expr.var contentField.Name @=> "Position") (Expr.value 0)
                                  Stmt.ofExpr ((Expr.var contentField.Name @-> "CopyTo") @% [Expr.var "temp"])
                                  Stmt.ret ((Expr.var "temp" @-> "ToArray") @% []) ]
                                [ Stmt.ofExpr ((Expr.var "temp" @-> "Dispose") @% []) ])
        |> Prop.addSetStmt (Stmt.assign (Expr.this @=> contentField.Name) (Expr.inst<MemoryStream> [Prop.setValue]))
    valueProp.CustomAttributes.Add(Attributes.XmlText) |> ignore
    let ctor =
        Ctor.create()
        |> Ctor.setAttr MemberAttributes.Public
        |> Ctor.addParam<IDictionary<string,Stream>> "attachments"
        |> Ctor.addParam<string> "id"
        |> Ctor.addStmt (Stmt.assign (Expr.this @=> idField.Name) ((Expr.typeRefOf<string> @-> "Format") @% [Expr.value "cid:{0}"; Expr.var "id"]))
        |> Ctor.addStmt (Stmt.assign (Expr.this @=> contentField.Name) (CodeArrayIndexerExpression(Expr.var "attachments", Expr.var "id")))
    Cls.create "BinaryContent"
    |> Cls.setAttr TypeAttributes.Public
    |> Cls.addMember (contentField)
    |> Cls.addMember (contentProp)
    |> Cls.addMember (idField)
    |> Cls.addMember (idProp)
    |> Cls.addMember (valueProp)
    |> Cls.addMember (ctor)
    |> Cls.addMember (Ctor.create() |> Ctor.setAttr MemberAttributes.Public)

let private makeServicePortBaseType(undescribedFaults, style: OperationStyle) =
    let portBaseTy = Cls.create "AbstractServicePort" |> Cls.setAttr (TypeAttributes.Public ||| TypeAttributes.Abstract)

    let addressField = Fld.create<string> "producerUri"
    let addressFieldRef = Expr.this @=> addressField.Name
    let addressProperty = CodeMemberProperty(Name="ProducerUri", Type=CodeTypeReference(typeof<string>))
    addressProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    addressProperty.GetStatements.Add(Stmt.ret addressFieldRef) |> ignore
    addressProperty.SetStatements.Add(Stmt.assign addressFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

    let producerField = CodeMemberField(typeof<string>, "producerName")
    let producerFieldRef = Expr.this @=> producerField.Name
    let producerProperty = CodeMemberProperty(Name="ProducerName", Type=CodeTypeReference(typeof<string>))
    producerProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    producerProperty.GetStatements.Add(Stmt.ret producerFieldRef) |> ignore
    producerProperty.SetStatements.Add(Stmt.assign producerFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

    let ctor =
        Ctor.create()
        |> Ctor.setAttr MemberAttributes.Family
        |> Ctor.addParam<string> "producerUri"
        |> Ctor.addParam<string> "producerName"
        |> Ctor.addStmt (Stmt.assign (Expr.this @=> "producerUri") (Expr.var("producerUri")))
        |> Ctor.addStmt (Stmt.assign (Expr.this @=> "producerName") (Expr.var("producerName")))

    let moveToElementMeth = CodeMemberMethod(Name="MoveToElement")
    moveToElementMeth.Attributes <- MemberAttributes.Family
    moveToElementMeth.ReturnType <- CodeTypeReference(typeof<bool>)
    moveToElementMeth |> Meth.addParam<XmlReader> "reader"
                      |> Meth.addParam<string> "name"
                      |> Meth.addParam<string> "ns"
                      |> Meth.addParam<int> "depth"
                      |> Meth.addStmt (Stmt.whileLoop (Expr.value true)
                                                      [ Stmt.condIf (Op.boolAnd (Op.equals (Expr.var "reader" @=> "Depth")
                                                                                           (Expr.var "depth"))
                                                                                (Op.boolAnd (Op.equals (Expr.var "reader" @=> "NodeType")
                                                                                                       (Expr.typeRefOf<XmlNodeType> @=> "Element"))
                                                                                            (Op.boolOr (Op.isNull (Expr.var "name"))
                                                                                                       (Op.boolAnd (Op.equals (Expr.var "reader" @=> "LocalName")
                                                                                                                              (Expr.var "name"))
                                                                                                                   (Op.equals (Expr.var "reader" @=> "NamespaceURI")
                                                                                                                              (Expr.var "ns"))))))
                                                                    [Expr.value true |> Stmt.ret]
                                                        Stmt.condIfElse (Op.boolAnd ((Expr.var "reader" @-> "Read") @% [])
                                                                                    (Op.ge (Expr.var "reader" @=> "Depth")
                                                                                           (Expr.var "depth")))
                                                                        []
                                                                        [Expr.value false |> Stmt.ret] ])
                      |> Meth.addStmt (Expr.value false |> Stmt.ret)
                      |> ignore

    let serviceCallMeth =
        Meth.create "MakeServiceCall"
        |> Meth.setAttr (MemberAttributes.Family ||| MemberAttributes.Final)
        |> Meth.addParam<IDictionary<string,Stream>> "attachments"
        |> Meth.typeParam "T"
        |> Meth.returnsOf (CodeTypeReference("T"))

    let writerStatements = [
        Stmt.assign (Expr.var("stream")) ((Expr.var "request" @-> "GetRequestStream") @% [])

        Stmt.declVarWith<StreamWriter> "sw" Expr.nil
        Stmt.tryFinally
            [ Stmt.assign (Expr.var "sw") (Expr.inst<StreamWriter> [Expr.var "stream"])
              Stmt.declVarWith<string> "boundaryMarker" Expr.nil
              Stmt.condIf (Op.boolAnd (Op.notEquals (Expr.var "attachments") Expr.nil)
                                      (Op.greater (Expr.var "attachments" @=> "Count") (Expr.value 0)))
                          [ Stmt.assign (Expr.var "boundaryMarker") ((((Expr.typeRefOf<Guid> @-> "NewGuid") @% []) @-> "ToString") @% [])
                            Stmt.assign (Expr.var "request" @=> "ContentType") ((Expr.typeRefOf<string> @-> "Format") @% [Expr.value "multipart/related; type=\"text/xml\"; start=\"test\"; boundary=\"{0}\""; Expr.var "boundaryMarker"])
                            Stmt.ofExpr (((Expr.var "request" @=> "Headers") @-> "Add") @% [Expr.value "MIME-Version"; Expr.value "1.0"])
                            Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [])
                            Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "--{0}"; Expr.var "boundaryMarker"])
                            Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "Content-Type: text/xml; charset=UTF-8"])
                            Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "Content-Transfer-Encoding: 8bit"])
                            Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "Content-ID: <test>"])
                            Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% []) ]
              Stmt.declVarWith<XmlWriter> "writer" Expr.nil
              Stmt.tryFinally
                  [ Stmt.assign (Expr.var("writer")) ((Expr.typeRefOf<XmlWriter> @-> "Create") @% [Expr.var "sw"])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteStartDocument") @% [])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteStartElement") @% [Expr.value "soapenv"; Expr.value "Envelope"; Expr.value XmlNamespace.SoapEnvelope])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteStartElement") @% [Expr.value "Header"; Expr.value XmlNamespace.SoapEnvelope])
                    Stmt.ofExpr ((Expr.var "writeHeaderAction") @%% [Expr.var "writer"])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteEndElement") @% [])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteStartElement") @% [Expr.value "Body"; Expr.value XmlNamespace.SoapEnvelope])
                    Stmt.ofExpr ((Expr.var "writeBody") @%% [Expr.var "writer"])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteEndElement") @% [])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteEndElement") @% [])
                    Stmt.ofExpr ((Expr.var "writer" @-> "WriteEndDocument") @% []) ]
                  [ Stmt.condIf (Op.isNotNull (Expr.var "writer")) [ (Expr.var "writer" @-> "Dispose") @% [] |> Stmt.ofExpr ] ]
              Stmt.condIf (Op.boolAnd (Op.notEquals (Expr.var "attachments") Expr.nil)
                                      (Op.greater (Expr.var "attachments" @=> "Count") (Expr.value 0)))
                          [ Stmt.forLoop (Stmt.declVarWith<IEnumerator<KeyValuePair<string,Stream>>> "enumerator" ((Expr.var "attachments" @-> "GetEnumerator") @% []))
                                         ((Expr.var "enumerator" @-> "MoveNext") @% [])
                                         (Stmt.ofExpr Expr.empty)
                                         [ Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [])
                                           Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "--{0}"; Expr.var "boundaryMarker"])
                                           Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "Content-Disposition: attachment; filename=notAnswering"])
                                           Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "Content-Type: application/octet-stream"])
                                           Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "Content-Transfer-Encoding: binary"])
                                           Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "Content-ID: <{0}>"; (Expr.var "enumerator" @=> "Current") @=> "Key"])
                                           Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [])
                                           Stmt.ofExpr ((Expr.var "sw" @-> "Flush") @% [])
                                           Stmt.declVarWith<int> "bytesRead" (Expr.value 1000)
                                           Stmt.declVarWith<byte[]> "buffer" (Arr.createOfSize<byte> 1000)
                                           Stmt.assign (((Expr.var "enumerator" @=> "Current") @=> "Value") @=> "Position") (Expr.value 0)
                                           Stmt.whileLoop (Op.ge (Expr.var "bytesRead") (Expr.value 1000))
                                                          [ Stmt.assign (Expr.var "bytesRead") (((((Expr.var "enumerator" @=> "Current") @=> "Value") @-> "Read")) @% [Expr.var "buffer"; Expr.value 0; Expr.value 1000])
                                                            Stmt.ofExpr ((Expr.var "stream" @-> "Write") @% [Expr.var "buffer"; Expr.value 0; Expr.var "bytesRead"]) ]
                                           Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% []) ]
                            Stmt.ofExpr ((Expr.var "sw" @-> "WriteLine") @% [Expr.value "--{0}--"; Expr.var "boundaryMarker"]) ] ]
            [ Stmt.condIf (Op.isNotNull (Expr.var "sw")) [ (Expr.var "sw" @-> "Dispose") @% [] |> Stmt.ofExpr ] ]
        ]

    let xmlReaderTypRef = if undescribedFaults then CodeTypeReference("XmlBookmarkReader") else CodeTypeReference(typeof<XmlReader>)

    let createReaderExpr =
        let readerExpr = (Expr.typeRefOf<XmlReader> @-> "Create") @% [(Expr.var "response" @-> "GetResponseStream") @% []]
        if undescribedFaults then Expr.instOf xmlReaderTypRef [readerExpr] else readerExpr

    let readerStatements = [
        Stmt.assign (Expr.var("response")) ((Expr.var "request" @-> "GetResponse") @% [])
        Stmt.declVarRefWith xmlReaderTypRef "reader" Expr.nil
        Stmt.tryFinally
            [ Stmt.assign (Expr.var("reader")) createReaderExpr
              Stmt.condIfElse ((Expr.this @-> "MoveToElement") @% [Expr.var "reader"; Expr.value "Envelope"; Expr.value XmlNamespace.SoapEnvelope; Expr.value 0])
                               []
                               [ Stmt.throw<Exception> [Expr.value "Soap envelope element was not found in response message."] ]
              Stmt.condIfElse ((Expr.this @-> "MoveToElement") @% [Expr.var "reader"; Expr.value "Body"; Expr.value XmlNamespace.SoapEnvelope; Expr.value 1])
                               []
                               [ Stmt.throw<Exception> [Expr.value "Soap body element was not found in response message."] ]
              (Expr.this @-> "MoveToElement") @% [Expr.var "reader"; Expr.nil; Expr.nil; Expr.value 2] |> Stmt.ofExpr
              Stmt.condIf (Op.boolAnd (Op.equals (Expr.var "reader" @=> "LocalName")
                                                 (Expr.value "Fault"))
                                      (Op.equals (Expr.var "reader" @=> "NamespaceURI")
                                                 (Expr.value XmlNamespace.SoapEnvelope)))
                           [Stmt.throw<Exception> [(Expr.var "reader" @-> "ReadInnerXml") @% []]]
              Stmt.ret ((Expr.var "readBody") @%% [Expr.var "reader"]) ]
            [ Stmt.condIf (Op.isNotNull (Expr.var "reader"))
                          [(Expr.var "reader" @-> "Dispose") @% [] |> Stmt.ofExpr] ]
        ]

    serviceCallMeth |> Meth.addParam<Action<XmlWriter>> "writeHeaderAction"
                    |> Meth.addParam<Action<XmlWriter>> "writeBody"
                    |> Meth.addParamRef (CodeTypeReference("System.Func", typeRef<XmlReader>, CodeTypeReference("T"))) "readBody"
                    |> Meth.addStmt (Stmt.declVarWith<WebRequest> "request" ((Expr.typeRefOf<Net.WebRequest> @-> "Create") @% [Expr.var "producerUri"]))
                    |> Meth.addStmt (Stmt.assign (Expr.var "request" @=> "Method") (Expr.value "POST"))
                    |> Meth.addStmt (Stmt.assign (Expr.var "request" @=> "ContentType") (Expr.value "text/xml; charset=utf-8"))
                    |> Meth.addExpr (((Expr.var "request" @=> "Headers") @-> "Set") @% [Expr.value "SOAPAction"; Expr.value ""])
                    |> Meth.addStmt (Stmt.declVarWith<Stream> "stream" Expr.nil)
                    |> Meth.addStmt (Stmt.tryFinally writerStatements
                                                     [ Stmt.condIf (Op.isNotNull (Expr.var "stream"))
                                                                   [(Expr.var "stream" @-> "Dispose") @% [] |> Stmt.ofExpr] ])
                    |> Meth.addStmt (Stmt.declVarWith<WebResponse> "response" Expr.nil)
                    |> Meth.addStmt (Stmt.tryFinally readerStatements
                                                     [ Stmt.condIf (Op.isNotNull (Expr.var "response"))
                                                                   [(Expr.var "response" @-> "Dispose") @% [] |> Stmt.ofExpr] ])
                    |> ignore

    portBaseTy.Members.Add(ctor) |> ignore
    portBaseTy.Members.Add(addressField) |> ignore
    portBaseTy.Members.Add(addressProperty) |> ignore
    portBaseTy.Members.Add(producerField) |> ignore
    portBaseTy.Members.Add(producerProperty) |> ignore
    portBaseTy.Members.Add(serviceCallMeth) |> ignore
    portBaseTy.Members.Add(writeXRoadHeaderMethod(style)) |> ignore
    portBaseTy.Members.Add(defaultNonceMethod) |> ignore
    portBaseTy.Members.Add(moveToElementMeth) |> ignore

    let choose = headerMapping >> (match style with RpcEncoded -> fst | DocLiteral -> snd)
    let propName = choose >> snd3
    let docValue = choose >> trd3

    [ "asutus"; "andmekogu"; "isikukood"; "id"; "nimi"; "toimik"; "allasutus"; "amet"; "ametniknimi"; "autentija"; "makstud"; "salastada"; "salastada_sertifikaadiga"; "salastatud"; "salastatud_sertifikaadiga" ]
    |> List.fold (fun typ hdr -> typ |> createProperty<string> (propName hdr) (docValue hdr)) portBaseTy
    |> iif (style = RpcEncoded) (fun typ -> typ |> createProperty<string> (propName "ametnik") (docValue "ametnik"))
    |> createProperty<Nullable<bool>> (propName "asynkroonne") (docValue "asynkroonne")

let private getRequiredHeaders(operation: Operation) =
    let headers, rest =
        operation.Request.Header
        |> List.partition (fun part ->
            match part with
            | IsXteeHeader _ when operation.Style = RpcEncoded -> true
            | IsXRoadHeader _ when operation.Style = DocLiteral -> true
            | _ -> false)
    if rest.Length > 0 then
        failwithf "Unhandled SOAP Header elements detected: %A" rest
    headers |> List.map (fun part -> part.Name)

let makeReturnType (types: RuntimeType list) =
    let rec getReturnTypeTuple (tuple: (int * RuntimeType) list, types) =
        match types with
        | [] -> let typ = CodeTypeReference("System.Tuple", tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference()) |> Array.ofList)
                (typ, Expr.instOf typ (tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i))))
        | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
        | x::xs -> let inner = getReturnTypeTuple([x], xs)
                   let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
                   (typ, Expr.instOf typ ((tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i))) @ [snd inner]))
    match types |> List.mapi (fun i x -> (i, x)) with
    | [] -> (CodeTypeReference(typeof<Void>), Expr.var("???"))
    | (i,tp)::[] -> (tp.AsCodeTypeReference(), Expr.var(sprintf "v%d" i))
    | many -> getReturnTypeTuple([], many)

let buildParameterType (context: TypeBuilderContext) isMultipart (part: MessagePart) =
    match context.Style, part.Reference with
    | DocLiteral, SchemaType(t) ->
        failwithf "Document/Literal style message part '%s' should reference global element as message part, but type '%s' is used instead" part.Name t.LocalName
    | RpcEncoded, SchemaElement(e) ->
        failwithf "RPC/Encoded style message part '%s' should reference global type as message part, but element '%s' is used instead" part.Name e.LocalName
    | DocLiteral, SchemaElement(elementName) ->
        let elemType = snd <| context.GetElementDefinition(context.GetElementSpec(elementName))
        match elemType with
        | Name(name) ->
            context.GetRuntimeType(SchemaType(name)),
            fun varName ->
                [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") (Expr.inst<XmlRootAttribute> [Expr.value elementName.LocalName])
                  yield Stmt.assign (Expr.var (varName + "Root") @=> "Namespace") (Expr.value elementName.NamespaceName)
                  if isMultipart then
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") (Expr.inst<XmlAttributeOverrides> [])
                    yield Stmt.declVarWith<XmlAttributes> (varName + "Value") (Expr.inst<XmlAttributes> [])
                    yield Stmt.assign (Expr.var (varName + "Value") @=> "XmlIgnore") (Expr.value true)
                    yield Stmt.ofExpr ((Expr.var (varName + "Overrides") @-> "Add") @% [Expr.typeOf (CodeTypeReference("BinaryContent")); Expr.value "Value"; Expr.var (varName + "Value")])
                  else
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") Expr.nil ]
        | Reference(_) ->
            failwith "not implemented"
        | Definition(_) ->
            context.GetRuntimeType(SchemaElement(elementName)),
            fun varName ->
                [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") Expr.nil
                  if isMultipart then
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") (Expr.inst<XmlAttributeOverrides> [])
                    yield Stmt.declVarWith<XmlAttributes> (varName + "Value") (Expr.inst<XmlAttributes> [])
                    yield Stmt.assign (Expr.var (varName + "Value") @=> "XmlIgnore") (Expr.value true)
                    yield Stmt.ofExpr ((Expr.var (varName + "Overrides") @-> "Add") @% [Expr.typeOf (CodeTypeReference("BinaryContent")); Expr.value "Value"; Expr.var (varName + "Value")])
                  else
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") Expr.nil ]
    | RpcEncoded, SchemaType(typeName) ->
        context.GetRuntimeType(SchemaType(typeName)), 
        fun varName ->
            [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") (Expr.inst<XmlRootAttribute> [Expr.value part.Name])
              if isMultipart then
                yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") (Expr.inst<XmlAttributeOverrides> [])
                yield Stmt.declVarWith<XmlAttributes> (varName + "Value") (Expr.inst<XmlAttributes> [])
                yield Stmt.assign (Expr.var (varName + "Value") @=> "XmlIgnore") (Expr.value true)
                yield Stmt.ofExpr ((Expr.var (varName + "Overrides") @-> "Add") @% [Expr.typeOf (CodeTypeReference("BinaryContent")); Expr.value "Value"; Expr.var (varName + "Value")])
              else
                yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") Expr.nil ]

let makeProducerType (typeNamePath: string [], producerUri, undescribedFaults) =
    let schema = resolveUri producerUri |> readSchema
    let context = TypeBuilderContext.FromSchema(schema)

    let portBaseTy = makeServicePortBaseType(undescribedFaults, context.Style)
    let serviceTypesTy = Cls.create "DefinedTypes" |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

//    let (|ArrayType|_|) (attributes: AttributeSpec list) =
//        attributes |> List.tryFind (fun a -> a.Name = Some("arrayType") || a.RefOrType = Reference(XName.Get("arrayType", XmlNamespace.SoapEncoding)))
//
//    let (|SoapEncArray|_|) (def: SchemaType) =
//        match def with SchemaType.ComplexType(x) -> Some(x) | _ -> None
//        |> Option.bind (fun x ->
//            match x.Content with
//            | ComplexTypeContent.ComplexContent(Restriction(c))
//                when c.Base.LocalName = "Array" && c.Base.NamespaceName = XmlNamespace.SoapEncoding -> Some(c.Content)
//            | _ -> None)
//
//    let getArrayItemElement particle =
//        match particle with
//        | Some(ComplexTypeParticle.Sequence(spec)) ->
//            match spec.Content with
//            | [ SequenceContent.Element(e) ] -> Some(e)
//            | _ -> None
//        | Some(ComplexTypeParticle.All(spec)) ->
//            match spec.Elements with
//            | [ e ] -> Some(e)
//            | _ -> None
//        | _ -> None

    let rec buildType(runtimeType: RuntimeType, typeInfo) =
        let providedTy, providedTypeName =
            match runtimeType with
            | ProvidedType(decl, name) -> decl, name
            | _ -> failwith "Only provided types are accepted as arguments!"

        let addProperty(name, ty: RuntimeType, isOptional) =
            let specifiedField =
                if isOptional then
                    let f = CodeMemberField(typeof<bool>, name + "__specified")
                    f.CustomAttributes.Add(Attributes.DebuggerBrowsable) |> ignore
                    providedTy.Members.Add(f) |> ignore
                    let p = CodeMemberProperty(Name=name + "Specified", Type=CodeTypeReference(typeof<bool>))
                    p.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
                    p.GetStatements.Add(Expr.this @=> f.Name |> Stmt.ret) |> ignore
                    providedTy.Members.Add(p) |> ignore
                    Some(f)
                else None
            let backingField = CodeMemberField(ty.AsCodeTypeReference(), name + "__backing")
            backingField.CustomAttributes.Add(Attributes.DebuggerBrowsable) |> ignore
            providedTy.Members.Add(backingField) |> ignore
            let backingFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), backingField.Name)
            let property = CodeMemberProperty(Name=name, Type=ty.AsCodeTypeReference())
            property.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
            property.GetStatements.Add(CodeMethodReturnStatement(backingFieldRef)) |> ignore
            property.SetStatements.Add(CodeAssignStatement(backingFieldRef, CodePropertySetValueReferenceExpression())) |> ignore
            match specifiedField with
            | Some(field) ->
                let fieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), field.Name)
                property.SetStatements.Add(CodeAssignStatement(fieldRef, CodePrimitiveExpression(true))) |> ignore
            | _ -> ()
            providedTy.Members.Add(property) |> ignore
            property

        let rec getAttributeType (schemaObject, name) =
            let convertedObject =
                match schemaObject with
                | Definition(simpleTypeSpec) -> Definition(SimpleType(simpleTypeSpec))
                | Name(name) -> Name(name)
                | Reference(ref) -> Reference(ref)
            getParticleType(convertedObject, 1u, false, name)

//        and getArrayType (contentSpec: ComplexTypeContentSpec) =
//            match contentSpec.Attributes with
//            | ArrayType(attrSpec) ->
//                match attrSpec.ArrayType with
//                | Some(typeName, rank) ->
//                    let itemName = getArrayItemElement(contentSpec.Content) |> Option.bind (fun x -> x.Name) |> Option.orDefault "item"
//                    [1..rank] |> List.fold (fun aggType _ -> CollectionType(aggType, itemName)) (context.GetRuntimeType(SchemaType(typeName)))
//                | _ -> failwith "Array underlying type specification is missing."
//            | _ ->
//                match getArrayItemElement(contentSpec.Content) with
//                | Some(elementSpec) ->
//                    let elemName, elemType = context.GetElementDefinition(elementSpec)
//                    let subTyName = providedTy.Name + "ArrayItem"
//                    let elementTy = fst <| getParticleType(elemType, elementSpec.MaxOccurs, elementSpec.IsNillable, subTyName)
//                    CollectionType(elementTy, elemName)
//                | None -> failwith "Unsupported SOAP encoding array definition."

        and getParticleType (particleType, maxOccurs, isNillable, name) =
            match particleType with
            | Name(xname) ->
                let typ = context.GetRuntimeType(SchemaType(xname))
                match typ with
                | x when maxOccurs > 1u ->
                    CollectionType(x, name, None), [Attributes.XmlElement(true)]
                | PrimitiveType(x) when x.IsValueType ->
                    if isNillable then (PrimitiveType(typedefof<Nullable<_>>.MakeGenericType(x)), [Attributes.XmlElement(true)])
                    else (PrimitiveType(x), [Attributes.XmlElement(false)])
                | x -> (x, [Attributes.XmlElement(true)])
//            | Definition(SoapEncArray(contentSpec)) ->
//                getArrayType(contentSpec), [ Attributes.XmlArray(true); Attributes.XmlArrayItem("temp") ]
            | Definition(typeInfo) ->
                let subTy = Cls.create (name + "Type") |> Cls.addAttr TypeAttributes.Public
                let runtimeType = ProvidedType(subTy, subTy.Name)
                buildType(runtimeType, typeInfo)
                providedTy.Members.Add(subTy) |> ignore
                if maxOccurs > 1u
                then CollectionType(runtimeType, name, None), [Attributes.XmlElement(true)]
                else runtimeType, [Attributes.XmlElement(true)]
            | _ -> failwithf "not implemented: %A" name

        and parseElementSpec(spec: ElementSpec) =
            let elemName, elemType = context.GetElementDefinition(spec)
            let elementTy, attrs = getParticleType(elemType, spec.MaxOccurs, spec.IsNillable, elemName)
            let property = addProperty(elemName, elementTy, spec.MinOccurs = 0u)
            attrs |> List.iter (property.CustomAttributes.Add >> ignore)

        let parseComplexTypeContentSpec(spec: ComplexTypeContentSpec) =
            spec.Attributes |> List.iter (fun spec ->
                let attrName, attrTypeDef = context.GetAttributeDefinition(spec)
                let attributeTy, _ = getAttributeType(attrTypeDef, attrName)
                let property = addProperty(attrName, attributeTy, match spec.Use with Required -> true | _ -> false)
                property.CustomAttributes.Add(Attributes.XmlAttribute) |> ignore)
            match spec.Content with
            | Some(ComplexTypeParticle.All(spec)) ->
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then
                    failwith "not implemented"
                spec.Elements |> List.iter parseElementSpec
            | Some(ComplexTypeParticle.Sequence(spec)) ->
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then
                    failwith "not implemented"
                spec.Content |> List.iter (fun item ->
                    match item with
                    | SequenceContent.Choice(_) -> printfn "TODO: Choice not implemented."
                    | SequenceContent.Element(spec) -> parseElementSpec(spec)
                    | SequenceContent.Any -> printfn "TODO: Any not implemented."
                    | _ -> failwith "not implemented")
            | Some(ComplexTypeParticle.Choice(_)) -> printfn "TODO: Choice type not implemented!"
            | None -> ()

        match typeInfo with
//        | SoapEncArray(contentSpec) ->
//            match getArrayType(contentSpec) with
//            | CollectionType(elementType, itemName) as arrayType ->
//                let property = addProperty("Array", arrayType, false)
//                property.CustomAttributes.Add(Attributes.XmlElement2(itemName, elementType.AsCodeTypeReference())) |> ignore
//            | _ -> failwith "not implemented"
        | SimpleType(SimpleTypeSpec.Restriction(spec)) ->
            match context.GetRuntimeType(SchemaType(spec.Base)) with
            | PrimitiveType(_) as rtyp ->
                let property = addProperty("BaseValue", rtyp, false)
                property.CustomAttributes.Add(Attributes.XmlText) |> ignore
                // TODO: Apply constraints?
            | ContentType -> providedTy |> inheritBinaryContent
            | _ -> failwith "not implemented"
        | SimpleType(Union(_)) ->
            failwith "not implemented"
        | ComplexType(spec) ->
            if spec.IsAbstract then
                providedTy.TypeAttributes <- providedTy.TypeAttributes ||| TypeAttributes.Abstract
                providedTy.Members.Add(CodeConstructor(Attributes=MemberAttributes.Family)) |> ignore
            match spec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                match context.GetRuntimeType(SchemaType(spec.Base)) with
                | PrimitiveType(_)
                | ContentType as rtyp ->
                    let property = addProperty("BaseValue", rtyp, false)
                    property.CustomAttributes.Add(Attributes.XmlText) |> ignore
                    parseComplexTypeContentSpec(spec.Content)
                | _ -> failwith "not implemented"
            | SimpleContent(SimpleContentSpec.Restriction(_)) ->
                failwith "not implemented"
            | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                let baseTy = context.GetRuntimeType(SchemaType(spec.Base))
                providedTy |> Cls.setParent (baseTy.AsCodeTypeReference()) |> ignore
                match baseTy with
                | ProvidedType(baseDecl,_) ->
                    baseDecl |> Cls.describe (Attributes.XmlInclude(typeRefName providedTypeName)) |> ignore
                | _ -> failwithf "Only complex types can be inherited! (%A)" spec.Base
                parseComplexTypeContentSpec(spec.Content)
            | ComplexContent(ComplexContentSpec.Restriction(_)) ->
                failwith "not implemented"
            | ComplexTypeContent.Particle(spec) ->
                parseComplexTypeContentSpec(spec)
        | EmptyType -> ()

    let buildElementType (typ: RuntimeType, spec: ElementSpec) =
        match spec.Type with
        | Definition(def) -> buildType(typ, def)
        | Reference(_) -> failwith "Root level element references are not allowed."
        | Name(_) -> ()

    let buildOperationService (operation: Operation) =
        let isMultipart = operation.Request.MultipartContent |> List.isEmpty |> not
        let serviceMethod = Meth.create operation.Name.LocalName |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        let requestParameters = operation.Request.Body.Parts |> List.map (fun p -> p |> buildParameterType context isMultipart, p.Name)
        let responseParameters = operation.Response.Body.Parts |> List.map (fun p -> p |> buildParameterType context isMultipart, p.Name)
        let returnType, returnExpr = responseParameters |> List.map (fst >> fst) |> makeReturnType

        serviceMethod.ReturnType <- returnType

        let requiredHeadersExpr =
            getRequiredHeaders(operation)
            |> List.map Expr.value
            |> Arr.create<string>

        let serviceName = match operation.Version with Some v -> sprintf "%s.%s" operation.Name.LocalName v | _ -> operation.Name.LocalName

        // CodeDom doesn't support delegates, so we have to improvise
        serviceMethod
        |> Meth.addStmt (Stmt.declVarWith<string[]> "requiredHeaders" requiredHeadersExpr)
        |> Meth.addStmt (Stmt.declVarWith<Action<XmlWriter>> "writeHeader" (Expr.code "(writer) => { //"))
        |> Meth.addExpr ((Expr.parent @-> "WriteHeader") @% [Expr.var "writer"; Expr.value serviceName; Expr.var "requiredHeaders"])
        |> Meth.addExpr (Expr.code "}")
        |> Meth.addStmt (Stmt.declVarWith<Action<XmlWriter>> "writeBody" (Expr.code "(writer) => { //"))
        |> Meth.addExpr ((Expr.var "writer" @-> "WriteAttributeString") @% [Expr.value "xmlns"; Expr.value "svc"; Expr.nil; Expr.value operation.Name.NamespaceName])
        |> iif (operation.Request.Body.Namespace <> operation.Name.NamespaceName) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteAttributeString") @% [Expr.value "xmlns"; Expr.value "svcns"; Expr.nil; Expr.value operation.Request.Body.Namespace]))
        |> iif (operation.Style = RpcEncoded) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteStartElement") @% [Expr.value operation.Request.Name.LocalName; Expr.value operation.Request.Body.Namespace]))
        |> ignore

        requestParameters
        |> List.iter (fun ((runtimeType, overrideFunc), partName) ->
            let serializerName = partName + "Serializer"
            let typ = runtimeType.AsCodeTypeReference()
            serviceMethod |> Meth.addParamRef typ partName |> ignore
            partName |> overrideFunc |> List.iter (fun s -> serviceMethod |> Meth.addStmt s |> ignore)
            serviceMethod
            |> Meth.addStmt (Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf typ; Expr.var (partName + "Overrides"); Arr.createOfSize<Type> 0; Expr.var (partName + "Root"); Expr.nil ]))
            |> Meth.addExpr ((Expr.var serializerName @-> "Serialize") @% [Expr.var "writer"; Expr.var partName])
            |> ignore)

        let deserializePartsExpr =
            responseParameters
            |> List.mapi (fun i ((runtimeType, overrideFunc), partName) ->
                let serializerName = partName + "Serializer"
                let typ = runtimeType.AsCodeTypeReference()

                let deserializeExpr =
                    (overrideFunc partName) @
                        [ Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf typ; Expr.var (partName + "Overrides"); Arr.createOfSize<Type> 0; Expr.var (partName + "Root"); Expr.nil])
                          Stmt.assign (Expr.var(sprintf "v%d" i)) (Expr.cast typ ((Expr.var serializerName @-> "Deserialize") @% [Expr.var "reader"])) ]

                let deserializeExpr =
                    if partName = "keha" && undescribedFaults then
                      [ (Expr.var "reader" @-> "SetBookmark") @% [Expr.value "keha"] |> Stmt.ofExpr
                        Stmt.condIfElse ((Expr.this @-> "MoveToElement") @% [Expr.var "reader"; Expr.value "faultCode"; Expr.value ""; Expr.value 4])
                                        [ (Expr.var "reader" @-> "ReturnToAndRemoveBookmark") @% [Expr.value "keha"] |> Stmt.ofExpr
                                          Stmt.throw<Exception> [(Expr.var "reader" @-> "ReadInnerXml") @% []] ]
                                        ([ (Expr.var "reader" @-> "ReturnToAndRemoveBookmark") @% [Expr.value "keha"] |> Stmt.ofExpr ] @ deserializeExpr) ]
                    else deserializeExpr

                Stmt.condIf (Op.equals (Expr.var "reader" @=> "LocalName")
                                       (Expr.value partName))
                            deserializeExpr)

        serviceMethod
        |> iif (operation.Style = RpcEncoded) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteEndElement") @% []))
        |> Meth.addExpr (Expr.code "}")
        |> Meth.addStmt (Stmt.declVarRefWith (CodeTypeReference("System.Func", typeRef<XmlReader>, returnType)) "readBody" (Expr.code "(r) => { //"))
        |> Meth.addStmt (if undescribedFaults
                         then Stmt.declVarRefWith (typeRefName "XmlBookmarkReader") "reader" (Expr.cast (typeRefName "XmlBookmarkReader") (Expr.var "r"))
                         else Stmt.declVarWith<XmlReader> "reader" (Expr.var "r"))
        |> Meth.addStmt (Stmt.condIf (Op.boolOr (Op.notEquals (Expr.var "reader" @=> "LocalName")
                                                              (Expr.value operation.Response.Name.LocalName))
                                                (Op.notEquals (Expr.var "reader" @=> "NamespaceURI")
                                                              (Expr.value operation.Response.Body.Namespace)))
                                     [Stmt.throw<Exception> [Expr.value "Invalid response message."]])
        |> ignore

        responseParameters
        |> List.iteri (fun i ((runtimeType,_),_) ->
            let typ = runtimeType.AsCodeTypeReference()
            serviceMethod |> Meth.addStmt (Stmt.declVarRefWith typ (sprintf "v%d" i) Expr.nil) |> ignore)

        serviceMethod
        |> Meth.addStmt (Stmt.whileLoop ((Expr.this @-> "MoveToElement") @% [Expr.var "reader"; Expr.nil; Expr.nil; Expr.value 3]) deserializePartsExpr)
        |> Meth.addStmt (Stmt.ret returnExpr)
        |> Meth.addExpr (Expr.code "}")
        |> ignore

        let attachmentsExpr =
            if isMultipart then
                serviceMethod |> Meth.addParam<IDictionary<string,Stream>> "attachments" |> ignore
                Expr.var "attachments"
            else Expr.nil

        match operation.Documentation.TryGetValue("et") with
        | true, doc -> serviceMethod.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
        | _ -> ()

        let methodCall = ((Expr.parent @-> "MakeServiceCall") @<> [returnType]) @% [attachmentsExpr; Expr.var "writeHeader"; Expr.var "writeBody"; Expr.var "readBody"]

        if responseParameters.IsEmpty then serviceMethod |> Meth.addExpr methodCall |> ignore
        else serviceMethod |> Meth.addStmt (Stmt.ret methodCall) |> ignore 

        serviceMethod

    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Types)
    |> Seq.choose (fun x ->
        match context.GetRuntimeType(SchemaType(x.Key)) with
        | CollectionType(prtyp, _, Some(st)) -> Some(prtyp, st)
        | CollectionType(_, _, None) -> None
        | rtyp -> Some(rtyp, x.Value))
    |> Seq.iter buildType

    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Elements)
    |> Seq.choose (fun x ->
        match x.Value.Type with
        | Definition(_) -> Some(context.GetRuntimeType(SchemaElement(x.Key)), x.Value)
        | _ -> None)
    |> Seq.iter buildElementType

    let targetClass = Cls.create typeNamePath.[typeNamePath.Length - 1] |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

    if undescribedFaults then
        targetClass.Members.Add(createXmlBookmarkReaderType()) |> ignore

    targetClass.Members.Add(portBaseTy) |> ignore
    targetClass.Members.Add(serviceTypesTy) |> ignore
    targetClass.Members.Add(createBinaryContentType()) |> ignore

    schema.Services |> List.iter (fun service ->
        let serviceTy = Cls.create service.Name |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic
        service.Ports |> List.iter (fun port ->
            let portTy = CodeTypeDeclaration(port.Name, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTy.Members.Add(portTy) |> ignore

            portTy |> Cls.setParent (typeRefName portBaseTy.Name) |> ignore

            match port.Documentation.TryGetValue("et") with
            | true, doc -> portTy.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
            | _ -> ()

            let ctor = CodeConstructor()
            ctor.Attributes <- MemberAttributes.Public
            ctor.BaseConstructorArgs.Add(CodePrimitiveExpression(port.Address)) |> ignore
            ctor.BaseConstructorArgs.Add(CodePrimitiveExpression(port.Producer)) |> ignore
            portTy.Members.Add(ctor) |> ignore

            port.Operations |> List.iter (buildOperationService >> portTy.Members.Add >> ignore))
        targetClass.Members.Add(serviceTy) |> ignore)

    context.CachedNamespaces |> Seq.iter (fun kvp -> kvp.Value |> serviceTypesTy.Members.Add |> ignore)

    let codeNamespace = CodeNamespace(String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1)))
    codeNamespace.Types.Add(targetClass) |> ignore

    let assembly = Compiler.buildAssembly(codeNamespace)
    assembly.GetType(sprintf "%s.%s" codeNamespace.Name targetClass.Name)
