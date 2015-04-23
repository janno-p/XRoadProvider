module private XRoad.ProducerDefinition

open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open System.Xml.Serialization

open XRoad.CodeDom
open XRoad.Parser
open XRoad.Parser.XsdSchema

[<AutoOpen>]
module String =
    let join (sep: string) (arr: seq<'T>) = System.String.Join(sep, arr)

    type String with
        member this.toClassName() =
            let str =
                match this.StartsWith("http://") with
                | true -> this.Substring(7)
                | _ -> this
            let className =
                str.Split('/')
                |> Array.map (fun p ->
                    p.Split('.')
                    |> Array.map (fun x -> CultureInfo.InvariantCulture.TextInfo.ToTitleCase(x.ToLower()).Replace("-", ""))
                    |> join "")
                |> join "_"
            if not <| CodeGenerator.IsValidLanguageIndependentIdentifier(className)
            then failwithf "invalid name %s" className
            className

type RuntimeType =
    | PrimitiveType of Type
    | ProvidedType of CodeTypeDeclaration * string
    | CollectionType of RuntimeType * string
    member this.AsCodeTypeReference() = match this with
                                        | PrimitiveType(typ) -> CodeTypeReference(typ)
                                        | ProvidedType(_,name) -> CodeTypeReference(name)
                                        | CollectionType(typ,_) -> CodeTypeReference(typ.AsCodeTypeReference(), 1)

type TypeBuilderContext =
    { CachedTypes: Dictionary<XName,CodeTypeDeclaration*string>
      CachedNamespaces: Dictionary<XNamespace,CodeTypeDeclaration>
      Attributes: Map<string,AttributeSpec>
      Elements: Map<string,ElementSpec>
      Style: OperationStyle }
    with
        member this.GetOrCreateNamespace(nsname: XNamespace) =
            let (|Producer|_|) ns =
                match Regex.Match(ns, @"^http://(((?<producer>\w+)\.x-road\.ee/producer(/(?<path>.*))?)|(producers\.\w+\.xtee\.riik\.ee/producer/(?<producer>\w+)(/(?<path>.*))?))$") with
                | m when m.Success ->
                    let suffix =
                        if m.Groups.["path"].Success then sprintf "_%s" <| m.Groups.["path"].Value.toClassName()
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
        member this.GetOrCreateType(name: XName) =
            match this.CachedTypes.TryGetValue(name) with
            | false, _ ->
                let typ = Cls.create(name.LocalName) |> Cls.addAttr TypeAttributes.Public
                let nstyp = this.GetOrCreateNamespace(name.Namespace)
                nstyp.Members.Add(typ) |> ignore
                let info = typ, (sprintf "DefinedTypes.%s.%s" nstyp.Name typ.Name)
                this.CachedTypes.Add(name, info)
                info
            | true, info -> info
        member this.GetRuntimeType(name: XName) =
            match mapPrimitiveType name with
            | Some typ -> PrimitiveType(typ)
            | None -> ProvidedType(this.GetOrCreateType(name))
        member this.GetElementSpec(name: XName) =
            match this.Elements.TryFind(name.ToString()) with
            | Some(elementSpec) -> elementSpec
            | None -> failwithf "Invalid reference: global element %A was not found in current context." name
        member this.GetAttributeDefinition(spec) =
            let rec findAttributeDefinition (spec: AttributeSpec) =
                match spec.RefOrType with
                | Reference(ref) ->
                    match this.Attributes.TryFind(ref.ToString()) with
                    | Some(spec) -> findAttributeDefinition(spec)
                    | None -> failwithf "Missing referenced attribute %A." ref
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
                  |> Seq.collect (fun (ns, typ) -> typ.Attributes |> Seq.map (fun x -> x.Key.ToString(), x.Value))
                  |> Map.ofSeq
              Elements =
                  schema.TypeSchemas
                  |> Map.toSeq
                  |> Seq.collect (fun (ns, typ) -> typ.Elements |> Seq.map (fun x -> x.Key.ToString(), x.Value))
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
let private defaultNonceMethod =
    Meth.create "GenerateNonce"
    |> Meth.setAttr MemberAttributes.Private
    |> Meth.returns<string>
    |> Meth.addStmt (Stmt.declVar<byte[]> "nonce" (Some (Arr.create<byte> [Expr.value 42])))
    |> Meth.addStmt (Stmt.declVar<Security.Cryptography.RandomNumberGenerator> "rng" <| Some((Expr.typeRefOf<Security.Cryptography.RNGCryptoServiceProvider> @-> "Create") []))
    |> Meth.addExpr ((Expr.var "rng" @-> "GetNonZeroBytes") [Expr.var("nonce")])
    |> Meth.addStmt (Stmt.ret ((Expr.typeRefOf<Convert> @-> "ToBase64String") [Expr.var "nonce"]))

let (!~~) x = x :> CodeStatement
let (!~>) x = !~~ CodeExpressionStatement(x)

module private Stat =
    type IfThenElse = CodeConditionStatement
    type Return = CodeMethodReturnStatement
    type Snip = CodeSnippetStatement
    type Throw = CodeThrowExceptionStatement
    type TryCatch = CodeTryCatchFinallyStatement

    let While (testExpression, [<ParamArray>] statements) =
        CodeIterationStatement(Snip(), testExpression, Snip(), statements)

module private Expr =
    type Call = CodeMethodInvokeExpression
    type CallOp = CodeBinaryOperatorExpression
    type Field = CodeFieldReferenceExpression
    type Invoke = CodeDelegateInvokeExpression
    type NewArray = CodeArrayCreateExpression
    type NewObject = CodeObjectCreateExpression
    type Op = CodeBinaryOperatorType
    type Param = CodeParameterDeclarationExpression
    type Prop = CodePropertyReferenceExpression
    type Snip = CodeSnippetExpression
    type Value = CodePrimitiveExpression
    type Type = CodeTypeReferenceExpression

let private writeXRoadHeaderMethod (style) =
    let hdrns, hdrName, propName, nsprefix =
        match style with
        | DocLiteral -> XmlNamespace.XRoad, headerMapping >> snd >> fst3, headerMapping >> snd >> snd3, "xrd"
        | RpcEncoded -> XmlNamespace.Xtee, headerMapping >> fst >> fst3, headerMapping >> fst >> snd3, "xtee"
    let writerVar = Expr.var "writer"
    let writeHeaderElement name propVar methodName =
        let reqHeaderContainsNameExpr = ((Expr.var "requiredHeaders") @-> "Contains") [Expr.value name]
        let propNotNullExpr = Op.isNotNull propVar
        Stmt.condIf (propNotNullExpr |> Op.boolOr reqHeaderContainsNameExpr)
                    [ Stmt.ofExpr ((writerVar @-> "WriteStartElement") [Expr.value name; Expr.value hdrns])
                      Stmt.condIf propNotNullExpr [ Stmt.ofExpr ((writerVar @-> methodName) [propVar]) ]
                      Stmt.ofExpr ((writerVar @-> "WriteEndElement") []) ]
    let declareWithDefaultValue name xtname defExpr (m: CodeMemberMethod) =
        m |> Meth.addStmt (Stmt.declVar<string> name None)
          |> Meth.addStmt (Stmt.condIfElse (Op.isNull (Expr.this @~> (propName xtname)))
                                           [Stmt.assign (Expr.var name) defExpr]
                                           [Stmt.assign (Expr.var name) (Expr.this @~> (propName xtname))])
    Meth.create "WriteHeader"
    |> Meth.setAttr (MemberAttributes.Family ||| MemberAttributes.Final)
    |> Meth.addParam<XmlWriter> "writer"
    |> Meth.addParam<string> "serviceName"
    |> Meth.addParam<IList<string>> "requiredHeaders"
    |> Meth.addExpr ((writerVar @-> "WriteAttributeString") [Expr.value "xmlns"; Expr.value nsprefix; Expr.value null; Expr.value hdrns])
    |> declareWithDefaultValue "producerValue" "andmekogu" (Expr.var "producerName")
    |> declareWithDefaultValue "requestId" "id" ((Expr.this @-> "GenerateNonce") [])
    |> declareWithDefaultValue "fullServiceName" "nimi" ((Expr.typeRefOf<string> @-> "Format") [Expr.value "{0}.{1}"; Expr.var "producerValue"; Expr.var "serviceName"])
    |> Meth.addStmt (writeHeaderElement (hdrName "asutus") (Expr.this @~> (propName "asutus")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "andmekogu") (Expr.var "producerValue") "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "isikukood") (Expr.this @~> (propName "isikukood")) "WriteString")
    |> iif (style = RpcEncoded) (fun m -> m |> Meth.addStmt (writeHeaderElement (hdrName "ametnik") (Expr.this @~> (propName "ametnik")) "WriteString"))
    |> Meth.addStmt (writeHeaderElement (hdrName "id") (Expr.var "requestId") "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "nimi") (Expr.var "fullServiceName") "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "toimik") (Expr.this @~> (propName "toimik")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "allasutus") (Expr.this @~> (propName "allasutus")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "amet") (Expr.this @~> (propName "amet")) "WriteRaw")
    |> Meth.addStmt (writeHeaderElement (hdrName "ametniknimi") (Expr.this @~> (propName "ametniknimi")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "asynkroonne") (Expr.this @~> (propName "asynkroonne")) "WriteValue")
    |> Meth.addStmt (writeHeaderElement (hdrName "autentija") (Expr.this @~> (propName "autentija")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "makstud") (Expr.this @~> (propName "makstud")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "salastada") (Expr.this @~> (propName "salastada")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "salastada_sertifikaadiga") (Expr.this @~> (propName "salastada_sertifikaadiga")) "WriteString")

let private makeServicePortBaseType(undescribedFaults, style: OperationStyle) =
    let portBaseTy = Cls.create "AbstractServicePort" |> Cls.setAttr (TypeAttributes.Public ||| TypeAttributes.Abstract)

    let addressField = CodeMemberField(typeof<string>, "producerUri")
    let addressFieldRef = Expr.Field(Expr.this, addressField.Name)
    let addressProperty = CodeMemberProperty(Name="ProducerUri", Type=CodeTypeReference(typeof<string>))
    addressProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    addressProperty.GetStatements.Add(Stat.Return(addressFieldRef)) |> ignore
    addressProperty.SetStatements.Add(Stmt.assign addressFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

    let producerField = CodeMemberField(typeof<string>, "producerName")
    let producerFieldRef = Expr.Field(Expr.this, producerField.Name)
    let producerProperty = CodeMemberProperty(Name="ProducerName", Type=CodeTypeReference(typeof<string>))
    producerProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    producerProperty.GetStatements.Add(Stat.Return(producerFieldRef)) |> ignore
    producerProperty.SetStatements.Add(Stmt.assign producerFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

    let ctor = CodeConstructor()
    ctor.Attributes <- MemberAttributes.Family
    ctor.Parameters.Add(Expr.Param(typeof<string>, "producerUri")) |> ignore
    ctor.Parameters.Add(Expr.Param(typeof<string>, "producerName")) |> ignore
    ctor.Statements.Add(Stmt.assign (Expr.Field(Expr.this, "producerUri")) (Expr.var("producerUri"))) |> ignore
    ctor.Statements.Add(Stmt.assign (Expr.Field(Expr.this, "producerName")) (Expr.var("producerName"))) |> ignore

    let moveToElementMeth = CodeMemberMethod(Name="MoveToElement")
    moveToElementMeth.Attributes <- MemberAttributes.Family
    moveToElementMeth.ReturnType <- CodeTypeReference(typeof<bool>)
    moveToElementMeth |> Meth.addParam<XmlReader> "reader"
                      |> Meth.addParam<string> "name"
                      |> Meth.addParam<string> "ns"
                      |> Meth.addParam<int> "depth"
                      |> Meth.addStmt (Stat.While(Expr.Value(true) :> CodeExpression,
                                                   [| !~~ Stat.IfThenElse(Expr.CallOp(Expr.CallOp(Expr.Prop(Expr.var("reader"), "Depth"), Expr.Op.IdentityEquality, Expr.var("depth")),
                                                                                      Expr.Op.BooleanAnd,
                                                                                      Expr.CallOp(Expr.CallOp(Expr.Prop(Expr.var("reader"), "NodeType"), Expr.Op.IdentityEquality, Expr.Prop(Expr.Type(typeof<XmlNodeType>), "Element")),
                                                                                                  Expr.Op.BooleanAnd,
                                                                                                  Expr.CallOp(Expr.CallOp(Expr.var("name"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                                                                                              Expr.Op.BooleanOr,
                                                                                                              Expr.CallOp(Expr.CallOp(Expr.Prop(Expr.var("reader"), "LocalName"), Expr.Op.IdentityEquality, Expr.var("name")),
                                                                                                                          Expr.Op.BooleanAnd,
                                                                                                                          Expr.CallOp(Expr.Prop(Expr.var("reader"), "NamespaceURI"), Expr.Op.IdentityEquality, Expr.var("ns")))))),
                                                                          Stat.Return(Expr.Value(true)))
                                                      !~~ Stat.IfThenElse(Expr.CallOp(Expr.Call(Expr.var("reader"), "Read"),
                                                                                      Expr.Op.BooleanAnd,
                                                                                      Expr.CallOp(Expr.Prop(Expr.var("reader"), "Depth"), Expr.Op.GreaterThanOrEqual, Expr.var("depth"))),
                                                                          [| |],
                                                                          [| !~~ Stat.Return(Expr.Value(false)) |]) |]))
                      |> Meth.addStmt (Stat.Return(Expr.Value(false)))
                      |> ignore

    let serviceCallMeth = CodeMemberMethod(Name="MakeServiceCall")
    serviceCallMeth.TypeParameters.Add("T")
    serviceCallMeth.Attributes <- MemberAttributes.Family ||| MemberAttributes.Final
    serviceCallMeth.ReturnType <- CodeTypeReference("T")

    let writerStatements = [|
        Stmt.assign (Expr.var("stream")) (Expr.Call(Expr.var("request"), "GetRequestStream"))
        Stmt.declVar<XmlWriter> "writer" (Some Expr.nil)
        !~~ Stat.TryCatch(
                [| Stmt.assign (Expr.var("writer")) (Expr.Call(Expr.Type(typeof<XmlWriter>), "Create", Expr.var("stream")))
                   !~> Expr.Call(Expr.var("writer"), "WriteStartDocument")
                   !~> Expr.Call(Expr.var("writer"), "WriteStartElement", Expr.Value("soapenv"), Expr.Value("Envelope"), Expr.Value(XmlNamespace.SoapEnvelope))
                   !~> Expr.Call(Expr.var("writer"), "WriteStartElement", Expr.Value("Header"), Expr.Value(XmlNamespace.SoapEnvelope))
                   !~> Expr.Invoke(Expr.var("writeHeaderAction"), Expr.var("writer"))
                   !~> Expr.Call(Expr.var("writer"), "WriteEndElement")
                   !~> Expr.Call(Expr.var("writer"), "WriteStartElement", Expr.Value("Body"), Expr.Value(XmlNamespace.SoapEnvelope))
                   !~> Expr.Invoke(Expr.var("writeBody"), Expr.var("writer"))
                   !~> Expr.Call(Expr.var("writer"), "WriteEndElement")
                   !~> Expr.Call(Expr.var("writer"), "WriteEndElement")
                   !~> Expr.Call(Expr.var("writer"), "WriteEndDocument") |],
                [| |],
                [| Stat.IfThenElse(Expr.CallOp(Expr.var("writer"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.var("writer"), "Dispose")) |]) |]

    let xmlReaderTypRef = if undescribedFaults then CodeTypeReference("XmlBookmarkReader") else CodeTypeReference(typeof<XmlReader>)

    let createReaderExpr =
        let readerExpr = Expr.Call(Expr.Type(typeof<XmlReader>), "Create", Expr.Call(Expr.var("response"), "GetResponseStream")) :> CodeExpression
        if undescribedFaults then Expr.NewObject(xmlReaderTypRef, readerExpr) :> CodeExpression else readerExpr

    let readerStatements = [|
        Stmt.assign (Expr.var("response")) (Expr.Call(Expr.var("request"), "GetResponse"))
        Stmt.declVarRef xmlReaderTypRef "reader" (Some Expr.nil)
        !~~ Stat.TryCatch(
                [| Stmt.assign (Expr.var("reader")) createReaderExpr
                   Stat.IfThenElse(Expr.Call(Expr.this, "MoveToElement", Expr.var("reader"), Expr.Value("Envelope"), Expr.Value(XmlNamespace.SoapEnvelope), Expr.Value(0)),
                                   [| |],
                                   [| !~~ Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Value("Soap envelope element was not found in response message."))) |])
                   Stat.IfThenElse(Expr.Call(Expr.this, "MoveToElement", Expr.var("reader"), Expr.Value("Body"), Expr.Value(XmlNamespace.SoapEnvelope), Expr.Value(1)),
                                   [| |],
                                   [| !~~ Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Value("Soap body element was not found in response message."))) |])
                   !~> Expr.Call(Expr.this, "MoveToElement", Expr.var("reader"), Expr.Value(null), Expr.Value(null), Expr.Value(2))
                   Stat.IfThenElse(
                        Expr.CallOp(
                            Expr.CallOp(Expr.Prop(Expr.var("reader"), "LocalName"), Expr.Op.IdentityEquality, Expr.Value("Fault")),
                            Expr.Op.BooleanAnd,
                            Expr.CallOp(Expr.Prop(Expr.var("reader"), "NamespaceURI"), Expr.Op.IdentityEquality, Expr.Value(XmlNamespace.SoapEnvelope))),
                        Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Call(Expr.var("reader"), "ReadInnerXml"))))
                   Stat.Return(Expr.Invoke(Expr.var("readBody"), Expr.var("reader"))) |],
                [| |],
                [| Stat.IfThenElse(Expr.CallOp(Expr.var("reader"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.var("reader"), "Dispose")) |]) |]

    serviceCallMeth |> Meth.addParam<Action<XmlWriter>> "writeHeaderAction"
                    |> Meth.addParam<Action<XmlWriter>> "writeBody"
                    |> Meth.addParamRef (CodeTypeReference("System.Func", typeRef<XmlReader>, CodeTypeReference("T"))) "readBody"
                    |> Meth.addStmt (Stmt.declVar<Net.WebRequest> "request" <| Some(Expr.Call(Expr.Type(typeof<Net.WebRequest>), "Create", Expr.var("producerUri")) :> CodeExpression))
                    |> Meth.addStmt (Stmt.assign (Expr.Prop(Expr.var("request"), "Method")) (Expr.Value("POST")))
                    |> Meth.addStmt (Stmt.assign (Expr.Prop(Expr.var("request"), "ContentType")) (Expr.Value("text/xml; charset=utf-8")))
                    |> Meth.addExpr (Expr.Call(Expr.Prop(Expr.var("request"), "Headers"), "Set", Expr.Value("SOAPAction"), Expr.Value("")))
                    |> Meth.addStmt (Stmt.declVar<IO.Stream> "stream" (Some Expr.nil))
                    |> Meth.addStmt (Stat.TryCatch(writerStatements,
                                                   [| |],
                                                   [| Stat.IfThenElse(Expr.CallOp(Expr.var("stream"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.var("stream"), "Dispose")) |]))
                    |> Meth.addStmt (Stmt.declVar<Net.WebResponse> "response" (Some Expr.nil))
                    |> Meth.addStmt (Stat.TryCatch(readerStatements,
                                                   [| |],
                                                   [| Stat.IfThenElse(Expr.CallOp(Expr.var("response"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.var("response"), "Dispose")) |]))
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
                (typ, Expr.NewObject(typ, tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i)) |> Array.ofList) :> CodeExpression)
        | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
        | x::xs -> let inner = getReturnTypeTuple([x], xs)
                   let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
                   (typ, Expr.NewObject(typ, ((tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i))) @ [snd inner]) |> Array.ofList) :> CodeExpression)
    match types |> List.mapi (fun i x -> (i, x)) with
    | [] -> (CodeTypeReference(typeof<Void>), Expr.var("???"))
    | (i,tp)::[] -> (tp.AsCodeTypeReference(), Expr.var(sprintf "v%d" i))
    | many -> getReturnTypeTuple([], many)

let buildParameterType (context: TypeBuilderContext) (part: MessagePart) =
    match context.Style, part.Reference with
    | DocLiteral, SchemaType(t) ->
        failwithf "Document/Literal style message part '%s' should reference global element as message part, but type '%s' is used instead" part.Name t.LocalName
    | RpcEncoded, SchemaElement(e) ->
        failwithf "RPC/Encoded style message part '%s' should reference global type as message part, but element '%s' is used instead" part.Name e.LocalName
    | DocLiteral, SchemaElement(elementName) ->
        let elementName, typeDefinition = context.GetElementDefinition(context.GetElementSpec(elementName))
        let typeDeclaration = Cls.create("Test") |> Cls.setAttr TypeAttributes.Public
        ProvidedType(typeDeclaration, "Test"), []
    | RpcEncoded, SchemaType(typeName) ->
        context.GetRuntimeType(typeName), [Expr.inst<XmlRootAttribute> [Expr.value part.Name]]

let makeProducerType (typeNamePath: string [], producerUri, undescribedFaults) =
    let schema = resolveUri producerUri |> readSchema
    let context = TypeBuilderContext.FromSchema(schema)

    let portBaseTy = makeServicePortBaseType(undescribedFaults, context.Style)
    let serviceTypesTy = Cls.create "DefinedTypes" |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

    let (|ArrayType|_|) (attributes: AttributeSpec list) =
        attributes |> List.tryFind (fun a -> a.Name = Some("arrayType") || a.RefOrType = Reference(XName.Get("arrayType", XmlNamespace.SoapEncoding)))

    let (|SoapEncArray|_|) (def: SchemaType) =
        match def with SchemaType.ComplexType(x) -> Some(x) | _ -> None
        |> Option.bind (fun x ->
            match x.Content with
            | ComplexTypeContent.ComplexContent(Restriction(c))
                when c.Base.LocalName = "Array" && c.Base.NamespaceName = XmlNamespace.SoapEncoding -> Some(c.Content)
            | _ -> None)

    let getArrayItemElement particle =
        match particle with
        | Some(ComplexTypeParticle.Sequence(spec)) ->
            match spec.Content with
            | [ SequenceContent.Element(e) ] -> Some(e)
            | _ -> None
        | Some(ComplexTypeParticle.All(spec)) ->
            match spec.Elements with
            | [ e ] -> Some(e)
            | _ -> None
        | _ -> None

    let rec buildType(providedTy: CodeTypeDeclaration, typeInfo) =
        let addProperty(name, ty: RuntimeType, isOptional) =
            let specifiedField =
                if isOptional then
                    let f = CodeMemberField(typeof<bool>, name + "__specified")
                    f.CustomAttributes.Add(Attributes.DebuggerBrowsable) |> ignore
                    providedTy.Members.Add(f) |> ignore
                    let p = CodeMemberProperty(Name=name + "Specified", Type=CodeTypeReference(typeof<bool>))
                    p.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
                    p.GetStatements.Add(Stat.Return(Expr.Field(Expr.this, f.Name))) |> ignore
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

        let buildProperty(name, ty: RuntimeType, isOptional): CodeTypeMember list =
            [
                let specifiedField =
                    if isOptional then
                        let f = CodeMemberField(typeof<bool>, name + "__specified")
                        f.CustomAttributes.Add(Attributes.DebuggerBrowsable) |> ignore
                        let p = CodeMemberProperty(Name=name + "Specified", Type=CodeTypeReference(typeof<bool>))
                        p.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
                        p.GetStatements.Add(Stat.Return(Expr.Field(Expr.this, f.Name))) |> ignore
                        Some(f, p)
                    else None
                let backingField = CodeMemberField(ty.AsCodeTypeReference(), name + "__backing")
                backingField.CustomAttributes.Add(Attributes.DebuggerBrowsable) |> ignore
                let backingFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), backingField.Name)
                let property = CodeMemberProperty(Name=name, Type=ty.AsCodeTypeReference())
                property.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
                property.GetStatements.Add(CodeMethodReturnStatement(backingFieldRef)) |> ignore
                property.SetStatements.Add(CodeAssignStatement(backingFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

                yield upcast property
                yield upcast backingField

                match specifiedField with
                | Some(field, prop) ->
                    let fieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), field.Name)
                    property.SetStatements.Add(CodeAssignStatement(fieldRef, CodePrimitiveExpression(true))) |> ignore
                    yield upcast field
                    yield upcast prop
                | _ -> ()
            ]

        let rec getAttributeType (schemaObject, name) =
            let convertedObject =
                match schemaObject with
                | Definition(simpleTypeSpec) -> Definition(SimpleType(simpleTypeSpec))
                | Name(name) -> Name(name)
                | Reference(ref) -> Reference(ref)
            getParticleType(convertedObject, 1u, false, name)

        and getArrayType (contentSpec: ComplexTypeContentSpec) =
            match contentSpec.Attributes with
            | ArrayType(attrSpec) ->
                match attrSpec.ArrayType with
                | Some(typeName, rank) ->
                    let itemName = getArrayItemElement(contentSpec.Content) |> Option.bind (fun x -> x.Name) |> Option.orDefault "item"
                    [1..rank] |> List.fold (fun aggType _ -> CollectionType(aggType, itemName)) (context.GetRuntimeType(typeName))
                | _ -> failwith "Array underlying type specification is missing."
            | _ ->
                match getArrayItemElement(contentSpec.Content) with
                | Some(elementSpec) ->
                    let elemName, elemType = context.GetElementDefinition(elementSpec)
                    let subTyName = providedTy.Name + "ArrayItem"
                    let elementTy, attrs = getParticleType(elemType, elementSpec.MaxOccurs, elementSpec.IsNillable, subTyName)
                    CollectionType(elementTy, elemName)
                | None -> failwith "Unsupported SOAP encoding array definition."

        and getParticleType (particleType, maxOccurs, isNillable, name) =
            match particleType with
            | Name(xname) ->
                let typ = context.GetRuntimeType(xname)
                match typ with
                | x when maxOccurs > 1u ->
                    CollectionType(x, name), [Attributes.XmlElement(true)]
                | PrimitiveType(x) when x.IsValueType ->
                    if isNillable then (PrimitiveType(typedefof<Nullable<_>>.MakeGenericType(x)), [Attributes.XmlElement(true)])
                    else (PrimitiveType(x), [Attributes.XmlElement(false)])
                | x -> (x, [Attributes.XmlElement(true)])
            | Definition(SoapEncArray(contentSpec)) ->
                getArrayType(contentSpec), [ Attributes.XmlArray(true); Attributes.XmlArrayItem("temp") ]
            | Definition(typeInfo) ->
                let subTy = Cls.create (name + "Type") |> Cls.addAttr TypeAttributes.Public
                buildType(subTy, typeInfo)
                providedTy.Members.Add(subTy) |> ignore
                let runtimeType = ProvidedType(subTy, subTy.Name)
                if maxOccurs > 1u
                then CollectionType(runtimeType, name), [Attributes.XmlElement(true)]
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
                    | SequenceContent.Choice(spec) ->
                        // TODO: Create choice type
                        ()
                    | SequenceContent.Element(spec) ->
                        parseElementSpec(spec)
                    | _ -> failwith "not implemented")
            | Some(ComplexTypeParticle.Choice(spec)) ->
                // TODO: Create choice type
                ()
            | None -> ()

        match typeInfo with
        | SoapEncArray(contentSpec) ->
            match getArrayType(contentSpec) with
            | CollectionType(elementType, itemName) as arrayType ->
                let property = addProperty("Array", arrayType, false)
                property.CustomAttributes.Add(Attributes.XmlElement2(itemName, elementType.AsCodeTypeReference())) |> ignore
            | _ -> failwith "not implemented"
        | SimpleType(SimpleTypeSpec.Restriction(spec)) ->
            match context.GetRuntimeType(spec.Base) with
            | PrimitiveType(typ) as rtyp ->
                let property = addProperty("BaseValue", rtyp, false)
                property.CustomAttributes.Add(Attributes.XmlText) |> ignore
                // TODO: Apply constraints?
            | _ -> failwith "not implemented"
        | SimpleType(Union(spec)) ->
            failwith "not implemented"
        | ComplexType(spec) ->
            if spec.IsAbstract then
                providedTy.TypeAttributes <- providedTy.TypeAttributes ||| TypeAttributes.Abstract
                providedTy.Members.Add(CodeConstructor(Attributes=MemberAttributes.Family)) |> ignore
            match spec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                match context.GetRuntimeType(spec.Base) with
                | PrimitiveType(typ) as rtyp ->
                    let property = addProperty("BaseValue", rtyp, false)
                    property.CustomAttributes.Add(Attributes.XmlText) |> ignore
                    parseComplexTypeContentSpec(spec.Content)
                | _ -> failwith "not implemented"
            | SimpleContent(SimpleContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                let baseTy, name = context.GetOrCreateType(spec.Base)
                providedTy.BaseTypes.Add(name) |> ignore
                baseTy.CustomAttributes.Add(Attributes.XmlInclude(providedTy)) |> ignore
                parseComplexTypeContentSpec(spec.Content)
            | ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexTypeContent.Particle(spec) ->
                parseComplexTypeContentSpec(spec)
        | EmptyType -> failwith "not implemented"

    let buildRootElementType (xname: XName) =
        match context.Elements.TryFind <| xname.ToString() with
        | Some(spec) ->
            match mapPrimitiveType xname with
            | Some tp -> PrimitiveType(tp)
            | _ -> ProvidedType(context.GetOrCreateType(xname))
        | _ -> failwithf "Missing global element definition: (%A)" xname

    let buildOperationService (operation: Operation) =
        let serviceMethod = Meth.create operation.Name.LocalName |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        let requestParameters = operation.Request.Body.Parts |> List.map (fun p -> p |> buildParameterType context, p.Name)
        let responseParameters = operation.Response.Body.Parts |> List.map (fun p -> p |> buildParameterType context, p.Name)
        let returnType, returnExpr = responseParameters |> List.map (fst >> fst) |> makeReturnType

        serviceMethod.ReturnType <- returnType

        let requiredHeadersExpr =
            Expr.NewArray(typeof<string>,
                          getRequiredHeaders(operation)
                            |> List.map(fun x -> Expr.Value(x) :> CodeExpression)
                            |> Array.ofList) :> CodeExpression

        let serviceName = match operation.Version with
                          | Some v -> sprintf "%s.%s" operation.Name.LocalName v
                          | _ -> operation.Name.LocalName

        // CodeDom doesn't support delegates, so we have to improvise
        serviceMethod |> Meth.addStmt (Stmt.declVar<string[]> "requiredHeaders" (Some requiredHeadersExpr))
                      |> Meth.addStmt (Stmt.declVar<Action<XmlWriter>> "writeHeader" <| Some(Expr.Snip("delegate(System.Xml.XmlWriter writer) { //") :> CodeExpression))
                      |> Meth.addExpr (Expr.Call(Expr.parent, "WriteHeader", Expr.var("writer"), Expr.Value(serviceName), Expr.var("requiredHeaders")))
                      |> Meth.addStmt (Stat.Snip("};"))
                      |> Meth.addStmt (Stmt.declVar<Action<XmlWriter>> "writeBody" <| Some(Expr.Snip("delegate(System.Xml.XmlWriter writer) { //") :> CodeExpression))
                      |> Meth.addExpr (Expr.Call(Expr.var("writer"), "WriteAttributeString", Expr.Value("xmlns"), Expr.Value("svc"), Expr.Value(null), Expr.Value(operation.Name.NamespaceName)))
                      |> iif (operation.Request.Body.Namespace <> operation.Name.NamespaceName) (fun x -> x |> Meth.addExpr (Expr.Call(Expr.var("writer"), "WriteAttributeString", Expr.Value("xmlns"), Expr.Value("svcns"), Expr.Value(null), Expr.Value(operation.Request.Body.Namespace))))
                      |> iif (operation.Style = RpcEncoded) (fun x -> x |> Meth.addExpr (Expr.Call(Expr.var("writer"), "WriteStartElement", Expr.Value(operation.Request.Name.LocalName), Expr.Value(operation.Request.Body.Namespace))))
                      |> ignore

        requestParameters
        |> List.iter (fun ((runtimeType, attributeOverrides), partName) ->
            let serializerName = partName + "Serializer"
            let typ = runtimeType.AsCodeTypeReference()
            serviceMethod
            |> Meth.addParamRef typ partName
            |> Meth.addStmt (Stmt.declVar<XmlSerializer> serializerName (Some(Expr.inst<XmlSerializer> ((Expr.typeOf typ) :: attributeOverrides))))
            |> Meth.addExpr ((Expr.var serializerName @-> "Serialize") [Expr.var "writer"; Expr.var partName])
            |> ignore)

        let deserializePartsExpr =
            responseParameters
            |> List.mapi (fun i ((runtimeType, attributeOverrides), partName) ->
                let serializerName = partName + "Serializer"
                let typ = runtimeType.AsCodeTypeReference()

                let deserializeExpr =
                    [| Stmt.declVar<XmlSerializer> serializerName (Some(Expr.inst<XmlSerializer> ((Expr.typeOf typ) :: attributeOverrides)))
                       Stmt.assign (Expr.var(sprintf "v%d" i)) (Expr.cast typ ((Expr.var serializerName @-> "Deserialize") [Expr.var "reader"])) |]

                let deserializeExpr =
                    if partName = "keha" && undescribedFaults then
                     [| !~> Expr.Call(Expr.var("reader"), "SetBookmark", Expr.Value("keha"))
                        !~~ Stat.IfThenElse(Expr.Call(Expr.this, "MoveToElement", Expr.var("reader"), Expr.Value("faultCode"), Expr.Value(""), Expr.Value(4)),
                                            [| !~> Expr.Call(Expr.var("reader"), "ReturnToAndRemoveBookmark", Expr.Value("keha"))
                                               !~~ Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Call(Expr.var("reader"), "ReadInnerXml"))) |],
                                            Array.concat [
                                                [| !~> Expr.Call(Expr.var("reader"), "ReturnToAndRemoveBookmark", Expr.Value("keha")) |]
                                                deserializeExpr
                                            ]) |]
                    else deserializeExpr

                !~~ Stat.IfThenElse(Expr.CallOp(Expr.Prop(Expr.var("reader"), "LocalName"), Expr.Op.IdentityEquality, Expr.Value(partName)),
                                    deserializeExpr))
            |> Array.ofList

        serviceMethod |> iif (operation.Style = RpcEncoded) (fun x -> x |> Meth.addExpr (Expr.Call(Expr.var("writer"), "WriteEndElement")))
                      |> Meth.addStmt (Stat.Snip("};"))
                      |> Meth.addStmt (Stmt.declVarRef (CodeTypeReference("System.Func", typeRef<XmlReader>, returnType)) "readBody" <| Some(Expr.Snip("delegate(System.Xml.XmlReader r) { //") :> CodeExpression))
                      |> Meth.addStmt (if undescribedFaults then Stmt.declVarRef (CodeTypeReference("XmlBookmarkReader")) "reader" <| Some(Expr.cast (typeRefName "XmlBookmarkReader") (Expr.var "r")) else Stmt.declVar<XmlReader> "reader" <| Some(Expr.var "r"))
                      |> Meth.addStmt (Stat.IfThenElse(Expr.CallOp(
                                                          Expr.CallOp(Expr.Prop(Expr.var("reader"), "LocalName"), Expr.Op.IdentityInequality, Expr.Value(operation.Response.Name.LocalName)),
                                                          Expr.Op.BooleanOr,
                                                          Expr.CallOp(Expr.Prop(Expr.var("reader"), "NamespaceURI"), Expr.Op.IdentityInequality, Expr.Value(operation.Response.Body.Namespace))),
                                                       Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Value("Invalid response message.")))))
                      |> ignore

        responseParameters
        |> List.iteri (fun i ((runtimeType, attributeOverrides), partName) ->
            let typ = runtimeType.AsCodeTypeReference()
            serviceMethod |> Meth.addStmt (Stmt.declVarRef typ (sprintf "v%d" i) (Some Expr.nil)) |> ignore)

        serviceMethod |> Meth.addStmt (Stat.While(Expr.Call(Expr.this, "MoveToElement", Expr.var("reader"), Expr.Value(null), Expr.Value(null), Expr.Value(3)), deserializePartsExpr))
                      |> Meth.addStmt (Stat.Return(returnExpr))
                      |> Meth.addStmt (Stat.Snip("};"))
                      |> ignore

        match operation.Documentation.TryGetValue("et") with
        | true, doc -> serviceMethod.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
        | _ -> ()

        let methodCall = Expr.Call(CodeMethodReferenceExpression(Expr.parent, "MakeServiceCall", returnType), Expr.var("writeHeader"), Expr.var("writeBody"), Expr.var("readBody"))

        if responseParameters.IsEmpty then serviceMethod |> Meth.addExpr methodCall |> ignore
        else serviceMethod |> Meth.addStmt (Stat.Return(methodCall)) |> ignore 

        serviceMethod

    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Types)
    |> Seq.map (fun x -> context.GetOrCreateType(x.Key) |> fst, x.Value)
    |> Seq.iter buildType

    let targetClass = Cls.create typeNamePath.[typeNamePath.Length - 1] |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

    if undescribedFaults then
        targetClass.Members.Add(createXmlBookmarkReaderType()) |> ignore

    targetClass.Members.Add(portBaseTy) |> ignore
    targetClass.Members.Add(serviceTypesTy) |> ignore

    schema.Services |> List.iter (fun service ->
        let serviceTy = Cls.create service.Name |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic
        service.Ports |> List.iter (fun port ->
            let portTy = CodeTypeDeclaration(port.Name, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTy.Members.Add(portTy) |> ignore

            portTy.BaseTypes.Add(CodeTypeReference(portBaseTy.Name)) |> ignore

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
