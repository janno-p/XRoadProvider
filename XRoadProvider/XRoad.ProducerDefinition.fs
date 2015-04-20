module private XRoad.ProducerDefinition

open Microsoft.CSharp

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

let (!~~) x = x :> CodeStatement
let (!~>) x = !~~ CodeExpressionStatement(x)

module private Stat =
    type Assign = CodeAssignStatement
    type IfThenElse = CodeConditionStatement
    type Return = CodeMethodReturnStatement
    type Snip = CodeSnippetStatement
    type Throw = CodeThrowExceptionStatement
    type TryCatch = CodeTryCatchFinallyStatement
    type Var = CodeVariableDeclarationStatement

    let Continue = CodeSnippetStatement("continue;")

    let ofExpr(expr) = CodeExpressionStatement(expr)

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
    type Var = CodeVariableReferenceExpression
    type Value = CodePrimitiveExpression
    type Type = CodeTypeReferenceExpression
    type TypeOf = CodeTypeOfExpression
    type Cast = CodeCastExpression

    let Base = CodeBaseReferenceExpression()
    let This = CodeThisReferenceExpression()

module private Method =
    let addParam param (meth: CodeMemberMethod) =
        meth.Parameters.Add(param) |> ignore
        meth

    let addStat (statement: CodeStatement) (meth: CodeMemberMethod) =
        meth.Statements.Add(statement) |> ignore
        meth

    let addExpr (expr: CodeExpression) (meth: CodeMemberMethod) =
        meth.Statements.Add(expr) |> ignore
        meth

type private RuntimeType =
    | PrimitiveType of Type
    | ProvidedType of CodeTypeReference
    member this.AsCodeTypeReference() = match this with
                                        | PrimitiveType(typ) -> CodeTypeReference(typ)
                                        | ProvidedType(typ) -> typ

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
                |> String.join "")
            |> String.join "_"
        if not <| CodeGenerator.IsValidLanguageIndependentIdentifier(className)
        then failwithf "invalid name %s" className
        className

let private compileAssembly code =
    let fileName = Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> sprintf "%A.dll")
    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(OutputAssembly=fileName, GenerateExecutable=false)
    //parameters.CompilerOptions <- "/doc:" + Path.ChangeExtension(fileName, "xml")
    ( use wr = new StreamWriter(File.Open(Path.ChangeExtension(fileName, "cs"), FileMode.Create, FileAccess.Write))
      codeProvider.GenerateCodeFromCompileUnit(code, wr, CodeGeneratorOptions()))
    let compilerResults = codeProvider.CompileAssemblyFromDom(parameters, [| code |])
    if compilerResults.Errors.Count > 0 then
        printfn "%A" compilerResults.Errors
    compilerResults.CompiledAssembly

let private makeStaticClass(className, attributes) =
    let targetClass = CodeTypeDeclaration(className)
    targetClass.IsClass <- true
    targetClass.TypeAttributes <- attributes
    targetClass.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" Environment.NewLine)) |> ignore
    targetClass.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
    targetClass

let private makePublicClass name =
    CodeTypeDeclaration(name, IsClass=true, TypeAttributes=TypeAttributes.Public)

let private makeGenerateNonceMethod() =
    let meth = CodeMemberMethod(Name="GenerateNonce")
    meth.Attributes <- MemberAttributes.Private
    meth.ReturnType <- CodeTypeReference(typeof<string>)
    meth |> Method.addStat (Stat.Var(typeof<byte[]>, "nonce", Expr.NewArray(typeof<byte>, Expr.Value(42))))
         |> Method.addStat (Stat.Var(typeof<Security.Cryptography.RandomNumberGenerator>, "rng", Expr.Call(Expr.Type(typeof<Security.Cryptography.RNGCryptoServiceProvider>), "Create")))
         |> Method.addExpr (Expr.Call(Expr.Var("rng"), "GetNonZeroBytes", Expr.Var("nonce")))
         |> Method.addStat (Stat.Return(Expr.Call(Expr.Type(typeof<Convert>), "ToBase64String", Expr.Var("nonce"))))

let private writeHeaderStatements xrdns (name, prop, methodName) =
    let writerRef = Expr.Var("writer")
    Stat.IfThenElse(Expr.CallOp(Expr.Call(Expr.Var("requiredHeaders"), "Contains", Expr.Value(name)),
                                Expr.Op.BooleanOr,
                                Expr.CallOp(prop, Expr.Op.IdentityInequality, Expr.Value(null))),
                    Expr.Call(writerRef, "WriteStartElement", Expr.Value(name), Expr.Value(xrdns)) |> Stat.ofExpr,
                    Stat.IfThenElse(Expr.CallOp(prop, Expr.Op.IdentityInequality, Expr.Value(null)),
                                    Expr.Call(writerRef, methodName, prop) |> Stat.ofExpr),
                    Expr.Call(writerRef, "WriteEndElement") |> Stat.ofExpr)

let private makeWriteXRoadRpcHeaderMethod() =
    let meth = CodeMemberMethod(Name="WriteRpcHeader")
    meth.Attributes <- MemberAttributes.Family ||| MemberAttributes.Final

    meth |> Method.addParam (Expr.Param(typeof<XmlWriter>, "writer"))
         |> Method.addParam (Expr.Param(typeof<string>, "serviceName"))
         |> Method.addParam (Expr.Param(typeof<IList<string>>, "requiredHeaders"))
         |> ignore

    let writerRef = Expr.Var("writer")
    let headerProp propName = Expr.Prop(Expr.This, propName)
    let writeRpcHeaderStatements = writeHeaderStatements XmlNamespace.Xtee

    meth |> Method.addExpr (Expr.Call(writerRef, "WriteAttributeString", Expr.Value("xmlns"), Expr.Value("xtee"), Expr.Value(null), Expr.Value(XmlNamespace.Xtee)))
         |> Method.addStat (Stat.Var(typeof<string>, "producerValue"))
         |> Method.addStat (Stat.IfThenElse(Expr.CallOp(headerProp("Andmekogu"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                      [| !~~ Stat.Assign(Expr.Var("producerValue"), Expr.Var("producer")) |],
                                      [| !~~ Stat.Assign(Expr.Var("producerValue"), headerProp("Andmekogu")) |]))
         |> Method.addStat (Stat.Var(typeof<string>, "requestId"))
         |> Method.addStat (Stat.IfThenElse(Expr.CallOp(headerProp("Id"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                      [| !~~ Stat.Assign(Expr.Var("requestId"), Expr.Call(Expr.This, "GenerateNonce")) |],
                                      [| !~~ Stat.Assign(Expr.Var("requestId"), headerProp("Id")) |]))
         |> Method.addStat (Stat.Var(typeof<string>, "fullServiceName"))
         |> Method.addStat (Stat.IfThenElse(Expr.CallOp(headerProp("Nimi"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                      [| !~~ Stat.Assign(Expr.Var("fullServiceName"), Expr.Call(Expr.Type(typeof<string>), "Format", Expr.Value("{0}.{1}"), Expr.Var("producerValue"), Expr.Var("serviceName"))) |],
                                      [| !~~ Stat.Assign(Expr.Var("fullServiceName"), headerProp("Nimi")) |]))
         |> Method.addStat (writeRpcHeaderStatements("asutus", headerProp("Asutus") :> CodeExpression, "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("andmekogu", Expr.Var("producerValue"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("isikukood", headerProp("Isikukood"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("ametnik", headerProp("Ametnik"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("id", Expr.Var("requestId"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("nimi", Expr.Var("fullServiceName"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("toimik", headerProp("Toimik"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("allasutus", headerProp("Allasutus"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("amet", headerProp("Amet"), "WriteRaw"))
         |> Method.addStat (writeRpcHeaderStatements("ametniknimi", headerProp("Ametniknimi"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("asynkroonne", headerProp("Asynkroonne"), "WriteValue"))
         |> Method.addStat (writeRpcHeaderStatements("autentija", headerProp("Autentija"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("makstud", headerProp("Makstud"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("salastada", headerProp("Salastada"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("salastada_sertifikaadiga", headerProp("SalastadaSertifikaadiga"), "WriteString"))

let private makeServicePortBaseType(undescribedFaults, style: OperationStyle) =
    let portBaseTy = makePublicClass("AbstractServicePort")
    portBaseTy.TypeAttributes <- portBaseTy.TypeAttributes ||| TypeAttributes.Abstract

    let addressField = CodeMemberField(typeof<string>, "address")
    let addressFieldRef = Expr.Field(Expr.This, addressField.Name)
    let addressProperty = CodeMemberProperty(Name="Address", Type=CodeTypeReference(typeof<string>))
    addressProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    addressProperty.GetStatements.Add(Stat.Return(addressFieldRef)) |> ignore
    addressProperty.SetStatements.Add(Stat.Assign(addressFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

    let producerField = CodeMemberField(typeof<string>, "producer")
    let producerFieldRef = Expr.Field(Expr.This, producerField.Name)
    let producerProperty = CodeMemberProperty(Name="Producer", Type=CodeTypeReference(typeof<string>))
    producerProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    producerProperty.GetStatements.Add(Stat.Return(producerFieldRef)) |> ignore
    producerProperty.SetStatements.Add(Stat.Assign(producerFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

    let nonceMeth = makeGenerateNonceMethod()
    let writeRpcHeaderMeth = makeWriteXRoadRpcHeaderMethod()

    let ctor = CodeConstructor()
    ctor.Attributes <- MemberAttributes.Family
    ctor.Parameters.Add(Expr.Param(typeof<string>, "address")) |> ignore
    ctor.Parameters.Add(Expr.Param(typeof<string>, "producer")) |> ignore
    ctor.Statements.Add(Stat.Assign(Expr.Field(Expr.This, "address"), Expr.Var("address"))) |> ignore
    ctor.Statements.Add(Stat.Assign(Expr.Field(Expr.This, "producer"), Expr.Var("producer"))) |> ignore

    let moveToElementMeth = CodeMemberMethod(Name="MoveToElement")
    moveToElementMeth.Attributes <- MemberAttributes.Family
    moveToElementMeth.ReturnType <- CodeTypeReference(typeof<bool>)
    moveToElementMeth |> Method.addParam (Expr.Param(typeof<XmlReader>, "reader"))
                      |> Method.addParam (Expr.Param(typeof<string>, "name"))
                      |> Method.addParam (Expr.Param(typeof<string>, "ns"))
                      |> Method.addParam (Expr.Param(typeof<int>, "depth"))
                      |> Method.addStat (Stat.While(Expr.Value(true) :> CodeExpression,
                                                     [| !~~ Stat.IfThenElse(Expr.CallOp(Expr.CallOp(Expr.Prop(Expr.Var("reader"), "Depth"), Expr.Op.IdentityEquality, Expr.Var("depth")),
                                                                                        Expr.Op.BooleanAnd,
                                                                                        Expr.CallOp(Expr.CallOp(Expr.Prop(Expr.Var("reader"), "NodeType"), Expr.Op.IdentityEquality, Expr.Prop(Expr.Type(typeof<XmlNodeType>), "Element")),
                                                                                                    Expr.Op.BooleanAnd,
                                                                                                    Expr.CallOp(Expr.CallOp(Expr.Var("name"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                                                                                                Expr.Op.BooleanOr,
                                                                                                                Expr.CallOp(Expr.CallOp(Expr.Prop(Expr.Var("reader"), "LocalName"), Expr.Op.IdentityEquality, Expr.Var("name")),
                                                                                                                            Expr.Op.BooleanAnd,
                                                                                                                            Expr.CallOp(Expr.Prop(Expr.Var("reader"), "NamespaceURI"), Expr.Op.IdentityEquality, Expr.Var("ns")))))),
                                                                            Stat.Return(Expr.Value(true)))
                                                        !~~ Stat.IfThenElse(Expr.CallOp(Expr.Call(Expr.Var("reader"), "Read"),
                                                                                        Expr.Op.BooleanAnd,
                                                                                        Expr.CallOp(Expr.Prop(Expr.Var("reader"), "Depth"), Expr.Op.GreaterThanOrEqual, Expr.Var("depth"))),
                                                                            [| |],
                                                                            [| !~~ Stat.Return(Expr.Value(false)) |]) |]))
                      |> Method.addStat (Stat.Return(Expr.Value(false)))
                      |> ignore

    let serviceCallMeth = CodeMemberMethod(Name="MakeServiceCall")
    serviceCallMeth.TypeParameters.Add("T")
    serviceCallMeth.Attributes <- MemberAttributes.Family ||| MemberAttributes.Final
    serviceCallMeth.ReturnType <- CodeTypeReference("T")

    let writerStatements = [|
        !~~ Stat.Assign(Expr.Var("stream"), Expr.Call(Expr.Var("request"), "GetRequestStream"))
        !~~ Stat.Var(typeof<XmlWriter>, "writer", Expr.Value(null))
        !~~ Stat.TryCatch(
                [| Stat.Assign(Expr.Var("writer"), Expr.Call(Expr.Type(typeof<XmlWriter>), "Create", Expr.Var("stream")))
                   !~> Expr.Call(Expr.Var("writer"), "WriteStartDocument")
                   !~> Expr.Call(Expr.Var("writer"), "WriteStartElement", Expr.Value("soapenv"), Expr.Value("Envelope"), Expr.Value(XmlNamespace.SoapEnvelope))
                   !~> Expr.Call(Expr.Var("writer"), "WriteStartElement", Expr.Value("Header"), Expr.Value(XmlNamespace.SoapEnvelope))
                   !~> Expr.Invoke(Expr.Var("writeHeaderAction"), Expr.Var("writer"))
                   !~> Expr.Call(Expr.Var("writer"), "WriteEndElement")
                   !~> Expr.Call(Expr.Var("writer"), "WriteStartElement", Expr.Value("Body"), Expr.Value(XmlNamespace.SoapEnvelope))
                   !~> Expr.Invoke(Expr.Var("writeBody"), Expr.Var("writer"))
                   !~> Expr.Call(Expr.Var("writer"), "WriteEndElement")
                   !~> Expr.Call(Expr.Var("writer"), "WriteEndElement")
                   !~> Expr.Call(Expr.Var("writer"), "WriteEndDocument") |],
                [| |],
                [| Stat.IfThenElse(Expr.CallOp(Expr.Var("writer"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.Var("writer"), "Dispose")) |]) |]

    let xmlReaderTypRef = if undescribedFaults then CodeTypeReference("XmlBookmarkReader") else CodeTypeReference(typeof<XmlReader>)

    let createReaderExpr =
        let readerExpr = Expr.Call(Expr.Type(typeof<XmlReader>), "Create", Expr.Call(Expr.Var("response"), "GetResponseStream")) :> CodeExpression
        if undescribedFaults then Expr.NewObject(xmlReaderTypRef, readerExpr) :> CodeExpression else readerExpr

    let readerStatements = [|
        !~~ Stat.Assign(Expr.Var("response"), Expr.Call(Expr.Var("request"), "GetResponse"))
        !~~ Stat.Var(xmlReaderTypRef, "reader", Expr.Value(null))
        !~~ Stat.TryCatch(
                [| Stat.Assign(Expr.Var("reader"), createReaderExpr)
                   Stat.IfThenElse(Expr.Call(Expr.This, "MoveToElement", Expr.Var("reader"), Expr.Value("Envelope"), Expr.Value(XmlNamespace.SoapEnvelope), Expr.Value(0)),
                                   [| |],
                                   [| !~~ Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Value("Soap envelope element was not found in response message."))) |])
                   Stat.IfThenElse(Expr.Call(Expr.This, "MoveToElement", Expr.Var("reader"), Expr.Value("Body"), Expr.Value(XmlNamespace.SoapEnvelope), Expr.Value(1)),
                                   [| |],
                                   [| !~~ Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Value("Soap body element was not found in response message."))) |])
                   !~> Expr.Call(Expr.This, "MoveToElement", Expr.Var("reader"), Expr.Value(null), Expr.Value(null), Expr.Value(2))
                   Stat.IfThenElse(
                        Expr.CallOp(
                            Expr.CallOp(Expr.Prop(Expr.Var("reader"), "LocalName"), Expr.Op.IdentityEquality, Expr.Value("Fault")),
                            Expr.Op.BooleanAnd,
                            Expr.CallOp(Expr.Prop(Expr.Var("reader"), "NamespaceURI"), Expr.Op.IdentityEquality, Expr.Value(XmlNamespace.SoapEnvelope))),
                        Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Call(Expr.Var("reader"), "ReadInnerXml"))))
                   Stat.Return(Expr.Invoke(Expr.Var("readBody"), Expr.Var("reader"))) |],
                [| |],
                [| Stat.IfThenElse(Expr.CallOp(Expr.Var("reader"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.Var("reader"), "Dispose")) |]) |]

    serviceCallMeth |> Method.addParam (Expr.Param(typeof<Action<XmlWriter>>, "writeHeaderAction"))
                    |> Method.addParam (Expr.Param(typeof<Action<XmlWriter>>, "writeBody"))
                    |> Method.addParam (Expr.Param(CodeTypeReference("System.Func", CodeTypeReference(typeof<XmlReader>), CodeTypeReference("T")), "readBody"))
                    |> Method.addStat (Stat.Var(typeof<Net.WebRequest>, "request", Expr.Call(Expr.Type(typeof<Net.WebRequest>), "Create", Expr.Var("address"))))
                    |> Method.addStat (Stat.Assign(Expr.Prop(Expr.Var("request"), "Method"), Expr.Value("POST")))
                    |> Method.addStat (Stat.Assign(Expr.Prop(Expr.Var("request"), "ContentType"), Expr.Value("text/xml; charset=utf-8")))
                    |> Method.addExpr (Expr.Call(Expr.Prop(Expr.Var("request"), "Headers"), "Set", Expr.Value("SOAPAction"), Expr.Value("")))
                    |> Method.addStat (Stat.Var(typeof<IO.Stream>, "stream", Expr.Value(null)))
                    |> Method.addStat (Stat.TryCatch(writerStatements,
                                                     [| |],
                                                     [| Stat.IfThenElse(Expr.CallOp(Expr.Var("stream"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.Var("stream"), "Dispose")) |]))
                    |> Method.addStat (Stat.Var(typeof<Net.WebResponse>, "response", Expr.Value(null)))
                    |> Method.addStat (Stat.TryCatch(readerStatements,
                                                     [| |],
                                                     [| Stat.IfThenElse(Expr.CallOp(Expr.Var("response"), Expr.Op.IdentityInequality, Expr.Value(null)), !~> Expr.Call(Expr.Var("response"), "Dispose")) |]))
                    |> ignore

    portBaseTy.Members.Add(ctor) |> ignore
    portBaseTy.Members.Add(addressField) |> ignore
    portBaseTy.Members.Add(addressProperty) |> ignore
    portBaseTy.Members.Add(producerField) |> ignore
    portBaseTy.Members.Add(producerProperty) |> ignore
    portBaseTy.Members.Add(serviceCallMeth) |> ignore
    portBaseTy.Members.Add(writeRpcHeaderMeth) |> ignore
    portBaseTy.Members.Add(nonceMeth) |> ignore
    portBaseTy.Members.Add(moveToElementMeth) |> ignore

    match style with
    | RpcEncoded ->
        portBaseTy |> createProperty<string>         "Asutus"                   "Asutuse DNS-nimi."
                   |> createProperty<string>         "Andmekogu"                "Andmekogu DNS-nimi."
                   |> createProperty<string>         "Isikukood"                "Teenuse kasutaja isikukood, millele eelneb kahekohaline maa kood. Näiteks EE37702026518."
                   |> createProperty<string>         "Ametnik"                  "Teenuse kasutaja Eesti isikukood (ei ole kasutusel alates versioonist 5.0)."
                   |> createProperty<string>         "Id"                       "Teenuse väljakutse nonss (unikaalne identifikaator)."
                   |> createProperty<string>         "Nimi"                     "Kutsutava teenuse nimi."
                   |> createProperty<string>         "Toimik"                   "Teenuse väljakutsega seonduva toimiku number (mittekohustuslik)."
                   |> createProperty<string>         "Allasutus"                "Asutuse registrikood, mille nimel teenust kasutatakse (kasutusel juriidilise isiku portaalis)."
                   |> createProperty<string>         "Amet"                     "Teenuse kasutaja ametikoht."
                   |> createProperty<string>         "Ametniknimi"              "Teenuse kasutaja nimi."
                   |> createProperty<Nullable<bool>> "Asynkroonne"              "Teenuse kasutamise asünkroonsus. Kui väärtus on 'true', siis sooritab turvaserver päringu asünkroonselt."
                   |> createProperty<string>         "Autentija"                "Teenuse kasutaja autentimise viis. Võimalikud variandid on: ID - ID-kaardiga autenditud; SERT - muu sertifikaadiga autenditud; PANK - panga kaudu autenditud; PAROOL - kasutajatunnuse ja parooliga autenditud. Autentimise viisi järel võib sulgudes olla täpsustus (näiteks panga kaudu autentimisel panga tunnus infosüsteemis)."
                   |> createProperty<string>         "Makstud"                  "Teenuse kasutamise eest makstud summa."
                   |> createProperty<string>         "Salastada"                "Kui asutusele on X-tee keskuse poolt antud päringute salastamise õigus ja andmekogu on nõus päringut salastama, siis selle elemendi olemasolul päringu päises andmekogu turvaserver krüpteerib päringu logi, kasutades selleks X-tee keskuse salastusvõtit."
                   |> createProperty<string>         "SalastadaSertifikaadiga"  "Päringu sooritaja ID-kaardi autentimissertifikaat DERkujul base64 kodeerituna. Selle elemendi olemasolu päringu päises väljendab soovi päringu logi salastamiseks asutuse turvaserveris päringu sooritaja ID-kaardi autentimisvõtmega. Seda välja kasutatakse ainult kodaniku päringute portaalis."
                   |> createProperty<string>         "Salastatud"               "Kui päringu välja päises oli element salastada ja päringulogi salastamine õnnestus, siis vastuse päisesse lisatakse tühi element salastatud."
                   |> createProperty<string>         "SalastatudSertifikaadiga" "Kui päringu päises oli element salastada_sertifikaadiga ja päringulogi salastamine õnnestus, siis vastuse päisesesse lisatakse tühi element salastatud_sertifikaadiga."
    | DocLiteral ->
        portBaseTy |> createProperty<string>         "Consumer"                 "DNS-name of the institution"
                   |> createProperty<string>         "Producer"                 "DNS-name of the database"
                   |> createProperty<string>         "UserId"                   "ID code of the person invoking the service, preceded by a two-letter country code. For example: EE37702026518"
                   |> createProperty<string>         "Id"                       "Service invocation nonce (unique identifier)"
                   |> createProperty<string>         "Service"                  "Name of the service to be invoked"
                   |> createProperty<string>         "Issue"                    "Name of file or document related to the service invocation"
                   |> createProperty<string>         "Unit"                     "Registration code of the institution or its unit on whose behalf the service is used (applied in the legal entity portal)"
                   |> createProperty<string>         "Position"                 "Organizational position or role of the person invoking the service"
                   |> createProperty<string>         "UserName"                 "Name of the person invoking the service"
                   |> createProperty<Nullable<bool>> "Async"                    "Specifies asynchronous service. If the value is \"true\", then the security server performs the service call asynchronously."
                   |> createProperty<string>         "Authenticator"            "Authentication method, one of the following: ID-CARD - with a certificate of identity; CERT - with another certificate; EXTERNAL - through a third-party service; PASSWORD - with user ID and a password. Details of the authentication (e.g. the identification of a bank for external authentication) can be given in brackets after the authentication method."
                   |> createProperty<string>         "Paid"                     "The amount of money paid for invoking the service"
                   |> createProperty<string>         "Encrypt"                  "If an organization has got the right from the X-Road Center to hide queries, with the database agreeing to hide the query, the occurrence of this tag in the query header makes the database security server to encrypt the query log, using the encryption key of the X-Road Center"
                   |> createProperty<string>         "EncryptCert"              "Authentication certificate of the query invokers ID Card, in the base64-encoded DER format. Occurrence of this tag in the query header represents the wish to encrypt the query log in the organizations security server, using authentication key of the query invokers ID Card. This field is used in the Citizen Query Portal only."
                   |> createProperty<string>         "Encrypted"                "If the query header contains the encrypt tag and the query log as been successfully encrypted, an empty encrypted tag will be inserted in the reply header."
                   |> createProperty<string>         "EncryptedCert"            "If the query header contains the encryptedCert tag and the query log has been successfully encrypted, an empty encryptedCert tag will accordingly be inserted in the reply header."

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

let private makeReturnType (types: RuntimeType list) =
    let rec getReturnTypeTuple (tuple: (int * RuntimeType) list, types) =
        match types with
        | [] -> let typ = CodeTypeReference("System.Tuple", tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference()) |> Array.ofList)
                (typ, Expr.NewObject(typ, tuple |> List.map (fun (i, _) -> Expr.Var(sprintf "v%d" i) :> CodeExpression) |> Array.ofList) :> CodeExpression)
        | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
        | x::xs -> let inner = getReturnTypeTuple([x], xs)
                   let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
                   (typ, Expr.NewObject(typ, ((tuple |> List.map (fun (i, _) -> Expr.Var(sprintf "v%d" i) :> CodeExpression)) @ [snd inner]) |> Array.ofList) :> CodeExpression)
    match types |> List.mapi (fun i x -> (i, x)) with
    | [] -> (CodeTypeReference(typeof<Void>), Expr.Var("???") :> CodeExpression)
    | (i,tp)::[] -> (tp.AsCodeTypeReference(), Expr.Var(sprintf "v%d" i) :> CodeExpression)
    | many -> getReturnTypeTuple([], many)

let makeProducerType (typeNamePath: string [], producerUri, undescribedFaults) =
    let schema = resolveUri producerUri |> readSchema
    let typeCache = Dictionary<XName,CodeTypeDeclaration>()
    let namespaceCache = Dictionary<XNamespace,CodeTypeDeclaration>()

    let style =
        let reduceStyle s1 s2 =
            if s1 <> s2
            then failwith "Mixing different style services is not allowed!"
            s1
        schema.Services
        |> List.map (fun svc -> svc.Ports |> List.map (fun p -> p.Style) |> List.reduce reduceStyle)
        |> List.reduce reduceStyle

    let portBaseTy = makeServicePortBaseType(undescribedFaults, style)
    let serviceTypesTy = makeStaticClass("DefinedTypes", TypeAttributes.Public)

    let attributeLookup =
        schema.TypeSchemas
        |> Map.toSeq
        |> Seq.collect (fun (ns, typ) -> typ.Attributes |> Seq.map (fun x -> x.Key.ToString(), x.Value))
        |> Map.ofSeq

    let elementLookup =
        schema.TypeSchemas
        |> Map.toSeq
        |> Seq.collect (fun (ns, typ) -> typ.Elements |> Seq.map (fun x -> x.Key.ToString(), x.Value))
        |> Map.ofSeq

    let (|Producer|_|) ns =
        match Regex.Match(ns, @"^http://(((?<producer>\w+)\.x-road\.ee/producer(/(?<path>.*))?)|(producers\.\w+\.xtee\.riik\.ee/producer/(?<producer>\w+)(/(?<path>.*))?))$") with
        | m when m.Success ->
            let suffix =
                if m.Groups.["path"].Success then sprintf "_%s" <| m.Groups.["path"].Value.toClassName()
                else ""
            Some(sprintf "%s%s" m.Groups.["producer"].Value suffix)
        | _ -> None

    let getOrCreateNamespace (name: XNamespace) =
        match namespaceCache.TryGetValue(name) with
        | false, _ ->
            let producerName =
                match name.NamespaceName with
                | Producer(producerName) -> producerName
                | XmlNamespace.Xtee -> "xtee"
                | XmlNamespace.XRoad -> "xroad"
                | ns -> ns.toClassName()
            let typ = CodeTypeDeclaration(producerName, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTypesTy.Members.Add(typ) |> ignore
            namespaceCache.Add(name, typ)
            typ
        | true, typ -> typ

    let getOrCreateType (name: XName) =
        match typeCache.TryGetValue(name) with
        | false, _ ->
            let typ = makePublicClass(name.LocalName)
            typ.CustomAttributes.Add(Attributes.XmlType(name)) |> ignore
            let namespaceTy = getOrCreateNamespace(name.Namespace)
            namespaceTy.Members.Add(typ) |> ignore
            typeCache.Add(name, typ)
            typ.UserData.Add("full_name", sprintf "DefinedTypes.%s.%s" namespaceTy.Name typ.Name)
            typ
        | true, typ -> typ

    let getRuntimeType typeName =
        match mapPrimitiveType typeName with
        | Some tp -> PrimitiveType(tp)
        | _ ->
            let tp = getOrCreateType(typeName)
            let fullName: string = unbox tp.UserData.["full_name"]
            ProvidedType(CodeTypeReference(fullName))

    let makeArrayType(typ, rank) =
        match typ with
        | PrimitiveType(typ) ->
            [1..rank] |> List.fold (fun (aggTyp: Type) _ -> aggTyp.MakeArrayType()) typ |> PrimitiveType
        | ProvidedType(typ) ->
            [1..rank] |> List.fold (fun (aggTyp: CodeTypeReference) _ -> CodeTypeReference(aggTyp, 1)) typ |> ProvidedType

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
        let rec findAttributeDefinition (spec: AttributeSpec) =
            match spec.RefOrType with
            | Reference(ref) ->
                match attributeLookup.TryFind(ref.ToString()) with
                | Some(spec) -> findAttributeDefinition(spec)
                | None -> failwithf "Missing referenced attribute %A." ref
            | _ ->
                match spec.Name with
                | Some(name) -> name, spec.RefOrType
                | None -> failwithf "Attribute has no name."

        let rec findElementDefinition (spec: ElementSpec) =
            match spec.Type with
            | Reference(ref) ->
                match elementLookup.TryFind(ref.ToString()) with
                | Some(spec) -> findElementDefinition(spec)
                | None -> failwithf "Missing referenced attribute %A." ref
            | _ ->
                match spec.Name with
                | Some(name) -> name, spec.Type
                | None -> failwithf "Attribute has no name."

        let addProperty(name, ty: RuntimeType, isOptional) =
            let specifiedField =
                if isOptional then
                    let f = CodeMemberField(typeof<bool>, name + "__specified")
                    f.CustomAttributes.Add(Attributes.DebuggerBrowsable) |> ignore
                    providedTy.Members.Add(f) |> ignore
                    let p = CodeMemberProperty(Name=name + "Specified", Type=CodeTypeReference(typeof<bool>))
                    p.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
                    p.GetStatements.Add(Stat.Return(Expr.Field(Expr.This, f.Name))) |> ignore
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
                        p.GetStatements.Add(Stat.Return(Expr.Field(Expr.This, f.Name))) |> ignore
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
                    let itemType = getRuntimeType(typeName)
                    let typ = makeArrayType(itemType, rank)
                    let itemName = getArrayItemElement(contentSpec.Content) |> Option.bind (fun x -> x.Name) |> Option.orDefault "item"
                    typ, itemName, itemType
                | _ -> failwith "Array underlying type specification is missing."
            | _ ->
                match getArrayItemElement(contentSpec.Content) with
                | Some(elementSpec) ->
                    let elemName, elemType = findElementDefinition(elementSpec)
                    let subTyName = providedTy.Name + "ArrayItem"
                    let elementTy, attrs = getParticleType(elemType, elementSpec.MaxOccurs, elementSpec.IsNillable, subTyName)
                    let itemType = ProvidedType(CodeTypeReference(subTyName + "Type"))
                    let typ = makeArrayType(itemType, 1)
                    typ, elemName, itemType
                | None -> failwith "Unsupported SOAP encoding array definition."

        and getParticleType (particleType, maxOccurs, isNillable, name) =
            match particleType with
            | Name(xname) ->
                let typ = getRuntimeType(xname)
                match typ with
                | x when maxOccurs > 1u ->
                    (makeArrayType(x, 1), [Attributes.XmlElement(true)])
                | PrimitiveType(x) when x.IsValueType ->
                    if isNillable then (PrimitiveType(typedefof<Nullable<_>>.MakeGenericType(x)), [Attributes.XmlElement(true)])
                    else (PrimitiveType(x), [Attributes.XmlElement(false)])
                | x -> (x, [Attributes.XmlElement(true)])
            | Definition(SoapEncArray(contentSpec)) ->
                let typ, itemName, _ = getArrayType(contentSpec)
                typ, [ Attributes.XmlArray(true); Attributes.XmlArrayItem(itemName) ]
            | Definition(typeInfo) ->
                let subTy = makePublicClass(name + "Type")
                buildType(subTy, typeInfo)
                providedTy.Members.Add(subTy) |> ignore
                let subTyRef = ProvidedType(CodeTypeReference(subTy.Name))
                if maxOccurs > 1u then (makeArrayType(subTyRef, 1), [Attributes.XmlElement(true)])
                else (subTyRef, [Attributes.XmlElement(true)])
            | _ -> failwithf "not implemented: %A" name

        and parseElementSpec(spec: ElementSpec) =
            let elemName, elemType = findElementDefinition(spec)
            let elementTy, attrs = getParticleType(elemType, spec.MaxOccurs, spec.IsNillable, elemName)
            let property = addProperty(elemName, elementTy, spec.MinOccurs = 0u)
            attrs |> List.iter (property.CustomAttributes.Add >> ignore)

        let parseComplexTypeContentSpec(spec: ComplexTypeContentSpec) =
            spec.Attributes |> List.iter (fun spec ->
                let attrName, attrTypeDef = findAttributeDefinition(spec)
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
            let typ, itemName, itemType = getArrayType(contentSpec)
            let property = addProperty("Array", typ, false)
            property.CustomAttributes.Add(Attributes.XmlElement2(itemName, itemType.AsCodeTypeReference())) |> ignore
        | SimpleType(SimpleTypeSpec.Restriction(spec)) ->
            match getRuntimeType(spec.Base) with
            | PrimitiveType(typ) as rtyp ->
                let property = addProperty("BaseValue", rtyp, false)
                property.CustomAttributes.Add(Attributes.XmlText) |> ignore
                // TODO: Apply constraints?
            | ProvidedType(_) -> failwith "not implemented"
        | SimpleType(Union(spec)) ->
            failwith "not implemented"
        | ComplexType(spec) ->
            if spec.IsAbstract then
                providedTy.TypeAttributes <- providedTy.TypeAttributes ||| TypeAttributes.Abstract
                providedTy.Members.Add(CodeConstructor(Attributes=MemberAttributes.Family)) |> ignore
            match spec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                match getRuntimeType(spec.Base) with
                | PrimitiveType(typ) as rtyp ->
                    let property = addProperty("BaseValue", rtyp, false)
                    property.CustomAttributes.Add(Attributes.XmlText) |> ignore
                    parseComplexTypeContentSpec(spec.Content)
                | ProvidedType(_) ->
                    failwith "not implemented"
            | SimpleContent(SimpleContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                let baseTy = getOrCreateType(spec.Base)
                providedTy.BaseTypes.Add(baseTy.Name) |> ignore
                baseTy.CustomAttributes.Add(Attributes.XmlInclude(providedTy)) |> ignore
                parseComplexTypeContentSpec(spec.Content)
            | ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexTypeContent.Particle(spec) ->
                parseComplexTypeContentSpec(spec)
        | EmptyType -> failwith "not implemented"

    let buildOperationService (operation: Operation) =
        let serviceMethod = CodeMemberMethod(Name=operation.Name.LocalName)
        serviceMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final

        let returnType, returnExpr =
            operation.Response.Body.Parts
            |> List.map (fun part ->
                match part.Reference with
                | XmlReference.SchemaElement(elementName) ->
                    match elementLookup.TryFind <| elementName.ToString() with
                    | Some(schemaType) -> getRuntimeType(XName.Get(elementName.LocalName + "Type", elementName.NamespaceName))
                    | _ -> failwithf "not implemented (%A)" elementName
                | XmlReference.SchemaType(typeName) -> getRuntimeType(typeName))
            |> makeReturnType

        serviceMethod.ReturnType <- returnType

        let requiredHeadersExpr =
            Expr.NewArray(typeof<string>,
                          getRequiredHeaders(operation)
                            |> List.map(fun x -> Expr.Value(x) :> CodeExpression)
                            |> Array.ofList)

        let serviceName = match operation.Version with
                          | Some v -> sprintf "%s.%s" operation.Name.LocalName v
                          | _ -> operation.Name.LocalName

        // CodeDom doesn't support delegates, so we have to improvise
        serviceMethod |> Method.addStat (Stat.Var(typeof<string[]>, "requiredHeaders", requiredHeadersExpr))
                      |> Method.addStat (Stat.Var(typeof<Action<XmlWriter>>, "writeHeader", Expr.Snip("delegate(System.Xml.XmlWriter writer) { //")))
                      |> Method.addExpr (Expr.Call(Expr.Base, "WriteRpcHeader", Expr.Var("writer"), Expr.Value(serviceName), Expr.Var("requiredHeaders")))
                      |> Method.addStat (Stat.Snip("};"))
                      |> Method.addStat (Stat.Var(typeof<Action<XmlWriter>>, "writeBody", Expr.Snip("delegate(System.Xml.XmlWriter writer) { //")))
                      |> Method.addExpr (Expr.Call(Expr.Var("writer"), "WriteStartElement", Expr.Value("producer"), Expr.Value(operation.Request.Name.LocalName), Expr.Value(operation.Request.Body.Namespace)))
                      |> ignore

        operation.Request.Body.Parts
        |> List.iter (fun part ->
            let prtyp = match part.Reference with
                        | XmlReference.SchemaElement(elementName) ->
                            match elementLookup.TryFind <| elementName.ToString() with
                            | Some(schemaType) -> getRuntimeType(XName.Get(elementName.LocalName + "Type", elementName.NamespaceName))
                            | _ -> failwithf "not implemented (%A)" elementName
                        | XmlReference.SchemaType(typeName) -> getRuntimeType(typeName)
            let parameter = CodeParameterDeclarationExpression(prtyp.AsCodeTypeReference(), part.Name)
            serviceMethod |> Method.addParam parameter
                          |> ignore
            serviceMethod |> Method.addStat (Stat.Var(typeof<XmlSerializer>, part.Name + "Serializer", Expr.NewObject(typeof<XmlSerializer>, Expr.TypeOf(prtyp.AsCodeTypeReference()), Expr.NewObject(typeof<XmlRootAttribute>, Expr.Value(part.Name)))))
                          |> Method.addExpr (Expr.Call(Expr.Var(part.Name + "Serializer"), "Serialize", Expr.Var("writer"), Expr.Var(part.Name)))
                          |> ignore)

        let deserializePartsExpr =
            operation.Response.Body.Parts
            |> List.mapi (fun i part ->
                let prtyp = match part.Reference with
                            | XmlReference.SchemaElement(elementName) ->
                                match elementLookup.TryFind <| elementName.ToString() with
                                | Some(schemaType) -> getRuntimeType(XName.Get(elementName.LocalName + "Type", elementName.NamespaceName))
                                | _ -> failwithf "not implemented (%A)" elementName
                            | XmlReference.SchemaType(typeName) -> getRuntimeType(typeName)

                let deserializeExpr =
                    [| !~~ Stat.Var(typeof<XmlSerializer>, part.Name + "Serializer", Expr.NewObject(typeof<XmlSerializer>, Expr.TypeOf(prtyp.AsCodeTypeReference()), Expr.NewObject(typeof<XmlRootAttribute>, Expr.Value(part.Name))))
                       !~~ Stat.Assign(Expr.Var(sprintf "v%d" i), Expr.Cast(prtyp.AsCodeTypeReference(), Expr.Call(Expr.Var(part.Name + "Serializer"), "Deserialize", Expr.Var("reader")))) |]

                let deserializeExpr =
                    if part.Name = "keha" && undescribedFaults then
                     [| !~> Expr.Call(Expr.Var("reader"), "SetBookmark", Expr.Value("keha"))
                        !~~ Stat.IfThenElse(Expr.Call(Expr.This, "MoveToElement", Expr.Var("reader"), Expr.Value("faultCode"), Expr.Value(""), Expr.Value(4)),
                                            [| !~> Expr.Call(Expr.Var("reader"), "ReturnToAndRemoveBookmark", Expr.Value("keha"))
                                               !~~ Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Call(Expr.Var("reader"), "ReadInnerXml"))) |],
                                            Array.concat [
                                                [| !~> Expr.Call(Expr.Var("reader"), "ReturnToAndRemoveBookmark", Expr.Value("keha")) |]
                                                deserializeExpr
                                            ]) |]
                    else deserializeExpr

                !~~ Stat.IfThenElse(Expr.CallOp(Expr.Prop(Expr.Var("reader"), "LocalName"), Expr.Op.IdentityEquality, Expr.Value(part.Name)),
                                    deserializeExpr))
            |> Array.ofList

        serviceMethod |> Method.addExpr (Expr.Call(Expr.Var("writer"), "WriteEndElement"))
                      |> Method.addStat (Stat.Snip("};"))
                      |> Method.addStat (Stat.Var(CodeTypeReference("System.Func", CodeTypeReference(typeof<XmlReader>), returnType), "readBody", Expr.Snip("delegate(System.Xml.XmlReader r) { //")))
                      |> Method.addStat (if undescribedFaults then Stat.Var("XmlBookmarkReader", "reader", Expr.Cast("XmlBookmarkReader", Expr.Var("r"))) else Stat.Var(typeof<XmlReader>, "reader", Expr.Var("r")))
                      |> Method.addStat (Stat.IfThenElse(Expr.CallOp(
                                                            Expr.CallOp(Expr.Prop(Expr.Var("reader"), "LocalName"), Expr.Op.IdentityInequality, Expr.Value(operation.Response.Name.LocalName)),
                                                            Expr.Op.BooleanOr,
                                                            Expr.CallOp(Expr.Prop(Expr.Var("reader"), "NamespaceURI"), Expr.Op.IdentityInequality, Expr.Value(operation.Response.Body.Namespace))),
                                                         Stat.Throw(Expr.NewObject(typeof<Exception>, Expr.Value("Invalid response message.")))))
                      |> ignore

        operation.Response.Body.Parts
        |> List.iteri (fun i part ->
            let prtyp = match part.Reference with
                        | XmlReference.SchemaElement(elementName) ->
                            match elementLookup.TryFind <| elementName.ToString() with
                            | Some(schemaType) -> getRuntimeType(XName.Get(elementName.LocalName + "Type", elementName.NamespaceName))
                            | _ -> failwithf "not implemented (%A)" elementName
                        | XmlReference.SchemaType(typeName) -> getRuntimeType(typeName)
            serviceMethod |> Method.addStat (Stat.Var(prtyp.AsCodeTypeReference(), sprintf "v%d" i, Expr.Value(null)))
                          |> ignore)

        serviceMethod |> Method.addStat (Stat.While(Expr.Call(Expr.This, "MoveToElement", Expr.Var("reader"), Expr.Value(null), Expr.Value(null), Expr.Value(3)), deserializePartsExpr))
                      |> Method.addStat (Stat.Return(returnExpr))
                      |> Method.addStat (Stat.Snip("};"))
                      |> ignore

        match operation.Documentation.TryGetValue("et") with
        | true, doc -> serviceMethod.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
        | _ -> ()

        let methodCall = Expr.Call(CodeMethodReferenceExpression(Expr.Base, "MakeServiceCall", returnType), Expr.Var("writeHeader"), Expr.Var("writeBody"), Expr.Var("readBody"))

        if not <| operation.Response.Body.Parts.IsEmpty
        then serviceMethod |> Method.addStat (Stat.Return(methodCall)) |> ignore
        else serviceMethod |> Method.addExpr methodCall |> ignore

        serviceMethod

    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Types)
    |> Seq.map (fun x -> getOrCreateType(x.Key), x.Value)
    |> Seq.iter buildType

    let targetClass = makeStaticClass(typeNamePath.[typeNamePath.Length - 1], TypeAttributes.Public)

    if undescribedFaults then
        targetClass.Members.Add(createXmlBookmarkReaderType()) |> ignore

    targetClass.Members.Add(portBaseTy) |> ignore
    targetClass.Members.Add(serviceTypesTy) |> ignore

    schema.Services |> List.iter (fun service ->
        let serviceTy = makeStaticClass(service.Name, TypeAttributes.Public)
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

    let codeNamespace = CodeNamespace(String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1)))
    codeNamespace.Types.Add(targetClass) |> ignore

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.ReferencedAssemblies.Add("System.dll") |> ignore
    codeCompileUnit.ReferencedAssemblies.Add("System.Net.dll") |> ignore
    codeCompileUnit.ReferencedAssemblies.Add("System.Numerics.dll") |> ignore
    codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore

    codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore

    compileAssembly(codeCompileUnit).GetType(sprintf "%s.%s" codeNamespace.Name targetClass.Name)
