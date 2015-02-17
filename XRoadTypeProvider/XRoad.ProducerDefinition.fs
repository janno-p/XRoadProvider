module private XRoad.ProducerDefinition

open Microsoft.CSharp
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.Serialization
open XRoad.Parser
open XRoad.Parser.XsdSchema

let (!~~) x = x :> CodeStatement

module private Stat =
    type Assign = CodeAssignStatement
    type IfThenElse = CodeConditionStatement
    type Return = CodeMethodReturnStatement
    type Snip = CodeSnippetStatement
    type Var = CodeVariableDeclarationStatement

    let ofExpr(expr) = CodeExpressionStatement(expr)

    let While (testExpression, [<ParamArray>] statements) =
        CodeIterationStatement(null, testExpression, null, statements)

module private Expr =
    type Base = CodeBaseReferenceExpression
    type Call = CodeMethodInvokeExpression
    type CallOp = CodeBinaryOperatorExpression
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
    type This = CodeThisReferenceExpression

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

let private compileAssembly code =
    let fileName = Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> sprintf "%A.dll")
    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(OutputAssembly=fileName, GenerateExecutable=false)
    //parameters.CompilerOptions <- "/doc:" + Path.ChangeExtension(fileName, "xml")
    //( use wr = new StreamWriter(File.Open(Path.ChangeExtension(fileName, "cs"), FileMode.Create, FileAccess.Write))
    //  codeProvider.GenerateCodeFromCompileUnit(code, wr, CodeGeneratorOptions()))
    let compilerResults = codeProvider.CompileAssemblyFromDom(parameters, [| code |])
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

let private makeXmlIncludeAttribute (providedTy: CodeTypeDeclaration) =
    let attribute = CodeAttributeDeclaration(CodeTypeReference(typeof<XmlIncludeAttribute>))
    attribute.Arguments.Add(CodeAttributeArgument(CodeTypeOfExpression(providedTy.Name))) |> ignore
    attribute

let private makeXmlTypeAttribute(typeName: XName) =
    let attribute = CodeAttributeDeclaration(CodeTypeReference(typeof<XmlTypeAttribute>))
    attribute.Arguments.Add(CodeAttributeArgument(CodePrimitiveExpression(typeName.LocalName))) |> ignore
    attribute.Arguments.Add(CodeAttributeArgument("Namespace", CodePrimitiveExpression(typeName.NamespaceName))) |> ignore
    attribute

let private makeXmlElementAttribute() =
    let attribute = CodeAttributeDeclaration(CodeTypeReference(typeof<XmlElementAttribute>))
    let formExpr = CodePropertyReferenceExpression(CodeTypeReferenceExpression(typeof<XmlSchemaForm>), "Unqualified")
    attribute.Arguments.Add(CodeAttributeArgument("Form", formExpr)) |> ignore
    attribute

let private makeXmlAttributeAttribute() =
    let attribute = CodeAttributeDeclaration(CodeTypeReference(typeof<XmlAttributeAttribute>))
    let formExpr = CodePropertyReferenceExpression(CodeTypeReferenceExpression(typeof<XmlSchemaForm>), "Unqualified")
    attribute.Arguments.Add(CodeAttributeArgument("Form", formExpr)) |> ignore
    attribute

let private makeXmlTextAttribute() =
    CodeAttributeDeclaration(CodeTypeReference(typeof<XmlTextAttribute>))

let private makeXRoadHeaderType() =
    let headerTy = makePublicClass("XRoadHeader")
    let addProperty(name, ty: Type, doc) =
        let backingField = CodeMemberField(ty, name + "__backing")
        headerTy.Members.Add(backingField) |> ignore
        let backingFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), backingField.Name)
        let property = CodeMemberProperty(Name=name, Type=CodeTypeReference(ty))
        property.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
        property.GetStatements.Add(CodeMethodReturnStatement(backingFieldRef)) |> ignore
        property.SetStatements.Add(CodeAssignStatement(backingFieldRef, CodePropertySetValueReferenceExpression())) |> ignore
        property.Comments.Add(CodeCommentStatement("<summary>", true)) |> ignore
        property.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
        property.Comments.Add(CodeCommentStatement("</summary>", true)) |> ignore
        headerTy.Members.Add(property) |> ignore
    addProperty("Consumer", typeof<string>, "DNS-name of the institution")
    addProperty("Producer", typeof<string>, "DNS-name of the database")
    addProperty("UserId", typeof<string>, "ID code of the person invoking the service, preceded by a two-letter country code. For example: EE37702026518")
    addProperty("Id", typeof<string>, "Service invocation nonce (unique identifier)")
    addProperty("Service", typeof<string>, "Name of the service to be invoked")
    addProperty("Issue", typeof<string>, "Name of file or document related to the service invocation")
    addProperty("Unit", typeof<string>, "Registration code of the institution or its unit on whose behalf the service is used (applied in the legal entity portal)")
    addProperty("Position", typeof<string>, "Organizational position or role of the person invoking the service")
    addProperty("UserName", typeof<string>, "Name of the person invoking the service")
    addProperty("Async", typeof<Nullable<bool>>, "Specifies asynchronous service. If the value is \"true\", then the security server performs the service call asynchronously.")
    addProperty("Authenticator", typeof<string>, "Authentication method, one of the following: ID-CARD - with a certificate of identity; CERT - with another certificate; EXTERNAL - through a third-party service; PASSWORD - with user ID and a password. Details of the authentication (e.g. the identification of a bank for external authentication) can be given in brackets after the authentication method.")
    addProperty("Paid", typeof<string>, "The amount of money paid for invoking the service")
    addProperty("Encrypt", typeof<string>, "If an organization has got the right from the X-Road Center to hide queries, with the database agreeing to hide the query, the occurrence of this tag in the query header makes the database security server to encrypt the query log, using the encryption key of the X-Road Center")
    addProperty("EncryptCert", typeof<string>, "Authentication certificate of the query invokers ID Card, in the base64-encoded DER format. Occurrence of this tag in the query header represents the wish to encrypt the query log in the organizations security server, using authentication key of the query invokers ID Card. This field is used in the Citizen Query Portal only.")
    addProperty("Encrypted", typeof<string>, "If the query header contains the encrypt tag and the query log as been successfully encrypted, an empty encrypted tag will be inserted in the reply header.")
    addProperty("EncryptedCert", typeof<string>, "If the query header contains the encryptedCert tag and the query log has been successfully encrypted, an empty encryptedCert tag will accordingly be inserted in the reply header.")
    headerTy

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

    meth |> Method.addParam (Expr.Param(typeof<Xml.XmlWriter>, "writer"))
         |> Method.addParam (Expr.Param("XRoadHeader", "header"))
         |> Method.addParam (Expr.Param(typeof<string>, "serviceName"))
         |> Method.addParam (Expr.Param(typeof<IList<string>>, "requiredHeaders"))
         |> ignore

    let writerRef = Expr.Var("writer")
    let headerProp propName = Expr.Prop(Expr.Var("header"), propName)
    let writeRpcHeaderStatements = writeHeaderStatements XmlNamespace.Xtee

    meth |> Method.addExpr (Expr.Call(writerRef, "WriteAttributeString", Expr.Value("xmlns"), Expr.Value("xtee"), Expr.Value(null), Expr.Value(XmlNamespace.Xtee)))
         |> Method.addStat (Stat.Var(typeof<string>, "producerValue"))
         |> Method.addStat (Stat.IfThenElse(Expr.CallOp(headerProp("Producer"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                      [| !~~ Stat.Assign(Expr.Var("producerValue"), Expr.Var("producer")) |],
                                      [| !~~ Stat.Assign(Expr.Var("producerValue"), headerProp("Producer")) |]))
         |> Method.addStat (Stat.Var(typeof<string>, "requestId"))
         |> Method.addStat (Stat.IfThenElse(Expr.CallOp(headerProp("Id"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                      [| !~~ Stat.Assign(Expr.Var("requestId"), Expr.Call(Expr.This(), "GenerateNonce")) |],
                                      [| !~~ Stat.Assign(Expr.Var("requestId"), headerProp("Id")) |]))
         |> Method.addStat (Stat.Var(typeof<string>, "fullServiceName"))
         |> Method.addStat (Stat.IfThenElse(Expr.CallOp(headerProp("Service"), Expr.Op.IdentityEquality, Expr.Value(null)),
                                      [| !~~ Stat.Assign(Expr.Var("fullServiceName"), Expr.Call(Expr.Type(typeof<string>), "Format", Expr.Value("{0}.{1}"), Expr.Var("producerValue"), Expr.Var("serviceName"))) |],
                                      [| !~~ Stat.Assign(Expr.Var("fullServiceName"), headerProp("Service")) |]))
         |> Method.addStat (writeRpcHeaderStatements("asutus", headerProp("Consumer") :> CodeExpression, "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("andmekogu", Expr.Var("producerValue"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("isikukood", headerProp("UserId"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("id", Expr.Var("requestId"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("nimi", Expr.Var("fullServiceName"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("toimik", headerProp("Issue"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("allasutus", headerProp("Unit"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("amet", headerProp("Position"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("ametniknimi", headerProp("UserName"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("asynkroonne", headerProp("Async"), "WriteValue"))
         |> Method.addStat (writeRpcHeaderStatements("autentija", headerProp("Authenticator"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("makstud", headerProp("Paid"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("salastada", headerProp("Encrypt"), "WriteString"))
         |> Method.addStat (writeRpcHeaderStatements("salastada_sertifikaadiga", headerProp("EncryptCert"), "WriteString"))

let private makeServicePortBaseType() =
    let portBaseTy = makePublicClass("AbstractServicePort")
    portBaseTy.TypeAttributes <- portBaseTy.TypeAttributes ||| TypeAttributes.Abstract

    let addressField = CodeMemberField(typeof<string>, "address")
    let addressFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), addressField.Name)
    let addressProperty = CodeMemberProperty(Name="Address", Type=CodeTypeReference(typeof<string>))
    addressProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    addressProperty.GetStatements.Add(CodeMethodReturnStatement(addressFieldRef)) |> ignore
    addressProperty.SetStatements.Add(CodeAssignStatement(addressFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

    let producerField = CodeMemberField(typeof<string>, "producer")
    let producerFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), producerField.Name)
    let producerProperty = CodeMemberProperty(Name="Producer", Type=CodeTypeReference(typeof<string>))
    producerProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    producerProperty.GetStatements.Add(CodeMethodReturnStatement(producerFieldRef)) |> ignore
    producerProperty.SetStatements.Add(CodeAssignStatement(producerFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

    let nonceMeth = makeGenerateNonceMethod()
    let writeRpcHeaderMeth = makeWriteXRoadRpcHeaderMethod()

    let ctor = CodeConstructor()
    ctor.Attributes <- MemberAttributes.Family
    ctor.Parameters.Add(CodeParameterDeclarationExpression(typeof<string>, "address")) |> ignore
    ctor.Parameters.Add(CodeParameterDeclarationExpression(typeof<string>, "producer")) |> ignore
    ctor.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "address"), CodeVariableReferenceExpression("address"))) |> ignore
    ctor.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "producer"), CodeVariableReferenceExpression("producer"))) |> ignore

    let serviceCallMeth = CodeMemberMethod(Name="MakeServiceCall")
    serviceCallMeth.Attributes <- MemberAttributes.Family ||| MemberAttributes.Final

    serviceCallMeth |> Method.addParam (Expr.Param(typeof<string>, "operationNamespace"))
                    |> Method.addParam (Expr.Param(typeof<Action<Xml.XmlWriter>>, "writeHeaderAction"))
                    |> Method.addParam (Expr.Param(typeof<Action<Xml.XmlWriter>>, "writeBody"))
                    |> ignore

    let addStatement(x: CodeStatement) = serviceCallMeth.Statements.Add(x) |> ignore
    let addExpression(x: CodeExpression) = serviceCallMeth.Statements.Add(x) |> ignore

    addStatement <| CodeVariableDeclarationStatement(typeof<Net.WebRequest>, "request", CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<Net.WebRequest>), "Create", CodeVariableReferenceExpression("address")))
    let requestRef = CodeVariableReferenceExpression("request")

    addStatement <| CodeAssignStatement(CodePropertyReferenceExpression(requestRef, "Method"), CodePrimitiveExpression("POST"))
    addStatement <| CodeAssignStatement(CodePropertyReferenceExpression(requestRef, "ContentType"), CodePrimitiveExpression("text/xml; charset=utf-8"))
    addExpression <| CodeMethodInvokeExpression(CodePropertyReferenceExpression(requestRef, "Headers"), "Set", CodePrimitiveExpression("SOAPAction"), CodePrimitiveExpression(""))

    addStatement <| CodeVariableDeclarationStatement(typeof<IO.Stream>, "stream", CodePrimitiveExpression(null))
    let streamRef = CodeVariableReferenceExpression("stream")

    let writerRef = Expr.Var("writer")

    let writerStatements: CodeStatement [] = [|
        CodeAssignStatement(streamRef, CodeMethodInvokeExpression(requestRef, "GetRequestStream"))
        CodeVariableDeclarationStatement(typeof<Xml.XmlWriter>, "writer", CodePrimitiveExpression(null))
        CodeTryCatchFinallyStatement(
            [| CodeAssignStatement(writerRef, CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<Xml.XmlWriter>), "Create", streamRef))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteStartDocument"))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteStartElement", CodePrimitiveExpression("soapenv"), CodePrimitiveExpression("Envelope"), CodePrimitiveExpression(XmlNamespace.SoapEnvelope)))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteAttributeString", CodePrimitiveExpression("xmlns"), CodePrimitiveExpression("pns"), CodePrimitiveExpression(null), CodeVariableReferenceExpression("operationNamespace")))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteStartElement", CodePrimitiveExpression("Header"), CodePrimitiveExpression(XmlNamespace.SoapEnvelope)))
               CodeExpressionStatement(CodeDelegateInvokeExpression(CodeVariableReferenceExpression("writeHeaderAction"), writerRef))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteEndElement"))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteStartElement", CodePrimitiveExpression("Body"), CodePrimitiveExpression(XmlNamespace.SoapEnvelope)))
               CodeExpressionStatement(CodeDelegateInvokeExpression(CodeVariableReferenceExpression("writeBody"), writerRef))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteEndElement"))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteEndElement"))
               CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "WriteEndDocument")) |],
            [| |],
            [| CodeConditionStatement(CodeBinaryOperatorExpression(writerRef, CodeBinaryOperatorType.IdentityInequality, CodePrimitiveExpression(null)), CodeExpressionStatement(CodeMethodInvokeExpression(writerRef, "Dispose"))) |])
    |]

    addStatement <| CodeTryCatchFinallyStatement(
                        writerStatements,
                        [| |],
                        [| CodeConditionStatement(CodeBinaryOperatorExpression(streamRef, CodeBinaryOperatorType.IdentityInequality, CodePrimitiveExpression(null)), CodeExpressionStatement(CodeMethodInvokeExpression(streamRef, "Dispose"))) |])

    addStatement <| CodeVariableDeclarationStatement(typeof<Net.WebResponse>, "response", CodePrimitiveExpression(null))
    let responseRef = CodeVariableReferenceExpression("response")

    let readerRef = CodeVariableReferenceExpression("reader")
    let readerStatements: CodeStatement [] = [|
        CodeAssignStatement(responseRef, CodeMethodInvokeExpression(requestRef, "GetResponse"))
        CodeVariableDeclarationStatement(typeof<IO.StreamReader>, "reader", CodePrimitiveExpression(null))
        CodeTryCatchFinallyStatement(
            [| CodeAssignStatement(readerRef, CodeObjectCreateExpression(typeof<IO.StreamReader>, CodeMethodInvokeExpression(responseRef, "GetResponseStream")))
               CodeExpressionStatement(CodeMethodInvokeExpression(readerRef, "ReadToEnd")) |],
            [| |],
            [| CodeConditionStatement(CodeBinaryOperatorExpression(readerRef, CodeBinaryOperatorType.IdentityInequality, CodePrimitiveExpression(null)), CodeExpressionStatement(CodeMethodInvokeExpression(readerRef, "Dispose"))) |])
    |]

    addStatement <| CodeTryCatchFinallyStatement(
                        readerStatements,
                        [| |],
                        [| CodeConditionStatement(CodeBinaryOperatorExpression(responseRef, CodeBinaryOperatorType.IdentityInequality, CodePrimitiveExpression(null)), CodeExpressionStatement(CodeMethodInvokeExpression(responseRef, "Dispose"))) |])

    portBaseTy.Members.Add(ctor) |> ignore
    portBaseTy.Members.Add(addressField) |> ignore
    portBaseTy.Members.Add(addressProperty) |> ignore
    portBaseTy.Members.Add(producerField) |> ignore
    portBaseTy.Members.Add(producerProperty) |> ignore
    portBaseTy.Members.Add(serviceCallMeth) |> ignore
    portBaseTy.Members.Add(writeRpcHeaderMeth) |> ignore
    portBaseTy.Members.Add(nonceMeth) |> ignore

    portBaseTy

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
    let rec getReturnTypeTuple (tuple: RuntimeType list, types) =
        match types with
        | [] -> CodeTypeReference("System.Tuple", tuple |> List.map (fun x -> x.AsCodeTypeReference()) |> Array.ofList)
        | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
        | x::xs -> let inner = getReturnTypeTuple([x], xs)
                   CodeTypeReference("System.Tuple", ((tuple |> List.map (fun x -> x.AsCodeTypeReference())) @ [inner]) |> Array.ofList)
    match types with
    | [] -> CodeTypeReference(typeof<Void>)
    | tp::[] -> tp.AsCodeTypeReference()
    | many -> getReturnTypeTuple([], types)

let makeProducerType (typeNamePath: string [], producerUri) =
    let schema = resolveUri producerUri |> readSchema
    let typeCache = Dictionary<XName,CodeTypeDeclaration>()
    let namespaceCache = Dictionary<XNamespace,CodeTypeDeclaration>()

    let headerTy = makeXRoadHeaderType()
    let portBaseTy = makeServicePortBaseType()
    let serviceTypesTy = makeStaticClass("DefinedTypes", TypeAttributes.Public)

    let getOrCreateNamespace (name: XNamespace) =
        match namespaceCache.TryGetValue(name) with
        | false, _ ->
            let producerName =
                match Regex.Match(name.NamespaceName, @"^http://(((?<producer>\w+)\.x-road\.ee/producer)|(producers\.\w+\.xtee\.riik\.ee/producer/(?<producer>\w+)))$") with
                | m when m.Success -> m.Groups.["producer"].Value
                | _ -> failwithf "TODO: Implement normal namespace handling for tns: %A" name
            let typ = CodeTypeDeclaration(producerName, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTypesTy.Members.Add(typ) |> ignore
            namespaceCache.Add(name, typ)
            typ
        | true, typ -> typ

    let getOrCreateType (name: XName) =
        match typeCache.TryGetValue(name) with
        | false, _ ->
            let typ = makePublicClass(name.LocalName)
            typ.CustomAttributes.Add(makeXmlTypeAttribute(name)) |> ignore
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

    let (|SoapEncArray|_|) (typ: SchemaType) =
        let getArrayType arrayType =
            match arrayType with
            | Some(typeName, rank) -> makeArrayType(getRuntimeType(typeName), rank)
            | _ -> failwith "Array underlying type specification is missing."

        match typ with
        | SchemaType.ComplexType(spec) ->
            match spec.Content with
            | ComplexTypeContent.ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                match spec.Base.LocalName, spec.Base.NamespaceName with
                | "Array", XmlNamespace.SoapEncoding ->
                    match spec.Content.Attributes with
                    | [ arrayType ] when arrayType.Name = Some("arrayType") ->
                        Some(getArrayType(arrayType.ArrayType))
                    | [ arrayType ] when arrayType.RefOrType = RefOrType.Ref(XName.Get("arrayType", XmlNamespace.SoapEncoding)) ->
                        Some(getArrayType(arrayType.ArrayType))
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None

    let rec buildType(providedTy: CodeTypeDeclaration, typeInfo) =
        let addProperty(name, ty: RuntimeType, isOptional) =
            let specifiedField =
                if isOptional then
                    let f = CodeMemberField(typeof<bool>, name + "__specified")
                    providedTy.Members.Add(f) |> ignore
                    Some(f)
                else None
            let backingField = CodeMemberField(ty.AsCodeTypeReference(), name + "__backing")
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

        let getParticleType (particleType, maxOccurs, isNillable, name) =
            match particleType with
            | RefOrType.Name(xname) ->
                let typ = getRuntimeType(xname)
                match typ with
                | x when maxOccurs > 1u -> makeArrayType(x, 1)
                | PrimitiveType(x) when isNillable && x.IsValueType -> PrimitiveType(typedefof<Nullable<_>>.MakeGenericType(x))
                | x -> x
            | RefOrType.Type(SoapEncArray(typ)) ->
                typ
            | RefOrType.Type(typeInfo) ->
                let subTy = makePublicClass(name + "Type")
                buildType(subTy, typeInfo)
                providedTy.Members.Add(subTy) |> ignore
                let subTyRef = ProvidedType(CodeTypeReference(subTy.Name))
                if maxOccurs > 1u then makeArrayType(subTyRef, 1)
                else subTyRef
            | _ -> failwithf "not implemented: %A" name

        let parseElementSpec(spec: ElementSpec) =
            let elementTy = getParticleType(spec.Type, spec.MaxOccurs, spec.IsNillable, spec.Name)
            let property = addProperty(spec.Name, elementTy, spec.MinOccurs = 0u)
            property.CustomAttributes.Add(makeXmlElementAttribute()) |> ignore

        let parseComplexTypeContentSpec(spec: ComplexTypeContentSpec) =
            spec.Attributes |> List.iter (fun spec ->
                match spec.Name with
                | Some(name) ->
                    let attributeTy = getParticleType(spec.RefOrType, 1u, false, name)
                    let property = addProperty(name, attributeTy, match spec.Use with Required -> true | _ -> false)
                    property.CustomAttributes.Add(makeXmlAttributeAttribute()) |> ignore
                | _ -> failwith "not implemented")
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
                    | SequenceContent.Element(spec) ->
                        parseElementSpec(spec)
                    | SequenceContent.Sequence(spec) ->
                        failwith "not implemented")
            | None -> ()

        match typeInfo with
        | SoapEncArray _ ->
            // TODO: Some global types like message responses are actually array-s
            ()
        | SimpleType(SimpleTypeSpec.Restriction(spec)) -> failwith "not implemented"
        | ComplexType(spec) ->
            if spec.IsAbstract then
                providedTy.TypeAttributes <- providedTy.TypeAttributes ||| TypeAttributes.Abstract
                providedTy.Members.Add(CodeConstructor(Attributes=MemberAttributes.Family)) |> ignore
            match spec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                match getRuntimeType(spec.Base) with
                | PrimitiveType(typ) as rtyp ->
                    let property = addProperty("BaseValue", rtyp, false)
                    property.CustomAttributes.Add(makeXmlTextAttribute()) |> ignore
                    parseComplexTypeContentSpec(spec.Content)
                | ProvidedType(_) ->
                    failwith "not implemented"
            | SimpleContent(SimpleContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                let baseTy = getOrCreateType(spec.Base)
                providedTy.BaseTypes.Add(baseTy.Name) |> ignore
                baseTy.CustomAttributes.Add(makeXmlIncludeAttribute(providedTy)) |> ignore
                parseComplexTypeContentSpec(spec.Content)
            | ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexTypeContent.Particle(spec) ->
                parseComplexTypeContentSpec(spec)

    let buildOperationService (operation: Operation) =
        let serviceMethod = CodeMemberMethod(Name=operation.Name.LocalName)
        serviceMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final

        let returnType =
            operation.Response.Body
            |> List.map (fun part ->
                match part.Reference with
                | XmlReference.SchemaElement(_) -> failwith "not implemented"
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
                      |> Method.addStat (Stat.Var(typeof<Action<Xml.XmlWriter>>, "writeHeader", Expr.Snip("delegate(System.Xml.XmlWriter writer) { //")))
                      |> Method.addExpr (Expr.Call(Expr.Base(), "WriteRpcHeader", Expr.Var("writer"), Expr.Var("settings"), Expr.Value(serviceName), Expr.Var("requiredHeaders")))
                      |> Method.addStat (Stat.Snip("};"))
                      |> Method.addStat (Stat.Var(typeof<Action<Xml.XmlWriter>>, "writeBody", Expr.Snip("delegate(System.Xml.XmlWriter writer) { //")))
                      |> Method.addExpr (Expr.Call(Expr.Var("writer"), "WriteStartElement", Expr.Value(operation.Name.LocalName), Expr.Value(operation.Name.NamespaceName)))
                      |> ignore

        operation.Request.Body
        |> List.iter (fun part ->
            let prtyp = match part.Reference with
                        | XmlReference.SchemaElement(elem) -> failwith "Not implemented"
                        | XmlReference.SchemaType(typeName) -> getRuntimeType(typeName)
            let parameter = CodeParameterDeclarationExpression(prtyp.AsCodeTypeReference(), part.Name)
            serviceMethod |> Method.addParam parameter
                          |> ignore
            serviceMethod |> Method.addStat (Stat.Var(typeof<Xml.Serialization.XmlAttributes>, part.Name + "Attribs", Expr.NewObject(typeof<Xml.Serialization.XmlAttributes>)))
                          |> Method.addStat (Stat.Assign(Expr.Prop(Expr.Var(part.Name + "Attribs"), "XmlRoot"), Expr.NewObject(typeof<Xml.Serialization.XmlRootAttribute>, Expr.Value("keha"))))
                          |> Method.addStat (Stat.Var(typeof<Xml.Serialization.XmlAttributeOverrides>, part.Name + "Overrides", Expr.NewObject(typeof<Xml.Serialization.XmlAttributeOverrides>)))
                          |> Method.addExpr (Expr.Call(Expr.Var(part.Name + "Overrides"), "Add", Expr.TypeOf(prtyp.AsCodeTypeReference()), Expr.Var(part.Name + "Attribs")))
                          |> Method.addStat (Stat.Var(typeof<Xml.Serialization.XmlSerializer>, part.Name + "Serializer", Expr.NewObject(typeof<Xml.Serialization.XmlSerializer>, Expr.TypeOf(prtyp.AsCodeTypeReference()), Expr.Var(part.Name + "Overrides"))))
                          |> Method.addExpr (Expr.Call(Expr.Var(part.Name + "Serializer"), "Serialize", Expr.Var("writer"), Expr.Var(part.Name)))
                          |> ignore)

        serviceMethod |> Method.addExpr (Expr.Call(Expr.Var("writer"), "WriteEndElement"))
                      |> Method.addStat (Stat.Snip("};"))
                      |> Method.addExpr (Expr.Call(Expr.Base(), "MakeServiceCall", Expr.Value(operation.Name.NamespaceName), Expr.Var("writeHeader"), Expr.Var("writeBody")))
                      |> ignore

        let headerParam = CodeParameterDeclarationExpression(headerTy.Name, "settings")
        let optionalAttribute = CodeAttributeDeclaration(CodeTypeReference(typeof<OptionalAttribute>))
        headerParam.CustomAttributes.Add(optionalAttribute) |> ignore
        serviceMethod.Parameters.Add(headerParam) |> ignore

        if not <| operation.Response.Body.IsEmpty then
            serviceMethod |> Method.addStat (Stat.Return(Expr.Value(null))) |> ignore

        serviceMethod

    schema.TypeSchemas
    |> Seq.collect (fun typeSchema -> typeSchema.Types)
    |> Seq.map (fun x -> getOrCreateType(x.Key), x.Value)
    |> Seq.iter buildType

    let targetClass = makeStaticClass(typeNamePath.[typeNamePath.Length - 1], TypeAttributes.Public)
    targetClass.Members.Add(headerTy) |> ignore
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
