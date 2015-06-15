module internal XRoad.CodeDom.ServiceImpl

open System
open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Net
open System.Reflection
open System.Security.Cryptography
open System.Text
open System.Xml

open XRoad.CodeDom.Common
open XRoad.Common
open XRoad.ServiceDescription

/// Creates method for serializing X-Road specific header elements in SOAP message.
/// Header usage depends on operation style: rpc/encoded and document/literal styles use different names.
let createWriteXRoadHeaderMethod (protocol: XRoadProtocol) =
    // Initialize namespace, header name mapping, property name mapping and namespace prefix.
    let hdrns, hdrName, propName, nsprefix =
        let select, prefix = match protocol with Version_20 -> fst, "xtee" | _ -> snd, "xrd"
        let select = headerMapping >> select
        protocol.Namespace, select >> fst3, select >> snd3, prefix
    let writerVar = Expr.var "writer"
    // Helper function to generate statements for serializing each header element.
    // Header names provided in `requiredHeaders` argument are always serialized, others are serialized
    // when value is present.
    let writeHeaderElement name propVar methodName =
        let reqHeaderContainsNameExpr = ((Expr.var "requiredHeaders") @-> "Contains") @% [Expr.value name]
        let propNotNullExpr = Op.isNotNull propVar
        Stmt.condIf (Op.boolOr reqHeaderContainsNameExpr
                               propNotNullExpr)
                    [ Stmt.ofExpr ((writerVar @-> "WriteStartElement") @% [Expr.value name; Expr.value hdrns])
                      Stmt.condIf propNotNullExpr [ Stmt.ofExpr ((writerVar @-> methodName) @% [propVar]) ]
                      Stmt.ofExpr ((writerVar @-> "WriteEndElement") @% []) ]
    // Some headers are required by X-Road, these need default values in case user has not specified.
    let declareWithDefaultValue name xtname defExpr (m: CodeMemberMethod) =
        m |> Meth.addStmt (Stmt.declVar<string> name)
          |> Meth.addStmt (Stmt.condIfElse (Op.isNull (Expr.this @=> (propName xtname)))
                                           [Stmt.assign (Expr.var name) defExpr]
                                           [Stmt.assign (Expr.var name) (Expr.this @=> (propName xtname))])
    // Method declaration:
    Meth.create "WriteHeader"
    |> Meth.setAttr (MemberAttributes.Family ||| MemberAttributes.Final)
    // Method parameters:
    |> Meth.addParam<XmlWriter> "writer"
    |> Meth.addParam<string> "serviceName"
    |> Meth.addParam<IList<string>> "requiredHeaders"
    // Add namespace prefix definition:
    |> Meth.addExpr ((writerVar @-> "WriteAttributeString") @% [Expr.value "xmlns"; Expr.value nsprefix; Expr.nil; Expr.value hdrns])
    // Required elements with defualt values:
    |> declareWithDefaultValue "producerValue" "andmekogu" (Expr.var "producerName")
    |> declareWithDefaultValue "requestId" "id" (Expr.var "GenerateNonce" @%% [])
    |> declareWithDefaultValue "fullServiceName" "nimi" ((Expr.typeRefOf<string> @-> "Format") @% [Expr.value "{0}.{1}"; Expr.var "producerValue"; Expr.var "serviceName"])
    // Write header elements to stream:
    |> Meth.addStmt (writeHeaderElement (hdrName "asutus") (Expr.this @=> (propName "asutus")) "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "andmekogu") (Expr.var "producerValue") "WriteString")
    |> Meth.addStmt (writeHeaderElement (hdrName "isikukood") (Expr.this @=> (propName "isikukood")) "WriteString")
    // This header is only available with legacy format messages:
    |> iif (match protocol with Version_20 -> true | _ -> false) (fun m -> m |> Meth.addStmt (writeHeaderElement (hdrName "ametnik") (Expr.this @=> (propName "ametnik")) "WriteString"))
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

/// Types which extend BinaryContent types should inherit its properties from BinaryContent runtime type also.
/// Create overloaded constructor to match BinaryContent constructor.
let inheritBinaryContent typ =
    typ
    |> Cls.setParent (ContentType.AsCodeTypeReference())
    |> Cls.addMember (Ctor.create()
                      |> Ctor.setAttr MemberAttributes.Public
                      |> Ctor.addParam<IDictionary<string,Stream>> "attachments"
                      |> Ctor.addParam<string> "id"
                      |> Ctor.addBaseArg (Expr.var "attachments")
                      |> Ctor.addBaseArg (Expr.var "id"))
    |> Cls.addMember (Ctor.create()
                      |> Ctor.setAttr MemberAttributes.Public)

/// Adds header element properties to given type.
let private addHeaderProperties (protocol: XRoadProtocol) portBaseTy =
    let choose = headerMapping >> (match protocol with Version_20 -> fst | _ -> snd)
    let propName = choose >> snd3
    let docValue = choose >> trd3 >> Some
    [ "asutus"; "andmekogu"; "isikukood"; "id"; "nimi"; "toimik"; "allasutus"; "amet"; "ametniknimi"; "autentija"; "makstud"; "salastada"; "salastada_sertifikaadiga"; "salastatud"; "salastatud_sertifikaadiga" ]
    |> List.fold (fun typ hdr -> typ |> createProperty<string> (propName hdr) (docValue hdr)) portBaseTy
    |> iif (match protocol with Version_20 -> true | _ -> false) (fun typ -> typ |> createProperty<string> (propName "ametnik") (docValue "ametnik"))
    |> createProperty<Nullable<bool>> (propName "asynkroonne") (docValue "asynkroonne")

/// Builds serialization statements for writing main SOAP body element for request message in MakeServiceCall method.
let private createSerializationStatements () =
    [ // Retrieve stream to write contents into:
      Stmt.assign (Expr.var("stream")) ((Expr.var "request" @-> "GetRequestStream") @% [])
      Stmt.declVarWith<StreamWriter> "sw" Expr.nil
      Stmt.tryFinally
          [ Stmt.assign (Expr.var "sw") (Expr.inst<StreamWriter> [Expr.var "stream"])
            Stmt.declVarWith<string> "boundaryMarker" Expr.nil
            // If there are any attachments present, MIME/multipart message has to be sent.
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
            // Writes XML document itself.
            // In case of MIME/multipart message it's in the first MIME container.
            Stmt.declVarWith<XmlWriter> "writer" Expr.nil
            Stmt.tryFinally
                [ Stmt.assign (Expr.var("writer")) ((Expr.typeRefOf<XmlWriter> @-> "Create") @% [Expr.var "sw"])
                  Stmt.ofExpr ((Expr.var "writer" @-> "WriteStartDocument") @% [])
                  Stmt.ofExpr ((Expr.var "writer" @-> "WriteStartElement") @% [Expr.value "soapenv"; Expr.value "Envelope"; Expr.value XmlNamespace.SoapEnv])
                  Stmt.condIf (Expr.var "isEncoded")
                              [Stmt.ofExpr ((Expr.var "writer" @-> "WriteAttributeString") @% [Expr.value "encodingStyle"; Expr.value XmlNamespace.SoapEnv; Expr.value XmlNamespace.SoapEnc])]
                  Stmt.ofExpr ((Expr.var "writer" @-> "WriteStartElement") @% [Expr.value "Header"; Expr.value XmlNamespace.SoapEnv])
                  Stmt.ofExpr ((Expr.var "writeHeaderAction") @%% [Expr.var "writer"])
                  Stmt.ofExpr ((Expr.var "writer" @-> "WriteEndElement") @% [])
                  Stmt.ofExpr ((Expr.var "writeBody") @%% [Expr.var "writer"])
                  Stmt.ofExpr ((Expr.var "writer" @-> "WriteEndElement") @% [])
                  Stmt.ofExpr ((Expr.var "writer" @-> "WriteEndDocument") @% []) ]
                [ Stmt.condIf (Op.isNotNull (Expr.var "writer")) [ (Expr.var "writer" @-> "Dispose") @% [] |> Stmt.ofExpr ] ]
            // Write all attachments in separate containers using binary encoding.
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

/// Builds deserialization statements to extract data from request retrieved from producers adapter server.
let private createDeserializationStatements undescribedFaults =
    // Special reader class is required to support reading faults that are not defined in WSDL document.
    let xmlReaderTypRef = if undescribedFaults then typeRefName "XmlBookmarkReader" else typeRef<XmlReader>
    // If in undescribed faults mode we need to initialize XmlBookmarkReader in place of regular XmlReader.
    let createReaderExpr =
        let readerExpr = Expr.var "GetResponseReader" @%% [Expr.var "response"; Expr.var "responseAttachments"]
        if undescribedFaults then Expr.instOf xmlReaderTypRef [readerExpr] else readerExpr
    // Deserialization statements:
    [ Stmt.assign (Expr.var("response")) ((Expr.var "request" @-> "GetResponse") @% [])
      Stmt.declVarWith<IDictionary<string,Stream>> "responseAttachments" (Expr.inst<Dictionary<string,Stream>> [])
      Stmt.declVarRefWith xmlReaderTypRef "reader" Expr.nil
      Stmt.tryFinally
          [ Stmt.assign (Expr.var("reader")) createReaderExpr
            // Seek for SOAP:Envelope element, exception when not found:
            Stmt.condIfElse (Expr.var "MoveToElement" @%% [Expr.var "reader"; Expr.value "Envelope"; Expr.value XmlNamespace.SoapEnv; Expr.value 0])
                             []
                             [ Stmt.throw<Exception> [Expr.value "Soap envelope element was not found in response message."] ]
            // Seek for SOAP:Body element, exception when not found:
            Stmt.condIfElse (Expr.var "MoveToElement" @%% [Expr.var "reader"; Expr.value "Body"; Expr.value XmlNamespace.SoapEnv; Expr.value 1])
                             []
                             [ Stmt.throw<Exception> [Expr.value "Soap body element was not found in response message."] ]
            // Seek first element of body and try to deserialize it:
            Stmt.ofExpr (Expr.var "MoveToElement" @%% [Expr.var "reader"; Expr.nil; Expr.nil; Expr.value 2])
            Stmt.condIf (Op.boolAnd (Op.equals (Expr.var "reader" @=> "LocalName")
                                               (Expr.value "Fault"))
                                    (Op.equals (Expr.var "reader" @=> "NamespaceURI")
                                               (Expr.value XmlNamespace.SoapEnv)))
                         [Stmt.throw<Exception> [(Expr.var "reader" @-> "ReadInnerXml") @% []]]
            Stmt.ret ((Expr.var "readBody") @%% [Expr.var "reader"; Expr.var "responseAttachments"]) ]
          [ Stmt.condIf (Op.isNotNull (Expr.var "reader"))
                        [(Expr.var "reader" @-> "Dispose") @% [] |> Stmt.ofExpr] ]
    ]

/// Method which handles a single service call from request serialization to response deserialization.
/// General part is handled by method itself; individual logic for each service is injected with callback
/// delegates which serialize and deserialize SOAP body according to their specification.
let private createMakeServiceCallMethod undescribedFaults =
    // Method declaration:
    Meth.create "MakeServiceCall"
    |> Meth.setAttr (MemberAttributes.Family ||| MemberAttributes.Final)
    // Has generic type argument for result entity:
    |> Meth.typeParam "T"
    |> Meth.returnsOf (CodeTypeReference("T"))
    // Method parameters:
    |> Meth.addParam<bool> "isEncoded"
    |> Meth.addParam<IDictionary<string,Stream>> "attachments"
    |> Meth.addParam<Action<XmlWriter>> "writeHeaderAction"
    |> Meth.addParam<Action<XmlWriter>> "writeBody"
    |> Meth.addParamRef (CodeTypeReference("System.Func", typeRef<XmlReader>, typeRef<IDictionary<string,Stream>>, CodeTypeReference("T"))) "readBody"
    // Create request and initialize HTTP headers:
    |> Meth.addStmt (Stmt.declVarWith<WebRequest> "request" ((Expr.typeRefOf<Net.WebRequest> @-> "Create") @% [Expr.var "producerUri"]))
    |> Meth.addStmt (Stmt.assign (Expr.var "request" @=> "Method") (Expr.value "POST"))
    |> Meth.addStmt (Stmt.assign (Expr.var "request" @=> "ContentType") (Expr.value "text/xml; charset=utf-8"))
    |> Meth.addExpr (((Expr.var "request" @=> "Headers") @-> "Set") @% [Expr.value "SOAPAction"; Expr.value ""])
    // Serialize request message:
    |> Meth.addStmt (Stmt.declVarWith<Stream> "stream" Expr.nil)
    |> Meth.addStmt (Stmt.tryFinally (createSerializationStatements())
                                     [ Stmt.condIf (Op.isNotNull (Expr.var "stream"))
                                                   [(Expr.var "stream" @-> "Dispose") @% [] |> Stmt.ofExpr] ])
    // Deserialize response message:
    |> Meth.addStmt (Stmt.declVarWith<WebResponse> "response" Expr.nil)
    |> Meth.addStmt (Stmt.tryFinally (createDeserializationStatements undescribedFaults)
                                     [ Stmt.condIf (Op.isNotNull (Expr.var "response"))
                                                   [(Expr.var "response" @-> "Dispose") @% [] |> Stmt.ofExpr] ])

/// Create base class for all service port defined by producer.
/// Includes common logic for executing service request against producer adapter server.
let makeServicePortBaseType undescribedFaults protocol =
    // Create property and backing field for producer adapter server uri.
    // By default service port soap:address extension location value is used, but user can override that value.
    let addressField = Fld.create<string> "producerUri"
    let addressFieldRef = Expr.this @=> addressField.Name
    let addressProperty = CodeMemberProperty(Name="ProducerUri", Type=CodeTypeReference(typeof<string>))
    addressProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    addressProperty.GetStatements.Add(Stmt.ret addressFieldRef) |> ignore
    addressProperty.SetStatements.Add(Stmt.assign addressFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

    // Create property and backing field for producer name.
    // By default service port xrd/xtee:address extension producer value is used, but user can override that value.
    let producerField = CodeMemberField(typeof<string>, "producerName")
    let producerFieldRef = Expr.this @=> producerField.Name
    let producerProperty = CodeMemberProperty(Name="ProducerName", Type=CodeTypeReference(typeof<string>))
    producerProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
    producerProperty.GetStatements.Add(Stmt.ret producerFieldRef) |> ignore
    producerProperty.SetStatements.Add(Stmt.assign producerFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

    // Primary constructor initializes producer name and adapter server uri to default values.
    let ctor =
        Ctor.create()
        |> Ctor.setAttr MemberAttributes.Family
        |> Ctor.addParam<string> "producerUri"
        |> Ctor.addParam<string> "producerName"
        |> Ctor.addStmt (Stmt.assign (Expr.this @=> "producerUri") (Expr.var("producerUri")))
        |> Ctor.addStmt (Stmt.assign (Expr.this @=> "producerName") (Expr.var("producerName")))

    // Base class declaration:
    Cls.create "AbstractServicePort"
    |> Cls.setAttr (TypeAttributes.Public ||| TypeAttributes.Abstract)
    |> Cls.addMember ctor
    |> Cls.addMember addressField
    |> Cls.addMember addressProperty
    |> Cls.addMember producerField
    |> Cls.addMember producerProperty
    |> Cls.addMember (createMakeServiceCallMethod undescribedFaults)
    |> Cls.addMember (createWriteXRoadHeaderMethod protocol)
    |> Cls.addMember (createTypeFromAssemblyResource("AbstractServicePortExtensions.cs"))
    |> addHeaderProperties protocol
