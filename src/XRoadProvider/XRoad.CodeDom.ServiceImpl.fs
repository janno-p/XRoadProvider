module internal XRoad.CodeDom.ServiceImpl

open System
open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Net
open System.Xml

open XRoad
open XRoad.CodeDom.Common
open XRoad.Common

/// Types which extend BinaryContent types should inherit its properties from BinaryContent runtime type also.
/// Create overloaded constructor to match BinaryContent constructor.
let inheritBinaryContent typ =
    typ
    |> Cls.setParent (ContentType.AsCodeTypeReference())
    |> Cls.addMember (Ctor.create()
                      |> Ctor.setAttr MemberAttributes.Public
                      |> Ctor.addParam<System.IO.Stream> "content"
                      |> Ctor.addBaseArg (Expr.var "content"))
    |> Cls.addMember (Ctor.create()
                      |> Ctor.setAttr MemberAttributes.Public
                      |> Ctor.addParam<string> "contentId"
                      |> Ctor.addParam<System.IO.Stream> "content"
                      |> Ctor.addBaseArg (Expr.var "contentId")
                      |> Ctor.addBaseArg (Expr.var "content"))
    |> Cls.addMember (Ctor.create()
                      |> Ctor.setAttr MemberAttributes.Public)

/// Builds deserialization statements to extract data from request retrieved from producers adapter server.
let private createDeserializationStatements undescribedFaults =
    // Special reader class is required to support reading faults that are not defined in WSDL document.
    let xmlReaderTypRef = if undescribedFaults then typeRef<XRoad.XmlBookmarkReader> else typeRef<XmlReader>
    // If in undescribed faults mode we need to initialize XmlBookmarkReader in place of regular XmlReader.
    let createReaderExpr =
        let readerExpr = Expr.var "GetResponseReader" @%% [Expr.var "response"]
        if undescribedFaults then Expr.instOf xmlReaderTypRef [readerExpr] else readerExpr
    // Deserialization statements:
    [ Stmt.assign (Expr.var("response")) ((Expr.var "request" @-> "GetResponse") @% [])
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
            Stmt.ret ((Expr.var "readBody") @%% [Expr.var "reader"]) ]
          [ Stmt.condIf (Op.isNotNull (Expr.var "reader"))
                        [(Expr.var "reader" @-> "Dispose") @% [] |> Stmt.ofExpr] ]
    ]
