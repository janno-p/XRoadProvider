module internal XRoad.CodeDom

open System.CodeDom
open System.Collections
open System.Diagnostics
open System.IO

let private typeRef<'T> = CodeTypeReference(typeof<'T>)
let private typeRefExpr<'T> = CodeTypeReferenceExpression(typeRef<'T>)
let private typeCodeExpr (t: CodeTypeMember) = CodeTypeReferenceExpression(t.Name)

let private attrArg expr = CodeAttributeArgument(expr)
let private attrDecl<'T> = CodeAttributeDeclaration(typeRef<'T>)

let private enumExpr<'T> valueName = CodePropertyReferenceExpression(typeRefExpr<'T>, valueName)

let private iif condition f x = if condition then x |> f else x

module Expr =
    let value x = CodePrimitiveExpression(x) :> CodeExpression

module Attr =
    let create<'T> = CodeAttributeDeclaration(typeRef<'T>)
    let addArg e (a: CodeAttributeDeclaration) = a.Arguments.Add(CodeAttributeArgument(e)) |> ignore; a
    let addNamedArg name e (a: CodeAttributeDeclaration) = a.Arguments.Add(CodeAttributeArgument(name, e)) |> ignore; a

module Attributes =
    open System.Xml.Linq
    open System.Xml.Schema
    open System.Xml.Serialization

    let private addUnqualifiedForm a = a |> Attr.addNamedArg "Form" (enumExpr<XmlSchemaForm> "Unqualified")
    let private addNullable isNillable a = a |> iif isNillable (fun a -> a |> Attr.addNamedArg "IsNullable" (Expr.value true))

    let DebuggerBrowsable = Attr.create<DebuggerBrowsableAttribute> |> Attr.addArg (enumExpr<DebuggerBrowsableState> "Never")
    let XmlAttribute = Attr.create<XmlAttributeAttribute> |> addUnqualifiedForm
    let XmlText = Attr.create<XmlTextAttribute>

    let XmlType(typeName: XName) =
        Attr.create<XmlTypeAttribute>
        |> Attr.addArg (Expr.value typeName.LocalName)
        |> Attr.addNamedArg "Namespace" (Expr.value typeName.NamespaceName)

    let XmlInclude(providedTy: CodeTypeDeclaration) = Attr.create<XmlIncludeAttribute> |> Attr.addArg (typeCodeExpr providedTy)
    let XmlElement(isNillable) = Attr.create<XmlElementAttribute> |> addUnqualifiedForm |> addNullable isNillable
    let XmlArray(isNillable) = Attr.create<XmlArrayAttribute> |> addUnqualifiedForm |> addNullable isNillable
    let XmlArrayItem(name) = Attr.create<XmlArrayItemAttribute> |> Attr.addArg (Expr.value name) |> addUnqualifiedForm //|> addNullable isNillable

let makeChoiceType() =
    ()

let createXmlBookmarkReaderType() =
    let assembly = typeof<XRoad.Parser.MessagePart>.Assembly
    use stream = assembly.GetManifestResourceStream("XmlBookmarkReader.cs")
    use reader = new StreamReader(stream)
    CodeSnippetTypeMember(reader.ReadToEnd())
