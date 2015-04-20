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
    let fldref (f: CodeMemberField) e = CodeFieldReferenceExpression(e, f.Name) :> CodeExpression
    let this = CodeThisReferenceExpression() :> CodeExpression
    let typeOf (t: CodeTypeReference) = CodeTypeOfExpression(t) :> CodeExpression

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
    let XmlElement2(name, typ) = Attr.create<XmlElementAttribute> |> Attr.addArg (Expr.value name) |> Attr.addArg (Expr.typeOf typ) |> addUnqualifiedForm
    let XmlArray(isNillable) = Attr.create<XmlArrayAttribute> |> addUnqualifiedForm |> addNullable isNillable
    let XmlArrayItem(name) = Attr.create<XmlArrayItemAttribute> |> Attr.addArg (Expr.value name) |> addUnqualifiedForm //|> addNullable isNillable

module Fld =
    let create<'T> name = CodeMemberField(typeRef<'T>, name)
    let addAttr a (f: CodeMemberField) = f.CustomAttributes.Add(a) |> ignore; f

module Typ =
    let addMember m (t: CodeTypeDeclaration) = t.Members.Add(m) |> ignore; t

module Prop =
    let create<'T> name = CodeMemberProperty(Name=name, Type=typeRef<'T>)
    let setAttr a (p: CodeMemberProperty) = p.Attributes <- a; p
    let addGetStmt (s: CodeStatement) (p: CodeMemberProperty) = p.GetStatements.Add(s) |> ignore; p
    let addSetStmt (s: CodeStatement) (p: CodeMemberProperty) = p.SetStatements.Add(s) |> ignore; p
    let addDoc d (p: CodeMemberProperty) = p.Comments.Add(CodeCommentStatement(d, true)) |> ignore; p
    let setValue = CodePropertySetValueReferenceExpression() :> CodeExpression

module Stmt =
    let ret e = CodeMethodReturnStatement(e) :> CodeStatement
    let assign le re = CodeAssignStatement(le, re) :> CodeStatement

let makeChoiceType() =
    ()

let createXmlBookmarkReaderType() =
    let assembly = typeof<XRoad.Parser.MessagePart>.Assembly
    use stream = assembly.GetManifestResourceStream("XmlBookmarkReader.cs")
    use reader = new StreamReader(stream)
    CodeSnippetTypeMember(reader.ReadToEnd())

let createProperty<'T> name doc (ownerType: CodeTypeDeclaration) =
    let backingField =
        Fld.create<'T> (name + "__backing")
        |> Fld.addAttr Attributes.DebuggerBrowsable
    let property =
        Prop.create<'T> name
        |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        |> Prop.addGetStmt (Expr.this |> Expr.fldref backingField |> Stmt.ret)
        |> Prop.addSetStmt (Prop.setValue |> Stmt.assign (Expr.this |> Expr.fldref backingField))
        |> Prop.addDoc doc
    ownerType
    |> Typ.addMember backingField
    |> Typ.addMember property
