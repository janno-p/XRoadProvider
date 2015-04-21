module internal XRoad.CodeDom

open System.CodeDom
open System.Collections
open System.Diagnostics
open System.IO

let typeRef<'T> = CodeTypeReference(typeof<'T>)
let typeRefExpr<'T> = CodeTypeReferenceExpression(typeRef<'T>)
let private typeCodeExpr (t: CodeTypeMember) = CodeTypeReferenceExpression(t.Name)

let private attrArg expr = CodeAttributeArgument(expr)
let private attrDecl<'T> = CodeAttributeDeclaration(typeRef<'T>)

let private enumExpr<'T> valueName = CodePropertyReferenceExpression(typeRefExpr<'T>, valueName)

let iif condition f x = if condition then x |> f else x

module Expr =
    let value x = CodePrimitiveExpression(x) :> CodeExpression
    let fldref (f: CodeMemberField) e = CodeFieldReferenceExpression(e, f.Name) :> CodeExpression
    let this = CodeThisReferenceExpression() :> CodeExpression
    let typeOf (t: CodeTypeReference) = CodeTypeOfExpression(t) :> CodeExpression
    let var name = CodeVariableReferenceExpression(name) :> CodeExpression
    let empty = CodeSnippetExpression() :> CodeExpression
    let parent = CodeBaseReferenceExpression() :> CodeExpression
    let nil = CodePrimitiveExpression(null) :> CodeExpression
    let inst<'T> (args: CodeExpression list) = CodeObjectCreateExpression(typeof<'T>, args |> Array.ofList) :> CodeExpression

let (@->) (target: CodeExpression) (memberName: string) (args: CodeExpression list) = CodeMethodInvokeExpression(target, memberName, args |> Array.ofList) :> CodeExpression
let (@~>) (target: CodeExpression) (memberName: string) = CodePropertyReferenceExpression(target, memberName) :> CodeExpression

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
    let XmlRoot name ns = Attr.create<XmlRootAttribute> |> Attr.addArg (Expr.value name) |> Attr.addNamedArg "Namespace" (Expr.value ns)

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
    let condIf cond (args: CodeStatement list) = CodeConditionStatement(cond, args |> Array.ofList) :> CodeStatement

    let condIfElse cond (argsIf: CodeStatement list) (argsElse: CodeStatement list) =
        CodeConditionStatement(cond, argsIf |> Array.ofList, argsElse |> Array.ofList) :> CodeStatement

    let ofExpr e = CodeExpressionStatement(e) :> CodeStatement

    let declVar<'T> name (exp: CodeExpression option) =
        match exp with
        | Some(e) -> CodeVariableDeclarationStatement(typeof<'T>, name, e) :> CodeStatement
        | None -> CodeVariableDeclarationStatement(typeof<'T>, name) :> CodeStatement

    let declVarRef (typ: CodeTypeReference) name (exp: CodeExpression option) =
        match exp with
        | Some(e) -> CodeVariableDeclarationStatement(typ, name, e) :> CodeStatement
        | None -> CodeVariableDeclarationStatement(typ, name) :> CodeStatement

module Meth =
    let create name = CodeMemberMethod(Name=name)
    let setAttr a (m: CodeMemberMethod) = m.Attributes <- a; m
    let addParam<'T> name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typeof<'T>, name)) |> ignore; m
    let addParamRef (typ: CodeTypeReference) name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typ, name)) |> ignore; m
    let addExpr (e: CodeExpression) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m
    let addStmt (e: CodeStatement) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m

module Op =
    let equals lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.IdentityEquality, rhs) :> CodeExpression
    let notEquals lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.IdentityInequality, rhs) :> CodeExpression
    let boolOr lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.BooleanOr, rhs) :> CodeExpression
    let isNull e = equals e (Expr.value null)
    let isNotNull e = notEquals e (Expr.value null)

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
