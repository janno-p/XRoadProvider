module internal XRoad.CodeDom

open Microsoft.CSharp
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections
open System.Diagnostics
open System.Globalization
open System.IO

let typeRef<'T> = CodeTypeReference(typeof<'T>)
let typeRefName name = CodeTypeReference(name: string)

let private attrArg expr = CodeAttributeArgument(expr)
let private attrDecl<'T> = CodeAttributeDeclaration(typeRef<'T>)

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
    let instOf (t: CodeTypeReference) (args: CodeExpression list) = CodeObjectCreateExpression(t, args |> Array.ofList) :> CodeExpression
    let typeRefOf<'T> = CodeTypeReferenceExpression(typeRef<'T>) :> CodeExpression
    let typeRef (t: CodeTypeMember) = CodeTypeReferenceExpression(t.Name) :> CodeExpression
    let enumValue<'T> valueName = CodePropertyReferenceExpression(typeRefOf<'T>, valueName)
    let cast (t: CodeTypeReference) e = CodeCastExpression(t, e) :> CodeExpression
    let code text = CodeSnippetExpression(text) :> CodeExpression

let (@=>) (target: CodeExpression) (memberName: string) = CodePropertyReferenceExpression(target, memberName) :> CodeExpression

let (@->) (target: CodeExpression) (memberName: string) = CodeMethodReferenceExpression(target, memberName)
let (@<>) (mie: CodeMethodReferenceExpression) (args: CodeTypeReference list) = args |> List.iter (mie.TypeArguments.Add >> ignore); mie
let (@%) (mie: CodeMethodReferenceExpression) (args: CodeExpression list) = CodeMethodInvokeExpression(mie, args |> Array.ofList) :> CodeExpression

let (@%%) target args = CodeDelegateInvokeExpression(target, args |> Array.ofList) :> CodeExpression

module Attr =
    let create<'T> = CodeAttributeDeclaration(typeRef<'T>)
    let addArg e (a: CodeAttributeDeclaration) = a.Arguments.Add(CodeAttributeArgument(e)) |> ignore; a
    let addNamedArg name e (a: CodeAttributeDeclaration) = a.Arguments.Add(CodeAttributeArgument(name, e)) |> ignore; a

module Attributes =
    open System.Xml.Linq
    open System.Xml.Schema
    open System.Xml.Serialization

    let private addUnqualifiedForm a = a |> Attr.addNamedArg "Form" (Expr.enumValue<XmlSchemaForm> "Unqualified")
    let private addNullable isNillable a = a |> iif isNillable (fun a -> a |> Attr.addNamedArg "IsNullable" (Expr.value true))

    let DebuggerBrowsable = Attr.create<DebuggerBrowsableAttribute> |> Attr.addArg (Expr.enumValue<DebuggerBrowsableState> "Never")
    let XmlAttribute = Attr.create<XmlAttributeAttribute> |> addUnqualifiedForm
    let XmlText = Attr.create<XmlTextAttribute>

    let XmlType(typeName: XName) =
        Attr.create<XmlTypeAttribute>
        |> Attr.addArg (Expr.value typeName.LocalName)
        |> Attr.addNamedArg "Namespace" (Expr.value typeName.NamespaceName)

    let XmlInclude(providedTy: CodeTypeDeclaration) = Attr.create<XmlIncludeAttribute> |> Attr.addArg (Expr.typeRef providedTy)
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
    let declVar<'T> name = CodeVariableDeclarationStatement(typeof<'T>, name) :> CodeStatement
    let declVarWith<'T> name e = CodeVariableDeclarationStatement(typeof<'T>, name, e) :> CodeStatement
    let declVarRef (typ: CodeTypeReference) name = CodeVariableDeclarationStatement(typ, name) :> CodeStatement
    let declVarRefWith (typ: CodeTypeReference) name e = CodeVariableDeclarationStatement(typ, name, e) :> CodeStatement
    let throw<'T> (args: CodeExpression list) = CodeThrowExceptionStatement(Expr.inst<'T> args) :> CodeStatement
    let whl testExpression statements = CodeIterationStatement(CodeSnippetStatement(), testExpression, CodeSnippetStatement(), statements |> Array.ofList) :> CodeStatement
    let tryFinally tryStmts finallyStmts = CodeTryCatchFinallyStatement(tryStmts |> Array.ofList, [| |], finallyStmts |> Array.ofList) :> CodeStatement

module Meth =
    let create name = CodeMemberMethod(Name=name)
    let setAttr a (m: CodeMemberMethod) = m.Attributes <- a; m
    let addParam<'T> name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typeof<'T>, name)) |> ignore; m
    let addParamRef (typ: CodeTypeReference) name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typ, name)) |> ignore; m
    let addExpr (e: CodeExpression) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m
    let addStmt (e: CodeStatement) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m
    let returns<'T> (m: CodeMemberMethod) = m.ReturnType <- typeRef<'T>; m

module Ctor =
    let create () = CodeConstructor()
    let setAttr a (c: CodeConstructor) = c.Attributes <- a; c
    let addParam<'T> name (c: CodeConstructor) = c.Parameters.Add(CodeParameterDeclarationExpression(typeof<'T>, name)) |> ignore; c
    let addStmt (e: CodeStatement) (c: CodeConstructor) = c.Statements.Add(e) |> ignore; c

module Op =
    let equals lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.IdentityEquality, rhs) :> CodeExpression
    let notEquals lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.IdentityInequality, rhs) :> CodeExpression
    let boolOr lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.BooleanOr, rhs) :> CodeExpression
    let boolAnd lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.BooleanAnd, rhs) :> CodeExpression
    let isNull e = equals e (Expr.value null)
    let isNotNull e = notEquals e (Expr.value null)
    let ge lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.GreaterThanOrEqual, rhs) :> CodeExpression

module Cls =
    let create name = CodeTypeDeclaration(name, IsClass=true)
    let addAttr a (c: CodeTypeDeclaration) = c.TypeAttributes <- c.TypeAttributes ||| a; c
    let setAttr a (c: CodeTypeDeclaration) = c.TypeAttributes <- a; c

    let asStatic (c: CodeTypeDeclaration) =
        c.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" Environment.NewLine)) |> ignore
        c.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
        c

    let addMember m (c: CodeTypeDeclaration) = c.Members.Add(m) |> ignore; c

module Arr =
    let createOfSize<'T> (size: int) = CodeArrayCreateExpression(typeRef<'T>, size) :> CodeExpression
    let create<'T> (args: CodeExpression list) = CodeArrayCreateExpression(typeRef<'T>, args |> Array.ofList) :> CodeExpression

module Compiler =
    let buildAssembly codeNamespace =
        let codeCompileUnit = CodeCompileUnit()
        codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.dll") |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.Net.dll") |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.Numerics.dll") |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore
        let fileName = Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> sprintf "%A.dll")
        use codeProvider = new CSharpCodeProvider()
        let parameters = CompilerParameters(OutputAssembly=fileName, GenerateExecutable=false)
        //parameters.CompilerOptions <- "/doc:" + Path.ChangeExtension(fileName, "xml")
        ( use wr = new StreamWriter(File.Open(Path.ChangeExtension(fileName, "cs"), FileMode.Create, FileAccess.Write))
          codeProvider.GenerateCodeFromCompileUnit(codeCompileUnit, wr, CodeGeneratorOptions()))
        let compilerResults = codeProvider.CompileAssemblyFromDom(parameters, [| codeCompileUnit |])
        if compilerResults.Errors.Count > 0 then
            printfn "%A" compilerResults.Errors
        compilerResults.CompiledAssembly

[<AutoOpen>]
module String =
    let join (sep: string) (arr: seq<'T>) = String.Join(sep, arr)

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
