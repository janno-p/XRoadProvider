module internal XRoad.CodeDom.Common

open Microsoft.CSharp
open Microsoft.FSharp.Core.CompilerServices
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections
open System.Diagnostics
open System.Globalization
open System.IO

open XRoad.TypeSchema

/// Get type reference from generic argument.
let typeRef<'T> = CodeTypeReference(typeof<'T>)

/// Get type reference from given type name.
let typeRefName name = CodeTypeReference(name: string)

/// Applies function to argument if condition is met.
let iif condition f x = if condition then x |> f else x

/// Functions to simplify handling of code expressions.
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

/// Property reference operator.
let (@=>) (target: CodeExpression) (memberName: string) = CodePropertyReferenceExpression(target, memberName) :> CodeExpression

/// Method call operator.
let (@->) (target: CodeExpression) (memberName: string) = CodeMethodReferenceExpression(target, memberName)

/// Add generic arguments to method call.
let (@<>) (mie: CodeMethodReferenceExpression) (args: CodeTypeReference list) = args |> List.iter (mie.TypeArguments.Add >> ignore); mie

/// Add parameter values to method call.
let (@%) (mie: CodeMethodReferenceExpression) (args: CodeExpression list) = CodeMethodInvokeExpression(mie, args |> Array.ofList) :> CodeExpression

/// Add parameters to delegate call.
let (@%%) target args = CodeDelegateInvokeExpression(target, args |> Array.ofList) :> CodeExpression

/// Array indexer operator.
let (@?) target i = CodeArrayIndexerExpression(target, [| i |]) :> CodeExpression

/// Variable reference operator
let (!+) = Expr.var

/// Value reference operator
let (!^) = Expr.value

/// Functions to create and manipulate code attributes.
module Attr =
    let create<'T> = CodeAttributeDeclaration(typeRef<'T>)
    let addArg e (a: CodeAttributeDeclaration) = a.Arguments.Add(CodeAttributeArgument(e)) |> ignore; a
    let addNamedArg name e (a: CodeAttributeDeclaration) = a.Arguments.Add(CodeAttributeArgument(name, e)) |> ignore; a

/// Predefined attributes for code generator.
module Attributes =
    open System.Xml.Linq
    open System.Xml.Schema
    open System.Xml.Serialization

    let private addUnqualifiedForm a = a |> Attr.addNamedArg "Form" (Expr.enumValue<XmlSchemaForm> "Unqualified")
    let private addNullable isNillable a = a |> iif isNillable (fun a -> a |> Attr.addNamedArg "IsNullable" (Expr.value true))

    let DebuggerBrowsable = Attr.create<DebuggerBrowsableAttribute> |> Attr.addArg (Expr.enumValue<DebuggerBrowsableState> "Never")
    let XmlAttribute = Attr.create<XmlAttributeAttribute> |> addUnqualifiedForm
    let XmlAttributeWithName (name: string) = Attr.create<XmlAttributeAttribute> |> Attr.addArg (Expr.value name) |> addUnqualifiedForm
    let XmlText = Attr.create<XmlTextAttribute>

    let XmlType(typeName: XName) =
        Attr.create<XmlTypeAttribute>
        |> Attr.addArg (Expr.value typeName.LocalName)
        |> Attr.addNamedArg "Namespace" (Expr.value typeName.NamespaceName)

    let XmlTypeExclude = Attr.create<XmlTypeAttribute> |> Attr.addNamedArg "IncludeInSchema" (Expr.value false)
    let XmlInclude(providedTy: CodeTypeReference) = Attr.create<XmlIncludeAttribute> |> Attr.addArg (Expr.typeOf providedTy)
    let XmlElement(isNillable) = Attr.create<XmlElementAttribute> |> addUnqualifiedForm |> addNullable isNillable
    let XmlElement2(name, typ) = Attr.create<XmlElementAttribute> |> Attr.addArg (Expr.value name) |> Attr.addArg (Expr.typeOf typ) |> addUnqualifiedForm
    let XmlArray(isNillable) = Attr.create<XmlArrayAttribute> |> addUnqualifiedForm |> addNullable isNillable
    let XmlArrayItem(name, isNillable) = Attr.create<XmlArrayItemAttribute> |> Attr.addArg (Expr.value name) |> addUnqualifiedForm |> addNullable isNillable
    let XmlRoot name ns = Attr.create<XmlRootAttribute> |> Attr.addArg (Expr.value name) |> Attr.addNamedArg "Namespace" (Expr.value ns)
    let XmlIgnore = Attr.create<XmlIgnoreAttribute>
    let XmlAnyElement = Attr.create<XmlAnyElementAttribute>
    let XmlEnum name = Attr.create<XmlEnumAttribute> |> Attr.addArg (Expr.value name)
    let XmlChoiceIdentifier name = Attr.create<XmlChoiceIdentifierAttribute> |> Attr.addArg (Expr.value name)

/// Functions to create and manipulate type fields.
module Fld =
    let create<'T> name = CodeMemberField(typeRef<'T>, name)
    let createEnum enumName valueName = CodeMemberField((enumName : string), valueName)
    let createRef (typ: CodeTypeReference) name = CodeMemberField(typ, name)
    let describe a (f: CodeMemberField) = f.CustomAttributes.Add(a) |> ignore; f
    let setAttr a (f: CodeMemberField) = f.Attributes <- a; f

/// Functions to create and manipulate type properties.
module Prop =
    let create<'T> name = CodeMemberProperty(Name=name, Type=typeRef<'T>)
    let createRef (typ: CodeTypeReference) name = CodeMemberProperty(Name=name, Type=typ)
    let setAttr a (p: CodeMemberProperty) = p.Attributes <- a; p
    let addGetStmt (s: CodeStatement) (p: CodeMemberProperty) = p.GetStatements.Add(s) |> ignore; p
    let addSetStmt (s: CodeStatement) (p: CodeMemberProperty) = p.SetStatements.Add(s) |> ignore; p
    let setValue = CodePropertySetValueReferenceExpression() :> CodeExpression
    let describe a (p: CodeMemberProperty) = p.CustomAttributes.Add(a) |> ignore; p

/// Functions to simplify handling of code statements.
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
    let whileLoop testExpr statements = CodeIterationStatement(CodeSnippetStatement(), testExpr, CodeSnippetStatement(), statements |> Array.ofList) :> CodeStatement
    let tryFinally tryStmts finallyStmts = CodeTryCatchFinallyStatement(tryStmts |> Array.ofList, [| |], finallyStmts |> Array.ofList) :> CodeStatement
    let forLoop initStmt testExpr incrStmt statements = CodeIterationStatement(initStmt, testExpr, incrStmt, statements |> Array.ofList) :> CodeStatement

/// Functions to create and manipulate type methods.
module Meth =
    let create name = CodeMemberMethod(Name=name)
    let setAttr a (m: CodeMemberMethod) = m.Attributes <- a; m
    let addParam<'T> name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typeof<'T>, name)) |> ignore; m

    let addOptParam<'T> name (m: CodeMemberMethod) =
        let p = CodeParameterDeclarationExpression(typeof<'T>, name)
        p.CustomAttributes.Add(Attr.create<System.Runtime.InteropServices.OptionalAttribute>) |> ignore
        p.CustomAttributes.Add(Attr.create<System.Runtime.InteropServices.DefaultParameterValueAttribute> |> Attr.addArg Expr.nil) |> ignore
        m.Parameters.Add(p) |> ignore
        m

    let addParamRef (typ: CodeTypeReference) name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typ, name)) |> ignore; m
    let addExpr (e: CodeExpression) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m
    let addStmt (e: CodeStatement) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m
    let returns<'T> (m: CodeMemberMethod) = m.ReturnType <- typeRef<'T>; m
    let returnsOf (t: CodeTypeReference) (m: CodeMemberMethod) = m.ReturnType <- t; m
    let typeParam (name: string) (m: CodeMemberMethod) = m.TypeParameters.Add(name) |> ignore; m

/// Functions to create and manipulate type constructors.
module Ctor =
    let create () = CodeConstructor()
    let setAttr a (c: CodeConstructor) = c.Attributes <- a; c
    let addParam<'T> name (c: CodeConstructor) = c.Parameters.Add(CodeParameterDeclarationExpression(typeof<'T>, name)) |> ignore; c
    let addStmt (e: CodeStatement) (c: CodeConstructor) = c.Statements.Add(e) |> ignore; c
    let addBaseArg a (c: CodeConstructor) = c.BaseConstructorArgs.Add(a) |> ignore; c

/// Functions to simplify operator usage.
module Op =
    let equals lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.IdentityEquality, rhs) :> CodeExpression
    let notEquals lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.IdentityInequality, rhs) :> CodeExpression
    let boolOr lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.BooleanOr, rhs) :> CodeExpression
    let boolAnd lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.BooleanAnd, rhs) :> CodeExpression
    let isNull e = equals e (Expr.value null)
    let isNotNull e = notEquals e (Expr.value null)
    let ge lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.GreaterThanOrEqual, rhs) :> CodeExpression
    let greater lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.GreaterThan, rhs) :> CodeExpression
    let plus lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.Add, rhs) :> CodeExpression

/// Functions to create and manipulate types.
module Cls =
    let createEnum name = CodeTypeDeclaration(name, IsEnum=true)
    let create name = CodeTypeDeclaration(name, IsClass=true)
    let addAttr a (c: CodeTypeDeclaration) = c.TypeAttributes <- c.TypeAttributes ||| a; c
    let setAttr a (c: CodeTypeDeclaration) = c.TypeAttributes <- a; c

    let asStatic (c: CodeTypeDeclaration) =
        c.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" Environment.NewLine)) |> ignore
        c.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
        c

    let addMember m (c: CodeTypeDeclaration) = c.Members.Add(m) |> ignore; c
    let describe a (t: CodeTypeDeclaration) = t.CustomAttributes.Add(a) |> ignore; t
    let setParent (p: CodeTypeReference) (t: CodeTypeDeclaration) = t.BaseTypes.Add(p) |> ignore; t

module Code =
    let comment (text: string option) (t: #CodeTypeMember) =
        match text with
        | Some(text) ->
            let lines =
                text.Split('\n')
                |> Array.map (fun s -> s.Trim())
                |> Array.fold (fun (b: Text.StringBuilder) line ->
                    match line with
                    | "" -> b.AppendLine()
                    | x -> if b.Length > 0 && b.[b.Length - 1] <> '\n'
                           then b.AppendFormat(" {0}", x)
                           else b.Append(x)) (Text.StringBuilder())
            Attr.create<TypeProviderXmlDocAttribute>
            |> Attr.addArg (Expr.value (lines.ToString()))
            |> t.CustomAttributes.Add
            |> ignore
        | None -> ()
        t

/// Functions to create and manipulate arrays.
module Arr =
    let createOfSize<'T> (size: int) = CodeArrayCreateExpression(typeRef<'T>, size) :> CodeExpression
    let create<'T> (args: CodeExpression list) = CodeArrayCreateExpression(typeRef<'T>, args |> Array.ofList) :> CodeExpression

module Compiler =
    /// Builds new assembly for provided namespace.
    let buildAssembly codeNamespace =
        let codeCompileUnit = CodeCompileUnit()
        codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add(typeof<ITypeProvider>.Assembly.Location) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.dll") |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.Net.dll") |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.Numerics.dll") |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore
        // Assembly is placed under temporary files path.
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

/// Extensions for String module and class.
[<AutoOpen>]
module String =
    /// Joins sequence of elements with given separator to string.
    let join (sep: string) (arr: seq<'T>) = String.Join(sep, arr)

    type String with
        /// Converts given XML namespace to class name.
        member this.toClassName() =
            // Remove `http://` prefix from namespace if present.
            let str =
                match this.StartsWith("http://") with
                | true -> this.Substring(7)
                | _ -> this
            // Remove special symbols from class name.
            let className =
                str.Split('/')
                |> Array.map (fun p ->
                    p.Split('.')
                    |> Array.map (fun x -> CultureInfo.InvariantCulture.TextInfo.ToTitleCase(x.ToLower()).Replace("-", ""))
                    |> join "")
                |> join "_"
            // Check validity of generated class name.
            if not <| CodeGenerator.IsValidLanguageIndependentIdentifier(className)
            then failwithf "invalid name %s" className
            className

/// Type abstraction for code generator.
type RuntimeType =
    /// Simple types that are presented with system runtime types.
    | PrimitiveType of Type
    /// Types that are provided by generated assembly.
    | ProvidedType of CodeTypeDeclaration * string
    /// Types that represent collection or array of runtime type.
    | CollectionType of RuntimeType * string * SchemaType option
    /// Binary content types are handled separately.
    | ContentType
    /// Get type name reference for this instance.
    member this.AsCodeTypeReference() =
        match this with
        | PrimitiveType(typ) -> CodeTypeReference(typ)
        | ProvidedType(_,name) -> CodeTypeReference(name)
        | CollectionType(typ,_,_) -> CodeTypeReference(typ.AsCodeTypeReference(), 1)
        | ContentType -> CodeTypeReference("BinaryContent")

/// Add special XmlReader class to allow seeking previous parts of XML document.
/// Required for reading XML documents which do not comply with given schema.
let createTypeFromAssemblyResource resourceName =
    let assembly = typeof<RuntimeType>.Assembly
    use stream = assembly.GetManifestResourceStream(resourceName)
    use reader = new StreamReader(stream)
    CodeSnippetTypeMember(reader.ReadToEnd())

/// Create property with backing field.
let createProperty<'T> name doc (ownerType: CodeTypeDeclaration) =
    let backingField =
        Fld.create<'T> (name + "__backing")
        |> Fld.describe Attributes.DebuggerBrowsable
    let property =
        Prop.create<'T> name
        |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        |> Prop.addGetStmt (Expr.this |> Expr.fldref backingField |> Stmt.ret)
        |> Prop.addSetStmt (Prop.setValue |> Stmt.assign (Expr.this |> Expr.fldref backingField))
        |> Code.comment doc
    ownerType
    |> Cls.addMember backingField
    |> Cls.addMember property

/// Add property to given type with backing field.
/// For optional members, extra field is added to notify if property was assigned or not.
let addProperty (name, ty: RuntimeType, isOptional) (owner: CodeTypeDeclaration) =
    let sf =
        if isOptional then
            let f = Fld.create<bool> (name + "__specified")
                    |> Fld.describe Attributes.DebuggerBrowsable
            let p = Prop.create<bool> (name + "Specified")
                    |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
                    |> Prop.addGetStmt (Stmt.ret (Expr.this @=> f.Name))
            owner |> Cls.addMember(f) |> Cls.addMember(p) |> ignore
            Some(f)
        else None
    let f = Fld.createRef (ty.AsCodeTypeReference()) (name + "__backing")
            |> Fld.describe Attributes.DebuggerBrowsable
    let p = Prop.createRef (ty.AsCodeTypeReference()) name
            |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
            |> Prop.addGetStmt (Stmt.ret (Expr.this @=> f.Name))
            |> Prop.addSetStmt (Stmt.assign (Expr.this @=> f.Name) (Prop.setValue))
            |> iif (sf.IsSome) (fun x -> x |> Prop.addSetStmt (Stmt.assign (Expr.this @=> sf.Value.Name) (Expr.value true)))
    owner |> Cls.addMember(f) |> Cls.addMember(p) |> ignore
    p
