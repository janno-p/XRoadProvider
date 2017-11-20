module internal XRoad.CodeDom

open Microsoft.CSharp
open Microsoft.FSharp.Core.CompilerServices
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Diagnostics
open System.Globalization
open System.IO

open XRoad.TypeSchema

/// Get type reference from generic argument.
let typeRef<'T> = CodeTypeReference(typeof<'T>)

/// Applies function to argument if condition is met.
let iif condition f x = if condition then x |> f else x

let forall args f x = args |> List.iter (f x); x

/// Functions to simplify handling of code expressions.
module Expr =
    let fldref (f: CodeMemberField) e = CodeFieldReferenceExpression(e, f.Name) :> CodeExpression
    let this = CodeThisReferenceExpression() :> CodeExpression
    let typeOf (t: CodeTypeReference) = CodeTypeOfExpression(t) :> CodeExpression
    let empty = CodeSnippetExpression() :> CodeExpression
    let nil = CodePrimitiveExpression(null) :> CodeExpression
    let inst<'T> (args: CodeExpression list) = CodeObjectCreateExpression(typeof<'T>, args |> Array.ofList) :> CodeExpression
    let instOf (t: CodeTypeReference) (args: CodeExpression list) = CodeObjectCreateExpression(t, args |> Array.ofList) :> CodeExpression
    let typeRefOf<'T> = CodeTypeReferenceExpression(typeRef<'T>) :> CodeExpression
    let enumValue<'T> valueName = CodePropertyReferenceExpression(typeRefOf<'T>, valueName)
    let cast (t: CodeTypeReference) e = CodeCastExpression(t, e) :> CodeExpression
    let defaultValue (t: CodeTypeReference) = CodeDefaultValueExpression(t) :> CodeExpression

/// Property reference operator.
let (@=>) (target: CodeExpression) (memberName: string) = CodePropertyReferenceExpression(target, memberName) :> CodeExpression

/// Method call operator.
let (@->) (target: CodeExpression) (memberName: string) = CodeMethodReferenceExpression(target, memberName)

/// Add parameter values to method call.
let (@%) (mie: CodeMethodReferenceExpression) (args: CodeExpression list) = CodeMethodInvokeExpression(mie, args |> Array.ofList) :> CodeExpression

/// Variable reference operator
let (!+) name = CodeVariableReferenceExpression(name) :> CodeExpression

/// Value reference operator
let (!^) value = CodePrimitiveExpression(value) :> CodeExpression

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
    open XRoad.Serialization.Attributes

    let private addUnqualifiedForm a = a |> Attr.addNamedArg "Form" (Expr.enumValue<XmlSchemaForm> "Unqualified")

    let DebuggerBrowsable = Attr.create<DebuggerBrowsableAttribute> |> Attr.addArg (Expr.enumValue<DebuggerBrowsableState> "Never")
    let XmlAttribute = Attr.create<XmlAttributeAttribute> |> addUnqualifiedForm
    let XmlIgnore = Attr.create<XmlIgnoreAttribute>
    let XmlAnyElement = Attr.create<XmlAnyElementAttribute>

    let xrdDefType (layout: LayoutKind) =
        Attr.create<XRoadTypeAttribute>
        |> Attr.addArg (Expr.typeRefOf<LayoutKind> @=> (layout.ToString()))

    let xrdType (typeName: XName) (layout: LayoutKind) =
        Attr.create<XRoadTypeAttribute>
        |> Attr.addArg (!^ typeName.LocalName)
        |> Attr.addArg (Expr.typeRefOf<LayoutKind> @=> (layout.ToString()))
        |> Attr.addNamedArg "Namespace" (!^ typeName.NamespaceName)

    let xrdRoot = Attr.create<XRoadTypeAttribute> |> Attr.addArg (Expr.typeRefOf<LayoutKind> @=> (LayoutKind.Sequence.ToString()))

    let xrdElement(elementName, elementNamespace, isNullable, mergeContent) =
        let attr = Attr.create<XRoadElementAttribute>
        elementName |> Option.iter (fun name -> attr |> Attr.addArg (!^ name) |> ignore)
        elementNamespace |> Option.iter (fun ns -> attr |> Attr.addNamedArg "Namespace" (!^ ns) |> ignore)
        if isNullable then attr |> Attr.addNamedArg "IsNullable" (!^ true) |> ignore
        if mergeContent then attr |> Attr.addNamedArg "MergeContent" (!^ true) |> ignore
        attr

    let xrdContent =
        Attr.create<XRoadElementAttribute>
        |> Attr.addNamedArg "MergeContent" (!^ true)

    let xrdChoiceOption (id: int) (name: string) (mergeContent: bool) =
        Attr.create<XRoadElementAttribute>
        |> Attr.addArg (!^ id)
        |> Attr.addArg (!^ name)
        |> Attr.addNamedArg "MergeContent" (!^ mergeContent)

    let xrdCollection (itemName, isNullable) =
        itemName
        |> Option.fold (fun attr name -> attr |> Attr.addArg (!^ name)) Attr.create<XRoadCollectionAttribute>
        |> iif isNullable (fun attr -> attr |> Attr.addNamedArg "ItemIsNullable" (!^ true))
    
    let xrdOperation name (version: string option) (protocol: XRoadProtocol) messageProtocol =
        Attr.create<XRoadOperationAttribute>
        |> Attr.addArg (!^ name)
        |> Attr.addArg (!^ (version |> MyOption.defaultValue null))
        |> Attr.addArg (Expr.typeRefOf<XRoadProtocol> @=> protocol.ToString())
        |> iif (messageProtocol = XRoadMessageProtocolVersion.Version40) (fun attr -> attr |> Attr.addNamedArg "ProtocolVersion" (!^ "4.0"))
    
    let xrdRequest name ns isEncoded isMultipart =
            Attr.create<XRoadRequestAttribute>
            |> Attr.addArg (!^ name)
            |> Attr.addArg (!^ ns)
            |> iif isEncoded (fun attr -> attr |> Attr.addNamedArg "Encoded" (!^ true))
            |> iif isMultipart (fun attr -> attr |> Attr.addNamedArg "Multipart" (!^ true))
        
    let xrdResponse name ns isEncoded isMultipart =
        Attr.create<XRoadResponseAttribute>
        |> Attr.addArg (!^ name)
        |> Attr.addArg (!^ ns)
        |> iif isEncoded (fun attr -> attr |> Attr.addNamedArg "Encoded" (!^ true))
        |> iif isMultipart (fun attr -> attr |> Attr.addNamedArg "Multipart" (!^ true))
        
    let xrdRequiredHeaders ns hdrs =
        hdrs
        |> List.fold
            (fun attr hdr -> attr |> Attr.addArg (!^ hdr))
            (Attr.create<XRoadRequiredHeadersAttribute> |> Attr.addArg (!^ ns))

/// Functions to create and manipulate type fields.
module Fld =
    let create<'T> name = CodeMemberField(typeRef<'T>, name)
    let createRef (typ: CodeTypeReference) name = CodeMemberField(typ, name)
    let describe a (f: CodeMemberField) = f.CustomAttributes.Add(a) |> ignore; f
    let setAttr a (f: CodeMemberField) = f.Attributes <- a; f
    let init e (f: CodeMemberField) = f.InitExpression <- e; f
    let addTo (o: CodeTypeDeclaration) (f: CodeMemberField) = o.Members.Add(f) |> ignore; f

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

    let declVarWith<'T> name e = CodeVariableDeclarationStatement(typeof<'T>, name, e) :> CodeStatement

/// Functions to create and manipulate type methods.
module Meth =
    let create name = CodeMemberMethod(Name=name)
    let setAttr a (m: CodeMemberMethod) = m.Attributes <- a; m
    let addParam<'T> name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typeof<'T>, name)) |> ignore; m
    let addParamRef (typ: CodeTypeReference) name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typ, name)) |> ignore; m
    let addOutParamRef (typ: CodeTypeReference) name (m: CodeMemberMethod) = m.Parameters.Add(CodeParameterDeclarationExpression(typ, name, Direction=FieldDirection.Out)) |> ignore; m
    let addExpr (e: CodeExpression) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m
    let addStmt (e: CodeStatement) (m: CodeMemberMethod) = m.Statements.Add(e) |> ignore; m
    let returns<'T> (m: CodeMemberMethod) = m.ReturnType <- typeRef<'T>; m
    let returnsOf (t: CodeTypeReference) (m: CodeMemberMethod) = m.ReturnType <- t; m
    let describe a (m: CodeMemberMethod) = m.CustomAttributes.Add(a) |> ignore; m

/// Functions to create and manipulate type constructors.
module Ctor =
    let create () = CodeConstructor()
    let setAttr a (c: CodeConstructor) = c.Attributes <- a; c
    let addParam<'T> name (c: CodeConstructor) = c.Parameters.Add(CodeParameterDeclarationExpression(typeof<'T>, name)) |> ignore; c
    let addParamRef (r: CodeTypeReference) name (c: CodeConstructor) = c.Parameters.Add(CodeParameterDeclarationExpression(r, name)) |> ignore; c
    let addStmt (e: CodeStatement) (c: CodeConstructor) = c.Statements.Add(e) |> ignore; c
    let addBaseArg a (c: CodeConstructor) = c.BaseConstructorArgs.Add(a) |> ignore; c
    let addTo (o: CodeTypeDeclaration) (c: CodeConstructor) = o.Members.Add(c) |> ignore; c

/// Functions to simplify operator usage.
module Op =
    let equals lhs rhs = CodeBinaryOperatorExpression(lhs, CodeBinaryOperatorType.IdentityEquality, rhs) :> CodeExpression
    let isNull e = equals e (!^ null)

/// Functions to create and manipulate types.
module Cls =
    let create name = CodeTypeDeclaration(name, IsClass=true)
    let addAttr a (c: CodeTypeDeclaration) = c.TypeAttributes <- c.TypeAttributes ||| a; c
    let setAttr a (c: CodeTypeDeclaration) = c.TypeAttributes <- a; c

    let asStatic (c: CodeTypeDeclaration) =
        c.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" Environment.NewLine)) |> ignore
        c.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
        c

    let addMember m (c: CodeTypeDeclaration) = c.Members.Add(m) |> ignore; c
    let addMembers ms (c: CodeTypeDeclaration) = c.Members.AddRange(ms |> List.toArray); c
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
            |> Attr.addArg (!^ (lines.ToString()))
            |> t.CustomAttributes.Add
            |> ignore
        | None -> ()
        t

/// Functions to create and manipulate arrays.
module Arr =
    let create<'T> (args: CodeExpression list) = CodeArrayCreateExpression(typeRef<'T>, args |> Array.ofList) :> CodeExpression
    let first arr = CodeArrayIndexerExpression(arr, !^ 0)

module Compiler =
    /// Builds new assembly for provided namespace.
    let buildAssembly codeNamespace =
        let codeCompileUnit = CodeCompileUnit()
        codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add(typeof<ITypeProvider>.Assembly.Location) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add(typeof<XRoad.BinaryContent>.Assembly.Location) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add(typeof<NodaTime.LocalDate>.Assembly.Location) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add(typeof<Optional.Option>.Assembly.Location) |> ignore
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
        member this.ToClassName() =
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
        member this.ToPropertyName() =
            let fixedName = this.Replace('.', '_').Replace(' ', '_')
            let fixedName = if this.[0] |> Char.IsDigit then sprintf "_%s" fixedName else fixedName
            if not(CodeGenerator.IsValidLanguageIndependentIdentifier(fixedName))
            then failwithf "Invalid property name `%s`." fixedName
            fixedName

/// Type abstraction for code generator.
type RuntimeType =
    /// Simple types that are presented with system runtime types.
    | PrimitiveType of Type
    /// Types that are provided by generated assembly.
    | ProvidedType of CodeTypeDeclaration * string
    /// Types that represent collection or array of runtime type.
    | CollectionType of RuntimeType * string * SchemaTypeDefinition option
    /// Binary content types are handled separately.
    | ContentType
    /// Get type name reference for this instance.
    member this.AsCodeTypeReference(?readonly, ?optional): CodeTypeReference =
        let readonly = match readonly with Some(true) -> "readonly " | _ -> ""
        let ctr =
            match this with
            | PrimitiveType(typ) -> CodeTypeReference(typ)
            | ProvidedType(_,name) -> CodeTypeReference(readonly + name)
            | CollectionType(typ,_,_) -> CodeTypeReference(typ.AsCodeTypeReference(), 1)
            | ContentType -> CodeTypeReference(typeof<XRoad.BinaryContent>)
        match optional with
        | Some(true) ->
            let optionalType = CodeTypeReference(typedefof<Optional.Option<_>>)
            optionalType.TypeArguments.Add(ctr) |> ignore
            optionalType
        | _ -> ctr

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
let addProperty (name : string, ty: RuntimeType, isOptional) (owner: CodeTypeDeclaration) =
    let fixedName = name.ToPropertyName()
    let f = Fld.createRef (ty.AsCodeTypeReference(optional=isOptional)) (fixedName + "__backing")
            |> Fld.describe Attributes.DebuggerBrowsable
    let p = Prop.createRef (ty.AsCodeTypeReference(optional=isOptional)) fixedName
            |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
            |> Prop.addGetStmt (Stmt.ret (Expr.this @=> f.Name))
            |> Prop.addSetStmt (Stmt.assign (Expr.this @=> f.Name) (Prop.setValue))
    owner |> Cls.addMember(f) |> Cls.addMember(p) |> ignore
    p

let addContentProperty (name: string, ty: RuntimeType) (owner: CodeTypeDeclaration) =
    let name = name.ToPropertyName()
    Fld.createRef (ty.AsCodeTypeReference(true)) (sprintf "%s { get; private set; } //" name)
    |> Fld.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
    |> Fld.describe Attributes.xrdContent
    |> Fld.addTo owner
    |> ignore
    Ctor.create()
    |> Ctor.addParamRef (ty.AsCodeTypeReference()) "value"
    |> Ctor.addStmt (Stmt.assign (Expr.this @=> name) (!+ "value"))
    |> Ctor.addTo owner
