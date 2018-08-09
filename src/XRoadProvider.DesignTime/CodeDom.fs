module internal XRoad.CodeDom

open FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Diagnostics
open System.Globalization
open System.IO
open System.Reflection
open TypeSchema

(*
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
*)

/// Predefined attributes for code generator.
module Attributes =
    open System.Reflection
    open System.Xml.Linq
    open System.Xml.Schema
    open System.Xml.Serialization
    open XRoad.Serialization.Attributes

    let DebuggerBrowsable =
        { new CustomAttributeData() with
            member __.Constructor = typeof<DebuggerBrowsableAttribute>.GetConstructor([| typeof<DebuggerBrowsableState> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<DebuggerBrowsableState>, DebuggerBrowsableState.Never) |]
            member __.NamedArguments = upcast [| |]
        }

    let XmlAttribute =
        { new CustomAttributeData() with 
            member __.Constructor = typeof<XmlAttributeAttribute>.GetConstructor([| |])
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| CustomAttributeNamedArgument(typeof<XmlAttributeAttribute>.GetProperty("Form"), XmlSchemaForm.Unqualified) |]
        }

    let XmlIgnore =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XmlIgnoreAttribute>.GetConstructor([| |])
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| |]
        }

    let XmlAnyElement =
        { new CustomAttributeData() with 
            member __.Constructor = typeof<XmlAnyElementAttribute>.GetConstructor([| |])
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| |]
        }

    let Optional =
        { new CustomAttributeData() with
            member __.Constructor = typeof<Runtime.InteropServices.OptionalAttribute>.GetConstructor([| |])
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| |]
        }

    let xrdType (typeName: XName) (layout: LayoutKind) =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadTypeAttribute>.GetConstructor([| typeof<string>; typeof<LayoutKind> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, typeName.LocalName); CustomAttributeTypedArgument(typeof<LayoutKind>, layout) |]
            member __.NamedArguments = upcast [| CustomAttributeNamedArgument(typeof<XRoadTypeAttribute>.GetProperty("Namespace"), typeName.NamespaceName) |]
        }

    let xrdAnonymousType (layout: LayoutKind) =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadTypeAttribute>.GetConstructor([| typeof<LayoutKind> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<LayoutKind>, layout) |]
            member __.NamedArguments = upcast [| CustomAttributeNamedArgument(typeof<XRoadTypeAttribute>.GetProperty("IsAnonymous"), true) |]
        }

    let xrdElement idx name (``namespace``: string option) isNullable mergeContent useXop =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadElementAttribute>.GetConstructor([| typeof<int>; typeof<string> |])
            member __.ConstructorArguments =
                upcast [|
                    CustomAttributeTypedArgument(typeof<int>, idx |> MyOption.defaultValue -1)
                    CustomAttributeTypedArgument(typeof<string>, name |> MyOption.defaultValue "")
                |]
            member __.NamedArguments =
                upcast [|
                    if ``namespace``.IsSome then
                        yield CustomAttributeNamedArgument(typeof<XRoadElementAttribute>.GetProperty("Namespace"), ``namespace``.Value)
                    if isNullable then
                        yield CustomAttributeNamedArgument(typeof<XRoadElementAttribute>.GetProperty("IsNullable"), true)
                    if mergeContent then
                        yield CustomAttributeNamedArgument(typeof<XRoadElementAttribute>.GetProperty("MergeContent"), true)
                    if useXop then
                        yield CustomAttributeNamedArgument(typeof<XRoadElementAttribute>.GetProperty("UseXop"), true)
                |]
        }

    let xrdCollection idx itemName (itemNamespace: string option) itemIsNullable mergeContent =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadCollectionAttribute>.GetConstructor([| typeof<int>; typeof<string> |])
            member __.ConstructorArguments =
                upcast [|
                    CustomAttributeTypedArgument(typeof<int>, idx |> MyOption.defaultValue -1)
                    CustomAttributeTypedArgument(typeof<string>, itemName |> MyOption.defaultValue "")
                |]
            member __.NamedArguments =
                upcast [|
                    if itemNamespace.IsSome then
                        yield CustomAttributeNamedArgument(typeof<XRoadCollectionAttribute>.GetProperty("ItemNamespace"), itemNamespace.Value)
                    if itemIsNullable then
                        yield CustomAttributeNamedArgument(typeof<XRoadCollectionAttribute>.GetProperty("ItemIsNullable"), true)
                    if mergeContent then
                        yield CustomAttributeNamedArgument(typeof<XRoadCollectionAttribute>.GetProperty("MergeContent"), true)
                |]
        }

    let xrdOperation name (version: string option) (protocol: XRoadProtocol) messageProtocol =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadOperationAttribute>.GetConstructor([| typeof<string>; typeof<string>; typeof<XRoadProtocol> |])
            member __.ConstructorArguments =
                upcast [|
                    CustomAttributeTypedArgument(typeof<string>, name)
                    CustomAttributeTypedArgument(typeof<string>, version |> MyOption.defaultValue null)
                    CustomAttributeTypedArgument(typeof<XRoadProtocol>, protocol)
                |]
            member __.NamedArguments =
                upcast [|
                    if messageProtocol = Version40 then
                        yield CustomAttributeNamedArgument(typeof<XRoadOperationAttribute>.GetProperty("ProtocolVersion"), "4.0")
                |]
        }

    let xrdRequest name ns isEncoded isMultipart =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadRequestAttribute>.GetConstructor([| typeof<string>; typeof<string> |])
            member __.ConstructorArguments =
                upcast [|
                    CustomAttributeTypedArgument(typeof<string>, name)
                    CustomAttributeTypedArgument(typeof<string>, ns)
                |]
            member __.NamedArguments =
                upcast [|
                    if isEncoded then
                        yield CustomAttributeNamedArgument(typeof<XRoadRequestAttribute>.GetProperty("Encoded"), true)
                    if isMultipart then
                        yield CustomAttributeNamedArgument(typeof<XRoadRequestAttribute>.GetProperty("Multipart"), true)
                |]
        }

    let xrdResponse name ns isEncoded isMultipart (returnType: ProvidedTypeDefinition option) =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadResponseAttribute>.GetConstructor([| typeof<string>; typeof<string> |])
            member __.ConstructorArguments =
                upcast [|
                    CustomAttributeTypedArgument(typeof<string>, name)
                    CustomAttributeTypedArgument(typeof<string>, ns)
                |]
            member __.NamedArguments =
                upcast [|
                    if isEncoded then
                        yield CustomAttributeNamedArgument(typeof<XRoadResponseAttribute>.GetProperty("Encoded"), true)
                    if isMultipart then
                        yield CustomAttributeNamedArgument(typeof<XRoadResponseAttribute>.GetProperty("Multipart"), true)
                    if returnType.IsSome then
                        yield CustomAttributeNamedArgument(typeof<XRoadResponseAttribute>.GetProperty("ReturnType"), returnType.Value)
                |]
        }

    let xrdRequiredHeaders ns (hdrs: string list) =
        { new CustomAttributeData() with
            member __.Constructor = typeof<XRoadRequiredHeadersAttribute>.GetConstructor([| typeof<string>; typeof<string[]> |])
            member __.ConstructorArguments =
                upcast [|
                    CustomAttributeTypedArgument(typeof<string>, ns)
                    CustomAttributeTypedArgument(typeof<string[]>, hdrs |> List.toArray)
                |]
            member __.NamedArguments = upcast [| |]
        }

(*
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
    let declVarOf (t: CodeTypeReference) name e = CodeVariableDeclarationStatement(t, name, e) :> CodeStatement

module Param =
    let create (typ: CodeTypeReference) name = CodeParameterDeclarationExpression(typ, name)
    let describe a (p: CodeParameterDeclarationExpression) = p.CustomAttributes.Add(a) |> ignore; p

/// Functions to create and manipulate type methods.
module Meth =
    let create name = CodeMemberMethod(Name=name)
    let setAttr a (m: CodeMemberMethod) = m.Attributes <- a; m
    let addParamExpr p (m: CodeMemberMethod) = m.Parameters.Add(p) |> ignore; m
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
    let addChainedArg a (c: CodeConstructor) = c.ChainedConstructorArgs.Add(a) |> ignore; c
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
    let implements (i: CodeTypeReference) (c: CodeTypeDeclaration) = c.BaseTypes.Add(i) |> ignore; c

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
        codeCompileUnit.ReferencedAssemblies.Add(typeof<BinaryContent>.Assembly.Location) |> ignore
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
*)

/// Extensions for String module and class.
[<AutoOpen>]
module String =
    let isNullOrEmpty = String.IsNullOrEmpty

    // http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-334.pdf

    let isLetterCharacter (ch: char) =
        match CharUnicodeInfo.GetUnicodeCategory(ch) with
        | UnicodeCategory.UppercaseLetter
        | UnicodeCategory.LowercaseLetter
        | UnicodeCategory.TitlecaseLetter
        | UnicodeCategory.ModifierLetter
        | UnicodeCategory.OtherLetter
        | UnicodeCategory.LetterNumber -> true
        | _ -> false

    let isCombiningCharacter (ch: char) =
        match CharUnicodeInfo.GetUnicodeCategory(ch) with
        | UnicodeCategory.NonSpacingMark
        | UnicodeCategory.SpacingCombiningMark -> true
        | _ -> false

    let inline private isDecimalDigitCharacter (ch: char) = CharUnicodeInfo.GetUnicodeCategory(ch) = UnicodeCategory.DecimalDigitNumber
    let inline private isConnectingCharacter (ch: char) = CharUnicodeInfo.GetUnicodeCategory(ch) = UnicodeCategory.ConnectorPunctuation
    let inline private isFormattingCharacter (ch: char) = CharUnicodeInfo.GetUnicodeCategory(ch) = UnicodeCategory.Format
    let inline private isUnderscoreCharacter (ch: char) = ch = '_'
    let inline private isIdentifierStartCharacter (ch: char) = isLetterCharacter ch || isUnderscoreCharacter ch

    let private isIdentifierPartCharacter (ch: char) =
        isLetterCharacter ch || isDecimalDigitCharacter ch || isConnectingCharacter ch || isCombiningCharacter ch || isFormattingCharacter ch

    let private isValidIdentifier (name: string) =
        if name |> isNullOrEmpty then false else
        if isIdentifierStartCharacter name.[0] |> not then false else
        Array.TrueForAll(name.ToCharArray() |> Array.skip 1, Predicate(isIdentifierPartCharacter))

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
            if not (isValidIdentifier className) then failwithf "invalid name %s" className
            className

        member this.GetValidPropertyName() =
            let propertyName = Text.StringBuilder()
            if not (isIdentifierStartCharacter this.[0]) then propertyName.Append("_") |> ignore
            this.ToCharArray() |> Array.iter (fun c ->
                if isIdentifierPartCharacter c then propertyName.Append(c) |> ignore
                elif propertyName.[propertyName.Length - 1] <> '_' then propertyName.Append('_') |> ignore
                else ())
            let fixedName = propertyName.ToString()
            if not (isValidIdentifier fixedName) then failwithf "Invalid property name `%s`." fixedName
            fixedName

/// Type abstraction for code generator.
type RuntimeType =
    /// Simple types that are presented with system runtime types.
    | PrimitiveType of Type
    /// Types that are provided by generated assembly.
    | ProvidedType of ProvidedTypeDefinition * string
    /// Types that represent collection or array of runtime type.
    | CollectionType of RuntimeType * string * SchemaTypeDefinition option
    /// Binary content types are handled separately.
    | ContentType
(*
    /// Get type name reference for this instance.
    member this.AsCodeTypeReference(?readonly, ?optional): CodeTypeReference =
        let readonly = match readonly with Some(true) -> "readonly " | _ -> ""
        let ctr =
            match this with
            | PrimitiveType(typ) -> CodeTypeReference(typ)
            | ProvidedType(_,name) -> CodeTypeReference(readonly + name)
            | CollectionType(typ,_,_) -> CodeTypeReference(typ.AsCodeTypeReference(), 1)
            | ContentType -> CodeTypeReference(typeof<BinaryContent>)
        match optional with
        | Some(true) ->
            let optionalType = CodeTypeReference(typedefof<Optional.Option<_>>)
            optionalType.TypeArguments.Add(ctr) |> ignore
            optionalType
        | _ -> ctr 
*)

let rec runtimeTypeToSystemType isOptional (rty: RuntimeType) : Type =
    let typ =
        match rty with
        | PrimitiveType(typ) -> typ
        | ProvidedType(typ,_) -> upcast typ
        | CollectionType(itemRty,_,_) -> (itemRty |> runtimeTypeToSystemType false).MakeArrayType()
        | ContentType -> typeof<BinaryContent>
    if isOptional then
        let optType = typedefof<Optional.Option<_>>
        ProvidedTypeBuilder.MakeGenericType(optType, [typ])
    else typ

(*
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
*)

/// Add property to given type with backing field.
/// For optional members, extra field is added to notify if property was assigned or not.
let addProperty (name : string, ty: RuntimeType, isOptional) (owner: ProvidedTypeDefinition) =
    let fixedName = name.GetValidPropertyName()
    let f = ProvidedField(false, fixedName + "__backing", FieldAttributes.Private, ty |> runtimeTypeToSystemType isOptional, null, (K [| Attributes.DebuggerBrowsable |]))
    let p =
        ProvidedProperty(
            fixedName,
            ty |> runtimeTypeToSystemType isOptional,
            getterCode = (fun args -> Expr.FieldGet(Expr.Coerce(args.[0], owner), f)),
            setterCode = (fun args -> Expr.FieldSet(Expr.Coerce(args.[0], owner), f, args.[1]))
        )
    owner.AddMember(f)
    owner.AddMember(p)
    p

let addContentProperty (name: string, ty: RuntimeType, useXop, predefinedValues) (owner: ProvidedTypeDefinition) =
    let name = name.GetValidPropertyName()
    let systemType = ty |> runtimeTypeToSystemType false
    let f = ProvidedField(name + "__backing", systemType)
    let p =
        ProvidedProperty(
            name,
            systemType,
            getterCode = (fun args -> Expr.FieldGet(Expr.Coerce(args.[0], owner), f))
        )
    p.AddCustomAttribute(Attributes.xrdElement None None None false true useXop)
    owner.AddMember(f)
    owner.AddMember(p)
    let ctor =
        let attr = if predefinedValues then MethodAttributes.Private else MethodAttributes.Public
        ProvidedConstructor(
            false,
            attr ||| MethodAttributes.RTSpecialName,
            [| ProvidedParameter("value", systemType) |],
            (fun args -> Expr.FieldSet(Expr.Coerce(args.[0], owner), f, args.[1])),
            None,
            false,
            K [| |]
        )
    owner.AddMember(ctor)
    ctor
