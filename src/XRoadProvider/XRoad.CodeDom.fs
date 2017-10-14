module internal XRoad.CodeDom

open Microsoft.CSharp
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Diagnostics
open System.Globalization
open System.IO
open System.Reflection
open XRoad.TypeSchema

/// Applies function to argument if condition is met.
let iif condition f x = if condition then x |> f else x

let forall args f x = args |> List.iter (f x); x

/// Predefined attributes for code generator.
module Attributes =
    open System.Xml.Linq
    open XRoad.Serialization.Attributes

    let mkXrdType (typeName: XName) (layout: LayoutKind) =
        let typ = typeof<XRoadTypeAttribute>
        let nsprop = typ.GetProperty("Namespace")
        { new CustomAttributeData() with
            member __.Constructor = typ.GetConstructor([| typeof<string>; typeof<LayoutKind> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, typeName.LocalName); CustomAttributeTypedArgument(typeof<LayoutKind>, layout) |]
            member __.NamedArguments = upcast [| CustomAttributeNamedArgument(nsprop, CustomAttributeTypedArgument(typeof<string>, typeName.NamespaceName)) |] }
    (*
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
        Attr.create<XRoadChoiceOptionAttribute>
        |> Attr.addArg (!^ id)
        |> Attr.addArg (!^ name)
        |> Attr.addNamedArg "MergeContent" (!^ mergeContent)

    let xrdCollection (itemName, isNullable) =
        itemName
        |> Option.fold (fun attr name -> attr |> Attr.addArg (!^ name)) Attr.create<XRoadCollectionAttribute>
        |> iif isNullable (fun attr -> attr |> Attr.addNamedArg "ItemIsNullable" (!^ true))
    *)

/// Extensions for String module and class.
[<AutoOpen>]
module String =
    /// Joins sequence of elements with given separator to string.
    let join (sep: string) (arr: seq<'T>) = String.Join(sep, arr)
    
    let private isValidIdentifier =
        Microsoft.CodeAnalysis.CSharp.SyntaxFacts.IsValidIdentifier

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
            if not (isValidIdentifier className) then
                failwithf "invalid name %s" className
            className
        member this.ToPropertyName() =
            let fixedName = this.Replace('.', '_').Replace(' ', '_')
            let fixedName = if this.[0] |> Char.IsDigit then sprintf "_%s" fixedName else fixedName
            if not (isValidIdentifier fixedName) then
                failwithf "Invalid property name `%s`." fixedName
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
    /// Get type name reference for this instance.
    member this.AsCodeTypeReference(?readonly, ?optional): Type =
        let readonly = match readonly with Some(true) -> "readonly " | _ -> ""
        let ctr =
            match this with
            | PrimitiveType(typ) -> typ
            | ProvidedType(typ,_) -> typ :> Type
            | CollectionType(typ,_,_) -> typ.AsCodeTypeReference().MakeArrayType()
            | ContentType -> typeof<XRoad.BinaryContent>
        match optional with
        | Some(true) -> (typedefof<Optional.Option<_>>).MakeGenericType(ctr)
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
let addProperty (name : string, ty: RuntimeType, isOptional) (ctxt: ProvidedTypesContext) (owner: ProvidedTypeDefinition) =
    let fixedName = name.ToPropertyName()
    
    let typ = ty.AsCodeTypeReference(optional = isOptional)

    let f = ctxt.ProvidedField((sprintf "%s__backing" fixedName), typ)
    f.SetFieldAttributes(FieldAttributes.Private)
    // f.AddCustomAttribute(DebuggerBrowsable)
    owner.AddMember(f)

    let p = ctxt.ProvidedProperty(fixedName, typ, getterCode = (fun args -> Expr.FieldGet(args.[0], f)), setterCode = (fun args -> Expr.FieldSet(args.[0], f, args.[1])))
    // MemberAttributes.Public ||| MemberAttributes.Final
    owner.AddMember(p)

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
