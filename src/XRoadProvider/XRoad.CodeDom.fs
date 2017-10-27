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
    open System.Xml.Serialization
    open System.Xml.Schema
    open XRoad.Serialization.Attributes
    
    let mkXmlAttribute () =
        let typ = typeof<XmlAttributeAttribute>
        let nsprop = typ.GetProperty("Form")
        { new CustomAttributeData() with
            member __.Constructor = typ.GetConstructor([| |])
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| CustomAttributeNamedArgument(nsprop, CustomAttributeTypedArgument(typeof<XmlSchemaForm>, XmlSchemaForm.Unqualified)) |] }
    
    let mkXmlAnyElement () =
        let typ = typeof<XmlAnyElementAttribute>
        { new CustomAttributeData() with
            member __.Constructor = typ.GetConstructor([| |])
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| |] }
            
    let mkXrdChoiceOption (id: int) (name: string) (mergeContent: bool) =
        let typ = typeof<XRoadChoiceOptionAttribute>
        { new CustomAttributeData() with
            member __.Constructor = typ.GetConstructor([| typeof<int>; typeof<string> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<int>, id); CustomAttributeTypedArgument(typeof<string>, name) |]
            member __.NamedArguments = upcast [| CustomAttributeNamedArgument(typ.GetProperty("MergeContent"), CustomAttributeTypedArgument(typeof<bool>, mergeContent)) |] }
            
    let mkXrdCollection (itemName, isNullable) =
        let typ = typeof<XRoadCollectionAttribute>
        let ct, ctArgs =
            match itemName with
            | Some(name) -> typ.GetConstructor([| typeof<string> |]), [| CustomAttributeTypedArgument(typeof<string>, name) |]
            | None -> typ.GetConstructor([| |]), [| |]
        let namedArgs = if isNullable then [| CustomAttributeNamedArgument(typ.GetProperty("ItemIsNullable"), CustomAttributeTypedArgument(typeof<bool>, true)) |] else [| |]
        { new CustomAttributeData() with
            member __.Constructor = ct
            member __.ConstructorArguments = upcast ctArgs
            member __.NamedArguments = upcast namedArgs }
            
    let mkXrdDefType (layout: LayoutKind) =
        let typ = typeof<XRoadTypeAttribute>
        { new CustomAttributeData() with
            member __.Constructor = typ.GetConstructor([| typeof<LayoutKind> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<LayoutKind>, layout) |]
            member __.NamedArguments = upcast [| |] }
            
    let mkXrdElement (elementName, elementNamespace, isNullable, mergeContent) =
        let typ = typeof<XRoadElementAttribute>
        let ct, ctArgs =
            match elementName with
            | Some(name) -> typ.GetConstructor([| typeof<string> |]), [| CustomAttributeTypedArgument(typeof<string>, name) |]
            | None -> typ.GetConstructor([| |]), [| |]
        let namedArgs =
            [ yield elementNamespace |> Option.map (fun x -> CustomAttributeNamedArgument(typ.GetProperty("Namespace"), CustomAttributeTypedArgument(typeof<string>, x)))
              yield if isNullable then Some(CustomAttributeNamedArgument(typ.GetProperty("IsNullable"), CustomAttributeTypedArgument(typeof<bool>, true))) else None
              yield if mergeContent then Some(CustomAttributeNamedArgument(typ.GetProperty("MergeContent"), CustomAttributeTypedArgument(typeof<bool>, true))) else None ]
            |> List.choose id
            |> List.toArray
        { new CustomAttributeData() with
            member __.Constructor = ct
            member __.ConstructorArguments = upcast ctArgs
            member __.NamedArguments = upcast namedArgs }
    
    let mkXmlIgnore () =
        let typ = typeof<XmlIgnoreAttribute>
        { new CustomAttributeData() with
            member __.Constructor = typ.GetConstructor([| |])
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| |] }
    
    let mkXrdContent () =
            let typ = typeof<XRoadElementAttribute>
            let nsprop = typ.GetProperty("MergeContent")
            { new CustomAttributeData() with
                member __.Constructor = typ.GetConstructor([| |])
                member __.ConstructorArguments = upcast [| |]
                member __.NamedArguments = upcast [| CustomAttributeNamedArgument(nsprop, CustomAttributeTypedArgument(typeof<bool>, true)) |] }

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

    let DebuggerBrowsable = Attr.create<DebuggerBrowsableAttribute> |> Attr.addArg (Expr.enumValue<DebuggerBrowsableState> "Never")

    let xrdRoot = Attr.create<XRoadTypeAttribute> |> Attr.addArg (Expr.typeRefOf<LayoutKind> @=> (LayoutKind.Sequence.ToString()))
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
let createProperty<'T> name doc (ctxt: ProvidedTypesContext) (owner: ProvidedTypeDefinition) =
    let backingField = ProvidedField(sprintf "%s__backing" name, typeof<'T>)
    // Attributes.DebuggerBrowsable

    let property = ProvidedProperty(name, typeof<'T>, getterCode = (fun args -> Expr.FieldGet(args.[0], backingField)), setterCode = (fun args -> Expr.FieldSet(args.[0], backingField, args.[1])))
    property.AddXmlDoc(doc)
    // MemberAttributes.Public ||| MemberAttributes.Final
    
    owner.AddMember(backingField)
    owner.AddMember(property)
    owner

/// Add property to given type with backing field.
/// For optional members, extra field is added to notify if property was assigned or not.
let addProperty (name : string, ty: RuntimeType, isOptional) (owner: ProvidedTypeDefinition) =
    let fixedName = name.ToPropertyName()
    
    let typ = ty.AsCodeTypeReference(optional = isOptional)

    let f = ProvidedField((sprintf "%s__backing" fixedName), typ)
    f.SetFieldAttributes(FieldAttributes.Private)
    // f.AddCustomAttribute(DebuggerBrowsable)
    owner.AddMember(f)

    let p = ProvidedProperty(fixedName, typ, getterCode = (fun args -> Expr.FieldGet(args.[0], f)), setterCode = (fun args -> Expr.FieldSet(args.[0], f, args.[1])))
    // MemberAttributes.Public ||| MemberAttributes.Final
    owner.AddMember(p)

    p

let addContentProperty (name: string, ty: RuntimeType) (owner: ProvidedTypeDefinition) =
    let name = name.ToPropertyName()
    
    let f = ProvidedField(sprintf "%s__backing" name, ty.AsCodeTypeReference(true))
    f.SetFieldAttributes(FieldAttributes.InitOnly ||| FieldAttributes.Private)
    
    let p = ProvidedProperty(name, ty.AsCodeTypeReference(true), getterCode = (fun args -> Expr.FieldGet(args.[0], f)))
    p.AddCustomAttribute(Attributes.mkXrdContent())
     
    let ct = ProvidedConstructor([ProvidedParameter("value", ty.AsCodeTypeReference())], invokeCode = (fun args -> Expr.FieldSet(f, args.[0])))

    owner.AddMember(f)
    owner.AddMember(p)
    owner.AddMember(ct)
    
    owner
