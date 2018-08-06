module internal XRoad.CodeDom

open ProviderImplementation.ProvidedTypes
open System
open System.Globalization
open TypeSchema

module Attributes =
    open XRoad.Serialization.Attributes
    open System.Reflection
    open System.Xml.Linq

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
