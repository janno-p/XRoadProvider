﻿namespace XRoad.Serialization.Attributes

open System

/// Specifies content layout for a type. Controls how element properties are
/// handled in deserialization.
type LayoutKind =
    /// Corresponds to `xs:all` element which doesn't force particular order for elements.
    | All = 0
    /// Corresponds to `xs:choice` element which allows alternative contents.
    | Choice = 1
    /// Corresponds to `xs:sequence` element which forces certain element order.
    | Sequence = 2


/// Attribute which identifies serializable type.
/// Provides overrides for content layout, type name and namespace.
/// Default constructor initializes new attribute by giving type name and content layout.
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Class)>]
type XRoadTypeAttribute(name: string, layout: LayoutKind) =
    inherit Attribute()

    /// Initializes new attribute with sequential layout.
    new() = XRoadTypeAttribute("", LayoutKind.Sequence)

    /// Initializes new attribute by givin content layout value.
    /// Runtime type name is used as type name in serialization.
    new(layout) = XRoadTypeAttribute("", layout)

    /// Content layout for the type.
    member val Layout = layout with get

    /// Name of type in serialization context. If not present the runtime type
    /// name is used instead.
    member val Name = name with get

    /// Namespace of the type in serialization context. Empty (unqualified) by default.
    member val Namespace = "" with get, set


/// Attribute which identifies serializable property.
/// Provides overrides for property serialization.
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Property)>]
type XRoadElementAttribute(name: string) =
    inherit Attribute()

    /// Initializes new attribute. Property name is used as element name in serialization.
    new() = XRoadElementAttribute("")

    /// Specifies if element is allowed to contain `null` values.
    member val IsNullable = false with get, set

    /// Name of the element in serialization context. By default property name is used.
    member val Name = name with get

    /// Namespace of the element in serialization context. By default empty namespace is used.
    member val Namespace = "" with get, set

    /// When true, no extra element is serialized for this property. Instead, property
    /// contents become direct child elements of property owner element.
    member val MergeContent = false with get, set

    /// Applicable for binary properties. When true, given property is serialized using
    /// MTOM+XOP protocol.
    member val UseXop = false with get, set


/// Identifies each individual option for choice type element.
/// Each option should have its unique id and display name.
/// Initializes new attribute with unique id for current choice option
/// and name which is used for type members.
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Class, AllowMultiple = true)>]
type XRoadChoiceOptionAttribute(id: int, name: string) =
    inherit Attribute()

    /// Unique id for this attribute.
    member val Id = id with get

    /// Name which is used for members which are tied to this choice option.
    member val Name = name with get

    /// When true, no extra element is serialized for this option. Instead, type
    /// contents become direct child elements of owner element.
    member val MergeContent = false with get, set


/// Provides serialization option for various collection types.
/// Initializes new attribute with item element name.
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Property)>]
type XRoadCollectionAttribute(itemName: string) =
    inherit Attribute()

    /// Initializes new attribute with no item element name.
    new() = XRoadCollectionAttribute("")

    /// Item element name for particular collection element.
    member val ItemName = itemName with get

    /// Item element namespace for particular collection element.
    member val ItemNamespace = "" with get, set

    /// Specifies if collection elements item element is allowed to contain `null` values.
    member val ItemIsNullable = false with get, set

    /// When true, no extra element is serialized for this property. Instead, all collection
    /// item elements become direct child elements of property owner element.
    member val MergeContent = false with get, set
