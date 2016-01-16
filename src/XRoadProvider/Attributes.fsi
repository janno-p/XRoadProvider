/// Attributes to control serialization of generated types.
namespace XRoad.Serialization.Attributes


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
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Class)>]
type XRoadTypeAttribute =
    inherit System.Attribute

    /// Initializes new attribute by giving type name and content layout.
    new: string * LayoutKind -> XRoadTypeAttribute

    /// Initializes new attribute by givin content layout value.
    /// Runtime type name is used as type name in serialization.
    new: LayoutKind -> XRoadTypeAttribute

    /// Content layout for the type.
    member Layout: LayoutKind with get

    /// Name of type in serialization context. If not present the runtime type
    /// name is used instead.
    member Name: string with get

    /// Namespace of the type in serialization context. Empty (unqualified) by default.
    member Namespace: string with get, set


/// Attribute which identifies serializable property.
/// Provides overrides for property serialization.
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Property)>]
type XRoadElementAttribute =
    inherit System.Attribute

    /// Initializes new attribute by giving element name for serialization.
    new: string -> XRoadElementAttribute

    /// Initializes new attribute. Property name is used as element name in serialization.
    new: unit -> XRoadElementAttribute

    /// Specifies if element is allowed to contain `null` values.
    member IsNullable: bool with get, set

    /// Name of the element in serialization context. By default property name is used.
    member Name: string with get

    /// When true, no extra element is serialized for this property. Instead, property
    /// contents become direct child elements of property owner element.
    member MergeContent: bool with get, set

    /// Applicable for binary properties. When true, given property is serialized using
    /// MTOM+XOP protocol.
    member UseXop: bool with get, set

    /// Specifies type name for given element in serialization context. If not present,
    /// then type name is detected by serializer.
    member TypeName: string with get, set

    /// Specifies type namespace for given element in serialization context. If not present,
    /// then type namespace is detected by serializer.
    member TypeNamespace: string with get, set


/// Identifies each individual option for choice type element.
/// Each option should have its unique id and display name.
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Class, AllowMultiple = true)>]
type XRoadChoiceOptionAttribute =
    inherit System.Attribute

    /// Initializes new attribute with unique id for current choice option
    /// and name which is used for type members.
    new: int * string -> XRoadChoiceOptionAttribute

    /// Unique id for this attribute.
    member Id: int with get

    /// Name which is used for members which are tied to this choice option.
    member Name: string with get

    /// When true, no extra element is serialized for this option. Instead, type
    /// contents become direct child elements of owner element.
    member MergeContent: bool with get, set


/// Provides serialization option for various collection types.
[<AllowNullLiteral>]
[<System.AttributeUsage(System.AttributeTargets.Property)>]
type XRoadCollectionAttribute =
    inherit System.Attribute

    /// Initializes new attribute with item element name.
    new: string -> XRoadCollectionAttribute

    /// Initializes new attribute with no item element name.
    new: unit -> XRoadCollectionAttribute

    /// Item element name for particular collection element.
    member ItemName: string with get

    /// Specifies if collection elements item element is allowed to contain `null` values.
    member ItemIsNullable: bool with get, set

    /// When true, no extra element is serialized for this property. Instead, all collection
    /// item elements become direct child elements of property owner element.
    member MergeContent: bool with get, set
