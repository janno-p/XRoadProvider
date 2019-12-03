namespace XRoad.Serialization.Attributes

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


type XRoadProtocol =
    | Undefined = 0
    | Version20 = 1
    | Version30 = 2
    | Version31Ee = 3
    | Version31Eu = 4
    | Version40 = 5


type TypeHint =
    | None = 0
    | AnyUri = 1
    | Boolean = 2
    | Date = 3
    | DateTime = 4
    | Decimal = 5
    | Double = 6
    | Duration = 7
    | Float = 8
    | Int = 9
    | Integer = 10
    | Long = 11
    | String = 12
    | Id = 13
    | NmToken = 14
    | Token = 15
    | SwaRef = 16
    | Xop = 17


/// Attribute which identifies serializable type.
/// Provides overrides for content layout, type name and namespace.
/// Default constructor initializes new attribute by giving type name and content layout.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Class)>]
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

    member val IsAnonymous = false with get, set


/// Attribute which identifies serializable property.
/// Provides overrides for property serialization.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property ||| AttributeTargets.Parameter, AllowMultiple = true)>]
type XRoadElementAttribute(id: int, name: string) =
    inherit Attribute()

    /// Initializes new attribute. Property name is used as element name in serialization.
    new() = XRoadElementAttribute(-1, "")
    
    new(id) = XRoadElementAttribute(id, "")

    new(name) = XRoadElementAttribute(-1, name)

    /// Unique id for this attribute.
    member val Id = id with get

    /// Specifies if element is allowed to contain `null` values.
    member val IsNullable = false with get, set

    /// Name of the element in serialization context. By default property name is used.
    member val Name = name with get

    /// Namespace of the element in serialization context. By default empty namespace is used.
    member val Namespace = "" with get, set

    /// When true, no extra element is serialized for this property. Instead, property
    /// contents become direct child elements of property owner element.
    member val MergeContent = false with get, set

    /// Provides additional serialization context to given property.
    member val TypeHint = TypeHint.None with get, set


/// Provides serialization option for various collection types.
/// Initializes new attribute with item element name.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property ||| AttributeTargets.Parameter, AllowMultiple = true)>]
type XRoadCollectionAttribute(id: int, itemName: string) =
    inherit Attribute()

    /// Initializes new attribute with no item element name.
    new() = XRoadCollectionAttribute(-1, "")

    new(id) = XRoadCollectionAttribute(id, "")

    new(itemName) = XRoadCollectionAttribute(-1, itemName)

    /// Unique id for this attribute.
    member val Id = id with get

    /// Item element name for particular collection element.
    member val ItemName = itemName with get

    /// Item element namespace for particular collection element.
    member val ItemNamespace = "" with get, set

    /// Specifies if collection elements item element is allowed to contain `null` values.
    member val ItemIsNullable = false with get, set

    /// When true, no extra element is serialized for this property. Instead, all collection
    /// item elements become direct child elements of property owner element.
    member val MergeContent = false with get, set


/// Describes required header elements of specified X-Road operation.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
type XRoadRequiredHeadersAttribute(ns: string, [<ParamArray>] names: string []) =
    inherit Attribute()

    /// XML namespace of listed header names.
    member val Namespace = ns with get

    /// List of required header names.
    member val Names = names with get


/// Metadata of X-Road operation.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Method)>]
type XRoadOperationAttribute(serviceCode: string, serviceVersion: string, protocol: XRoadProtocol) =
    inherit Attribute()
    
    member val ServiceCode = serviceCode with get
    member val ServiceVersion = serviceVersion with get
    member val Protocol = protocol with get
    member val ProtocolVersion = Unchecked.defaultof<string> with get, set


/// Describes accessor element for X-Road operation request.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Method)>]
type XRoadRequestAttribute(name: string, ns: string) =
    inherit Attribute()
    
    member val Name = name with get
    member val Namespace = ns with get
    member val Encoded = false with get, set
    member val Multipart = false with get, set


/// Describes accessor element for X-Road operation response.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Method)>]
type XRoadResponseAttribute(name: string, ns: string) =
    inherit Attribute()
    
    member val Name = name with get
    member val Namespace = ns with get
    member val Encoded = false with get, set
    member val Multipart = false with get, set
    member val ReturnType = Unchecked.defaultof<Type> with get, set
