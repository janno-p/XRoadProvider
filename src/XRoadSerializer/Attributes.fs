namespace XRoad.Attributes

open System

type LayoutKind =
    | All = 0
    | Choice = 1
    | Sequence = 2

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Class)>]
type XRoadTypeAttribute(name: string, layout: LayoutKind) =
    inherit Attribute()
    new(layout) = XRoadTypeAttribute("", layout)
    member val Layout = layout with get
    member val Name = name with get
    member val Namespace = "" with get, set

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Property)>]
type XRoadElementAttribute(name: string) =
    inherit Attribute()
    new() = XRoadElementAttribute("")
    member val IsNullable = false with get, set
    member val Name = name with get

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Property)>]
type XRoadContentAttribute() =
    inherit Attribute()

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Class, AllowMultiple=true)>]
type XRoadChoiceOptionAttribute(id: int, name: string, isElement: bool) =
    inherit Attribute()
    member val Id = id with get
    member val Name = name with get
    member val IsElement = isElement with get
