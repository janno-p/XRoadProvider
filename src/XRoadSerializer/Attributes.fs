namespace XRoad.Attributes

open System

[<AttributeUsage(AttributeTargets.Class)>]
type XRoadTypeAttribute() =
    inherit Attribute()

[<AttributeUsage(AttributeTargets.Property)>]
type XRoadElementAttribute() =
    inherit Attribute()
