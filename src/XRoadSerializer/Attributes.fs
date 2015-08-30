namespace XRoad.Attributes

open System

[<AttributeUsage(AttributeTargets.Class)>]
type XRoadTypeAttribute() =
    inherit Attribute()
