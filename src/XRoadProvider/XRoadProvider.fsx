#I __SOURCE_DIRECTORY__

#r "NodaTime"
#r "Optional"
#r "System.Xml.Linq"
#r "XRoadProvider"

let (!?) =
    System.Nullable<'T>

let (!@) (v: 'T) =
    Optional.Option.Some<'T>(v)

let (!&) (v: 'T) =
    !@ (!? v)
