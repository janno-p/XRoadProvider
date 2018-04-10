#I __SOURCE_DIRECTORY__

#r "NodaTime"
#r "Optional"
#r "System.Xml.Linq"
#r "XRoadProvider"

let (!?) = System.Nullable<'T>
let (?=) (o: 'T option) =
    match o with
    | Some(v) -> Optional.Option.Some<'T>(v)
    | None -> Optional.Option.None<'T>() 
