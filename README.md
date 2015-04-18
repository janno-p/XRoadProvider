# XRoadProvider

A collection of F# type provider library to support easy interfacing with service providers over
[X-Road](http://x-road.eu) infrastructure.

## Usage

```F#
open XRoad.Providers

type Maakataster = XRoadProducer<"H:\\Work\\Maakataster.wsdl">

let myport = new Maakataster.myservice.myport()
myport.Address <- "http://localhost/maakataster/adapter.asmx"
myport.Producer <- "maakataster"

let paring = new Maakataster.DefinedTypes.maakataster.ky_paring()
paring.katastritunnus <- "test"

let vastus, _ = myport.ky(paring)
```

## TODO

* Overrideable service settings (producer, soap address, doc language).
* Logging outgoing incoming messages when necessary.
* Cleaner type hierarchy considering XRoad specific message structure.
* Separate types for XRoad specific structures: header, non-technical fault etc.
* Generate special type for return types instead of tuples.

## Mistakes in XRoad samples

### Maakataster (v4)

* Operation binding response for `uploadMime` service binds to `mimeResponse` message part `p1`
  through portType operation `uploadMime`, but that message has no part with that name. Part `p2`
  should be used instead.
