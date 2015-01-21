# XRoadTypeProvider

A F# type provider library to support easy interfacing with service providers over [X-Road](http://x-road.eu)
infrastructure.

## TODO

* Overrideable service settings (producer, soap address, doc language)
* Logging outgoing incoming messages when necessary.

## Mistakes in XRoad samples

### Maakataster (v4)

* Operation binding response for `uploadMime` service binds to `mimeResponse` message part `p1`
  through portType operation `uploadMime`, but that message has no part with that name. Part `p2`
  should be used instead.
