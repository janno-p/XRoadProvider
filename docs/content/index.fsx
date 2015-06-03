(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
XRoadProvider
======================

Collection of type providers that make easier the integration process for data exchange layer X-Road. Currently
two different type providers are implemented:

* [XRoadProducer](reference/xroad-providers-xroadproducer.html) is generated type provider which offers type
  signatures for types and method call functionality for operations defined in producers service description.

* [XRoadServer](reference/xroad-providers-xroadserver.html) is erased type provider that discovers available
  producer names from specified security server and offers their details inside development environment.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The XRoadProvider library can be <a href="https://nuget.org/packages/XRoadProvider">installed from NuGet</a>:
      <pre>PM> Install-Package XRoadProvider -Pre</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates the use of the XRoadProvider:

*)
// Reference the type provider assembly.
#r "XRoadProvider.dll"

open XRoad.Providers

// Acquire list of producer from security server.
type SecurityServer = XRoadServer<"xxx.xxx.xx.xxx">

// Initialize service interface for specific producer using details from security server.
type Maakataster = XRoadProducer<SecurityServer.Producers.maakataster.WsdlUri>

// Initialize service interface which provides access to operation methods.
let myport = new Maakataster.myservice.myport()

// Override default values acquired from port definition (when necessary).
myport.ProducerName <- "maakataster"
myport.ProducerUri <- "http://localhost:8001/"

// Assign X-Road header values.
myport.Ametniknimi <- "toomas.dumpty"

// Initialize request parameters.
let request = new Maakataster.DefinedTypes.maakataster.ky_paring()
request.katastritunnus <- "test"
request.ky_max <- new Maakataster.DefinedTypes.maakataster.t_ky_max(BaseValue="001")

// Execute service request against specified adapter.
let response, _ = myport.ky(request)

// Display results to console.
response |> Array.iteri (printfn "%d) %A")

(**

As an alternative to asking service descriptions from security server, it's also possible to
exclude security server from development process by using local WSDL definitions instead.

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/XRoadProvider/tree/master/docs/content
  [gh]: https://github.com/fsprojects/XRoadProvider
  [issues]: https://github.com/fsprojects/XRoadProvider/issues
  [readme]: https://github.com/fsprojects/XRoadProvider/blob/master/README.md
  [license]: https://github.com/fsprojects/XRoadProvider/blob/master/LICENSE.txt
*)
