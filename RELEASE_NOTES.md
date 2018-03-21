#### 1.0.2 - 21.03.2018

* Parse type schemas which provide only imported schemas, but no types on their own.
* Common interfaces for Choice types with 1-8 alternative options.

#### 1.0.1 - 13.03.2018

* Netstandard doesn't work..
* Downgrade FSharp.Core version

#### 1.0.0 - 13.03.2018

* Use F# 3.1 for .NET 4.0 framework version.
* Add operation filter static parameter to producer provider.
* Netstandard 2.0 support.

#### 1.0.0-alpha018 - 14.12.2017

* Experimental .NET 4.0 support added.

#### 1.0.0-alpha017 - 12.12.2017

* Fix `Content-Type` header in MTOM requests.

#### 1.0.0-alpha016 - 08.12.2017

* Support X-Road security server authentication certificates.
* Remove `ProducerName` property from v6 producer description, since it doesn't exist.
* Rename `ProducerUri` property to `Uri` in port types.
* Overloaded constructors for port types.
* Change `Uri` to read-only (assignment in overloaded constructor).

#### 1.0.0-alpha015 - 07.12.2017

* Can handle self-signed certificates.

#### 1.0.0-alpha014 - 06.12.2017

* Certificate fixes.

#### 1.0.0-alpha013 - 06.12.2017

* Fix deserialization of merged array content.
* Fix serialization of merged array content.
* Add https support for security server provider.
* Add https support for generated producer provider service calls.

#### 1.0.0-alpha012 - 04.12.2017

* Clean property names of symbols which are not allowed in identifier.
* Fix serialization of binary content without explicit ID.
* Ignore missing content attachments.

#### 1.0.0-alpha011 - 01.12.2017

* Handle xop binary content type elements.
* Fix multipart message deserialization.

#### 1.0.0-alpha010 - 30.11.2017

* Fix deserialization of empty elements.
* Add constant `__TargetNamespace__` to `DefinedTypes` type collections which holds corresponding namespace in which these types are defined.

#### 1.0.0-alpha009 - 30.11.2017

* Fix object reference exception in X-Road v6 security server provider.
* Add predefined identifiers for security server, member and subsystem identifiers.
* Add bootstrapper script to minimize scripting rituals.
* Update project site index.

#### 1.0.0-alpha008 - 29.11.2017

* Repackage

#### 1.0.0-alpha007 - 29.11.2017

* Repackage

#### 1.0.0-alpha006 - 29.11.2017

* Repackage

#### 1.0.0-alpha005 - 28.11.2017

* Custom serialization for better type support.
* X-Road message protocol version 4.0 support.

#### 0.0.4-alpha - August 19 2015

* Improved handling of binary content.
* Added default request uri to security server provider.
* Fix #13: Mono too can handle binary content.
* Add support for included schemas.
* Fix relative Uri-s for imported schemas.

#### 0.0.3-alpha - June 15 2015

* Restore MIT license.
* Improve documentation on home page.
* Automatically add framework assembly dependencies (System.Xml).
* Add partial support for unallowed binding styles Document/Encoded and RPC/Literal.
* Populate documentation tooltips from data extracted from service description.
* Fix deserialization bug when attachment marker gets splitted.
* Fix order of elements in sequence particle.
* Coding tooltips for types, methods and properties.

#### 0.0.2-alpha - June 2 2015

* Fix unnecessary dependency for NuGet package.

#### 0.0.1-alpha - May 15 2015

* Initial prerelease of XRoadProvider.
* Basic support for rpc/encoded style (X-Road protocol 2.0) producer definitions.
* Basic support for document/literal style (X-Road protocol 3.1) producer definitions.
* Support for binary content transferred with MIME/multipart containers.
* Handle non-technical faults that are not defined in producer definition with configuration option `undescribedFaults`.
