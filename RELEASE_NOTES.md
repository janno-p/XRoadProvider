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
