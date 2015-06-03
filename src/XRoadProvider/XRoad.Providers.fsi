/// Implementations for the type providers that are available for X-Road infrastructure.
module XRoad.Providers.Impl

/// Generated type provider for X-Road infrastructure.
/// Builds service interface for certain producer.
type XRoadProducerProvider =
    interface Microsoft.FSharp.Core.CompilerServices.ITypeProvider
    interface Microsoft.FSharp.Core.CompilerServices.IProvidedNamespace
    /// Initializes new type provider instance.
    new: unit -> XRoadProducerProvider

/// Erased type provider for acquiring X-Road producers from security server.
type XRoadProviders =
    inherit ProviderImplementation.ProvidedTypes.TypeProviderForNamespaces
    /// Initializes new type provider instance
    new: unit -> XRoadProviders
