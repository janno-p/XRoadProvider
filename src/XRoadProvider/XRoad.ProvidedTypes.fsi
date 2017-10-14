/// Implementations for the type providers that are available for X-Road infrastructure.
namespace XRoad.ProvidedTypes


/// Generated type provider for X-Road infrastructure.
/// Builds service interface for certain producer.
type XRoadProducerTypeProvider =
    inherit ProviderImplementation.ProvidedTypes.TypeProviderForNamespaces

    /// Initializes new type provider instance.
    new: Microsoft.FSharp.Core.CompilerServices.TypeProviderConfig -> XRoadProducerTypeProvider


/// Erased type provider for acquiring X-Road producers from security server.
type XRoadProviders =
    inherit ProviderImplementation.ProvidedTypes.TypeProviderForNamespaces

    /// Initializes new type provider instance
    new: Microsoft.FSharp.Core.CompilerServices.TypeProviderConfig -> XRoadProviders
