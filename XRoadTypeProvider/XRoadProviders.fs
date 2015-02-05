module XRoadProviders

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.IO

[<TypeProvider>]
type XRoadProviders(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let theAssembly = typeof<XRoadProviders>.Assembly
    let namespaceName = "XRoad.Providers"
    let baseTy = typeof<obj>

    let providerTy = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadService", Some baseTy, IsErased=false)
    let providedAssembly = ProvidedAssembly(Path.Combine(Path.GetTempPath(), "XRoadService.dll"))

    do providerTy.DefineStaticParameters(
        [ProvidedStaticParameter("ProducerUri", typeof<string>)],
        fun typeName parameterValues ->
            let thisTy = ProvidedTypeDefinition(theAssembly, namespaceName, typeName, Some baseTy, IsErased=false)
            providedAssembly.AddTypes([thisTy])
            thisTy
        )

    do this.AddNamespace(namespaceName, [providerTy])
