namespace XteeTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Reflection
open System.Xml
open XteeTypeProvider.Wsdl

[<TypeProvider>]
type public XteeTypeProvider() as this =
    inherit TypeProviderForNamespaces()

    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "XteeTypeProvider"
    let baseType = typeof<obj>
    let staticParams = [ProvidedStaticParameter("uri", typeof<string>)]

    let newType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XteeTypeProvider", Some baseType)

    do newType.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with
            | [| :? string as uri |] ->
                let description = uri |> Resolve |> ReadDescription

                let thisType = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, Some baseType)
                
                description.Types.Schemas |> Seq.iter (fun schema ->
                    let target = ProvidedTypeDefinition(schema.TargetNamespace, Some baseType, HideObjectMethods = true)

                    [ for name in schema.SchemaTypes.Names -> name :?> XmlQualifiedName ] |> List.iter (fun name ->
                        ProvidedTypeDefinition(name.Name, Some baseType, HideObjectMethods = true) |> target.AddMember
                    )

                    target |> thisType.AddMember
                )

                thisType
            | _ -> failwith "unexpected parameter values"))

    do this.AddNamespace(rootNamespace, [newType])

[<TypeProviderAssembly>]
do ()
