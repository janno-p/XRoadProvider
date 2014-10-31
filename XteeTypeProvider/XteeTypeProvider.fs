namespace XteeTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Reflection
open System.Xml
open XteeTypeProvider.Wsdl
open XteeTypeProvider.Xtee

[<TypeProvider>]
type public XteeTypeProvider() as this =
    inherit TypeProviderForNamespaces()

    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "XteeTypeProvider"
    let baseType = Some typeof<obj>
    let staticParams = [ProvidedStaticParameter("uri", typeof<string>)]

    let newType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XteeTypeProvider", baseType)

    do newType.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = (fun typeName parameterValues ->
            let thisType = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, baseType)
            try
                match parameterValues with
                | [| :? string as uri |] ->
                    let description = uri |> Resolve |> ReadDescription

                    let (|SoapAddress|_|) (e: obj) =
                        match e with
                        | :? System.Web.Services.Description.SoapAddressBinding as addr -> Some addr.Location
                        | _ -> None
                    
                    let (|Producer|_|) (e: obj) =
                        match e with
                        | :? System.Xml.XmlElement as el ->
                            match el.LocalName, el.NamespaceURI with
                            | "address", "http://x-tee.riik.ee/xsd/xtee.xsd"
                            | "address", "http://x-road.ee/xsd/x-road.xsd" ->
                                match [for a in el.Attributes -> a] |> Seq.tryFind (fun a -> a.LocalName = "producer") with
                                | Some a -> Some a.Value
                                | _ -> None
                            | _ -> None
                        | _ -> None

                    let (|XrdTitle|_|) (e: obj) =
                        match e with
                        | :? System.Xml.XmlElement as el ->
                            match el.LocalName, el.NamespaceURI with
                            | "title", "http://x-tee.riik.ee/xsd/xtee.xsd"
                            | "title", "http://x-road.ee/xsd/x-road.xsd" ->
                                match [for a in el.Attributes -> a] |> Seq.tryFind (fun a -> a.LocalName = "lang" && a.NamespaceURI = "http://www.w3.org/XML/1998/namespace") with
                                | Some a -> Some (a.Value, el.InnerText)
                                | _ -> Some ("et", el.InnerText)
                            | _ -> None
                        | _ -> None

                    for service in description.Services do
                        let serviceType = ProvidedTypeDefinition(service.Name, baseType, HideObjectMethods=true)
                        for port in service.Ports do
                            let portType = ProvidedTypeDefinition(port.Name, baseType, HideObjectMethods=true)
                            for ext in port.Extensions do
                                match ext with
                                | SoapAddress addr ->
                                    portType.AddMember(ProvidedLiteralField("address", typeof<string>, addr))
                                | Producer producer ->
                                    portType.AddMember(ProvidedLiteralField("producer", typeof<string>, producer))
                                | XrdTitle ("et", value) ->
                                    portType.AddXmlDoc(value)
                                | _ -> ()
                            serviceType.AddMember portType
                        thisType.AddMember serviceType

                    (*
                    description.Types.Schemas |> Seq.iter (fun schema ->
                        let target = ProvidedTypeDefinition(schema.TargetNamespace, baseType, HideObjectMethods = true)

                        [ for name in schema.SchemaTypes.Names -> name :?> XmlQualifiedName ] |> List.iter (fun name ->
                            ProvidedTypeDefinition(name.Name, baseType, HideObjectMethods = true) |> target.AddMember
                        )

                        target |> thisType.AddMember
                    )

                    for binding in description.Bindings do
                        for operation in binding.Operations do
                            let version = "0" //GetOperationVersion operation
                            let m = ProvidedMethod(methodName = operation.Name,
                                                   parameters = [],
                                                   returnType = typeof<unit>,
                                                   IsStaticMethod = true,
                                                   InvokeCode = (fun args -> <@@ () @@>))
                            m.AddXmlDoc(version)
                            m |> thisType.AddMember
                    *)
                | _ -> failwith "unexpected parameter values"
            with
            | e ->
                let msg = e.ToString()
                let noteProperty = ProvidedProperty("<Note>", typeof<string>, IsStatic=true)
                noteProperty.GetterCode <- (fun args -> <@@ msg @@>)
                noteProperty.AddXmlDoc(msg)
                thisType.AddMember noteProperty
            thisType))

    do this.AddNamespace(rootNamespace, [newType])

[<TypeProviderAssembly>]
do ()
