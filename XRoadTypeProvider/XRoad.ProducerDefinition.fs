module internal XRoad.ProducerDefinition

open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open System
open System.Xml
open System.Xml.Linq
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Wsdl.XsdSchema

let getConstructor (typ: Type) =
    typ.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance)
    |> Array.find (fun c -> c :? ProvidedConstructor) :?> ProvidedConstructor

let getProducerDefinition(uri, theAssembly, namespacePrefix) =
    let schema = resolveUri uri |> readSchema
    let typeCache = Dictionary<XName,ProvidedTypeDefinition>()
    let namespaceCache = Dictionary<XNamespace,ProvidedTypeDefinition>()

    let serviceTypesTy = ProvidedTypeDefinition("ServiceTypes", Some typeof<obj>, IsErased=false)

    let getOrCreateNamespace (name: XNamespace) =
        match namespaceCache.TryGetValue(name) with
        | true, typ -> typ
        | _ ->
            let producerName = 
                match Regex.Match(name.NamespaceName, @"^http://producers\.\w+\.xtee\.riik\.ee/producer/(\w+)$") with
                | m when m.Success -> m.Groups.[1].Value
                | _ -> failwithf "TODO: Implement normal namespace handling for tns: %A" name
            let typ = ProvidedTypeDefinition(producerName, Some typeof<obj>, IsErased=false)
            serviceTypesTy.AddMember(typ)
            namespaceCache.Add(name, typ)
            typ

    let getOrCreateType (name: XName) =
        match typeCache.TryGetValue(name) with
        | true, typ -> typ
        | _ ->
            let typ = ProvidedTypeDefinition(name.LocalName, Some typeof<obj>, IsErased=false)
            typ.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
            let ctor = ProvidedConstructor([], InvokeCode=(fun _ -> <@@ () @@>))
            typ.AddMember(ctor)
            let serializeMeth = ProvidedMethod("Serialize", [ProvidedParameter("writer", typeof<XmlWriter>)], typeof<System.Void>)
            serializeMeth.SetMethodAttrs(MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.VtableLayoutMask)
            serializeMeth.InvokeCode <- fun _ -> <@@ () @@>
            typ.AddMember(serializeMeth)
            let namespaceTy = getOrCreateNamespace (name.Namespace)
            namespaceTy.AddMember(typ)
            typeCache.Add(name, typ)
            typ

    // Step 1: Populate all global types from schema
    schema.TypeSchemas |> List.iter (fun typeSchema ->
        typeSchema.Types |> Seq.iter (fun kvp ->
            let providedTy = getOrCreateType(kvp.Key)
            match kvp.Value with
            | SimpleType(SimpleTypeSpec.Restriction(spec)) ->
                ()
            | ComplexType(spec) ->
                let ctor = getConstructor(providedTy)
                if spec.IsAbstract then
                    providedTy.SetAttributes(TypeAttributes.Abstract ||| TypeAttributes.Public ||| TypeAttributes.Class)
                    ctor.SetConstructorAttrs(MethodAttributes.Family ||| MethodAttributes.RTSpecialName)
                match spec.Content with
                | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                    ()
                | SimpleContent(SimpleContentSpec.Restriction(spec)) ->
                    ()
                | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                    let baseTy = getOrCreateType(spec.Base)
                    providedTy.SetBaseType(baseTy)
                    let baseCtor = getConstructor(baseTy)
                    ctor.BaseConstructorCall <- (fun args -> baseCtor :> ConstructorInfo, args)
                    let serializeMeth = providedTy.GetMethod("Serialize") :?> ProvidedMethod
                    serializeMeth.SetMethodAttrs(MethodAttributes.Public ||| MethodAttributes.Virtual)
                | ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                    ()
                | ComplexTypeContent.Particle(spec) ->
                    ()
            ))

    [serviceTypesTy]
