module XRoadTypeProvider.Expressions

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open System
open System.Collections.Generic
open System.Reflection
open System.Xml
open System.Xml.Linq
open XRoadTypeProvider.Runtime
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Wsdl.XsdSchema

type TypeCache = Dictionary<XmlReference,ProvidedTypeDefinition>

let andThen e2 e1 = Expr.Sequential(e1, e2)

let execute defExp exps =
    match exps with
    | [] -> defExp
    | exp::[] -> exp
    | exp::exps -> exps |> List.fold (fun exp e -> exp |> andThen e) exp

let requiredXRoadHeaders operation =
    let headers, rest =
        operation.Request.Header
        |> List.partition (fun part ->
            match part with
            | IsXteeHeader _ when operation.Style = RpcEncoded -> true
            | IsXRoadHeader _ when operation.Style = DocLiteral -> true
            | _ -> false)
    if rest.Length > 0 then
        failwithf "Unhandled SOAP Header elements detected: %A" rest
    headers |> List.map (fun part -> part.Name)

let getType id (cache: TypeCache) =
    match cache.TryGetValue(id) with
    | true, tp -> tp
    | _ -> failwithf "Unknown WSDL type: %A" id // typeof<obj>??

let getRuntimeType typeName cache =
    match mapPrimitiveType typeName with
    | Some tp -> tp
    | _ -> cache |> getType (SchemaType(typeName)) :> Type

let buildReturnType typeCache operation =
    let responseTypes = operation.Response.Body
                        |> List.map (fun p -> typeCache |> getType p.Reference :> Type)
    let innerType = match responseTypes with
                    | [] -> typeof<unit>
                    | tp::[] -> tp
                    | many -> many |> Array.ofList |> FSharpType.MakeTupleType
    // Multipart content will be implemented later
    //match operation.Response.MultipartContent with
    //| [] -> innerType
    //| _ -> typedefof<Runtime.IXRoadResponseWithAttachments<_>>.MakeGenericType(innerType)
    innerType

let buildParameters typeCache operation =
  [ yield! operation.Request.Body
           |> List.map (fun p -> ProvidedParameter(p.Name, typeCache |> getType p.Reference))
    yield ProvidedParameter("settings", typeof<XRoadHeader>) ]

let createXRoadOperationMethod typeCache operation =
    let xrdHeaders = requiredXRoadHeaders(operation)
    let parameters = operation |> buildParameters typeCache
    let returnType = operation |> buildReturnType typeCache

    let settingsIndex = 1 + (parameters |> List.findIndex (fun p -> p.ParameterType = typeof<XRoadHeader>))

    let providedMethod = ProvidedMethod(operation.Name.LocalName, parameters, returnType)
    providedMethod.InvokeCode <- fun args ->
        let operationName = operation.Name.LocalName
        let operationNamespace = operation.Name.NamespaceName
        let operationVersion = operation.Version |> Option.orDefault ""
        let writer = Var("writer", typeof<XmlWriter>)
        let f =
            Expr.Lambda(writer,
                operation.Request.Body
                |> List.mapi (fun i part ->
                    let partName = part.Name
                    let index = i
                    let tp = typeCache |> getType part.Reference
                    let entityExpr = Expr.Coerce(Expr.Coerce(args.[index + 1], tp), typeof<XRoadEntity>)
                    let writerExpr = Expr.Var(writer)
                    <@@ (%%writerExpr: XmlWriter).WriteStartElement(partName)
                        (%%entityExpr: XRoadEntity).Serializer(%%writerExpr: XmlWriter)
                        (%%writerExpr: XmlWriter).WriteEndElement() @@>)
                |> execute (Expr.Value(())))
        <@@ XRoadRequest.makeRpcCall((%%args.[0]: XRoadContext) :> IXRoadContext,
                                     operationName,
                                     operationVersion,
                                     operationNamespace,
                                     (%%args.[settingsIndex]: XRoadHeader),
                                     xrdHeaders |> Array.ofList,
                                     (%%f: System.Xml.XmlWriter -> unit)) @@>
    providedMethod

    // ====

    (*
    let tpoox = parameters
                |> List.choose (fun pm -> match pm.ParameterType with
                                            | :? ProvidedTypeDefinition as x -> Some x
                                            | _ -> None)
                |> List.map (fun tp -> tp, )
                |> List.tryFind (fun _ -> true)

    let meth = ProvidedMethod(operation.Name.LocalName, parameters, returnType)
    meth.InvokeCode <- (fun args ->
        let opName, opVer, opNs = (operation.Name.LocalName, operation.Version |> Option.orDefault "", operation.Name.NamespaceName)
        let ps = args |> Seq.ofList |> Seq.skip 1 |> Seq.mapi (fun i exp -> match parameters.[i] with
                                                                            | p when p.ParameterType = typeof<obj> -> Expr.Cast<obj> exp :> Expr
                                                                            | p when p.ParameterType = typeof<XRoadHeader> -> Expr.Coerce(Expr.Cast<XRoadHeader> exp, typeof<obj>)
                                                                            | p ->
                                                                                let pi = typeof<IXRoadEntity>.GetProperty("RootName")
                                                                                Expr.Sequential(
                                                                                    Expr.PropertySet(Expr.Cast<XRoadEntity> exp, pi, Expr.Value(p.Name)),
                                                                                    Expr.Coerce(Expr.Cast<XRoadEntity> exp, typeof<obj>)))
        let pl = Expr.NewArray(typeof<obj>, ps |> Seq.toList)
        match operation.Style with
        | RpcEncoded ->
            let f =
                match tpoox with
                | Some (tp, mi) ->
                    let v = Var("w", typeof<System.Xml.XmlWriter>)
                    Expr.Lambda(v, Expr.Call(Expr.Coerce(args.[1], tp), mi, [Expr.Coerce(Expr.Var(v), typeof<System.Xml.XmlWriter>)]))
                | _ -> <@@ printfn "Nuthin'!" @@>
            <@@ XRoadRequest.makeRpcCall((%%args.[0]: XRoadContext) :> IXRoadContext,
                                            opName,
                                            opVer,
                                            opNs,
                                            %%pl,
                                            xrdHeaders |> Array.ofList,
                                            (%%f: System.Xml.XmlWriter -> unit)) @@>
        | DocLiteral ->
            <@@ XRoadRequest.makeDocumentCall((%%args.[0]: XRoadContext) :> IXRoadContext,
                                                opName,
                                                opVer,
                                                opNs,
                                                %%pl,
                                                xrdHeaders |> Array.ofList) @@>)
    meth
    *)



let getParentType name (cache: TypeCache) =
    match name with
    | SoapEncType "Array" -> typeof<obj[]>
    | _ -> cache |> getRuntimeType name

let serializePropertyExpr (e: ElementSpec) (typ: Type) (entityExpr: Expr) (writerExpr: Expr) =
    let propertyName = e.Name
    let writeValueExpr =
        let matchValue = Var("matchValue", typeof<obj>)
        let varValue = Expr.Var(matchValue)
        let serializeExpr =
            match typ with
            | :? ProvidedTypeDefinition as typ ->
                let testExpr = Expr.Coerce(varValue, typeof<XRoadEntity>)
                let attributeExpr =
                    match e.Type with
                    | RefOrType.Name(xname) ->
                        let nm, ns = xname.LocalName, xname.NamespaceName
                        Expr.IfThenElse(<@@ (%%testExpr: XRoadEntity).TypeName <> (nm, ns) @@>,
                                        <@@ (%%writerExpr: XmlWriter).WriteStartAttribute("type", XmlNamespace.Xsi)
                                            (%%writerExpr: XmlWriter).WriteQualifiedName((%%testExpr: XRoadEntity).TypeName)
                                            (%%writerExpr: XmlWriter).WriteEndAttribute() @@>,
                                        Expr.Value(()))
                        |> Some
                    | _ -> None
                let serializeExpr = <@@ (%%testExpr: XRoadEntity).Serializer(%%writerExpr: XmlWriter) @@>
                attributeExpr |> Option.fold (fun aggExp exp -> exp |> andThen aggExp) serializeExpr
            | tp when tp = typeof<string> ->
                <@@ (%%writerExpr: XmlWriter).WriteValue(unbox<string> %%varValue) @@>
            | tp when tp = typeof<int64> ->
                <@@ (%%writerExpr: XmlWriter).WriteValue(unbox<int64> %%varValue) @@>
            | _ ->
                Expr.Value(())
        Expr.Let(matchValue,
                 <@@ (%%entityExpr: XRoadEntity).GetProperty(propertyName) @@>,
                 Expr.IfThenElse(<@@ %%varValue = null @@>,
                                 <@@ (%%writerExpr: XmlWriter).WriteAttributeString("nil", XmlNamespace.Xsi, "true") @@>,
                                 serializeExpr))
    let writeElementValueExpr =
        <@@ (%%writerExpr: XmlWriter).WriteStartElement(propertyName)
            (%%writeValueExpr)
            (%%writerExpr: XmlWriter).WriteEndElement() @@>
    match e.MinOccurs with
    | 0u -> <@@ match (%%entityExpr: XRoadEntity).HasProperty(propertyName) with
                | true -> %%writeElementValueExpr
                | _ -> () @@>
    | 1u -> writeElementValueExpr
    | _ -> failwith "Not implemented!"

let mGetProperty = typeof<XRoadEntity>.GetMethod("GetProperty")
let mSetProperty = typeof<XRoadEntity>.GetMethod("SetProperty")

let rec addXRoadEntityMembers (providedType: ProvidedTypeDefinition) (name: XName option) typ cache =
    let createNestedType name typeDef =
        let nestedType = ProvidedTypeDefinition(sprintf "%s'" name, Some typeof<XRoadEntity>, HideObjectMethods=true)
        providedType.AddMember(nestedType)
        addXRoadEntityMembers nestedType None typeDef cache
        nestedType

    let makeGenericMethod (tp: Type) (meth: MethodInfo) =
        meth.MakeGenericMethod(match tp with | :? ProvidedTypeDefinition -> typeof<XRoadEntity> | _ -> tp)

    let getRuntimeTypeInfo name (typ: RefOrType) =
        match typ with
        | RefOrType.Ref(name) -> failwith "Not supported"
        | RefOrType.Name(name) -> cache |> getRuntimeType name
        | RefOrType.Type(typ) -> createNestedType name typ :> Type

    let createProperty name (typ: Type) =
        let property = ProvidedProperty(name, typ)
        property.GetterCode <- (fun args -> Expr.Call(<@@ ((%%args.[0]: obj) :?> XRoadEntity) @@>, (mGetProperty |> makeGenericMethod typ), [Expr.Value(name)]))
        property.SetterCode <- (fun args ->
            Expr.Call(<@@ ((%%args.[0]: obj) :?> XRoadEntity) @@>,
                      mSetProperty |> makeGenericMethod typ,
                      [Expr.Value(name); (match typ with
                                          | :? ProvidedTypeDefinition -> <@@ ((%%args.[1]: obj) :?> XRoadEntity) @@>
                                          | _ -> args.[1])]))
        property

    let serializeExp = List<(Expr -> Expr -> Expr)>()

    let parseComplexTypeContentSpec (spec: ComplexTypeContentSpec) =
        match spec.Content with
        | Some(content) ->
            match content with
            | ComplexTypeParticle.All(spec) ->
                spec.Elements
                |> List.iter (fun element ->
                    let typ = getRuntimeTypeInfo element.Name element.Type
                    let property = createProperty element.Name typ
                    providedType.AddMember(property)
                    serializeExp.Add(serializePropertyExpr element typ))
            | ComplexTypeParticle.Sequence(spec) ->
                spec.Content
                |> List.iter (fun c ->
                    match c with
                    | SequenceContent.Element(element) ->
                        let typ = getRuntimeTypeInfo element.Name element.Type
                        let property = createProperty element.Name typ
                        providedType.AddMember(property)
                        serializeExp.Add(serializePropertyExpr element typ)
                    | _ -> failwith "not implemented!")
                ()
        | _ -> ()

    match typ with
    | ComplexType(spec) ->
        match spec.Content with
        | ComplexContent(spec) ->
            match spec with
            | ComplexContentSpec.Extension(spec) ->
                let baseType = cache |> getParentType spec.Base
                providedType.SetBaseType(baseType)
                match baseType with
                | :? ProvidedTypeDefinition ->
                    //let mi = baseType.GetMethod("Serialize")
                    //serializeExp.Add(fun args -> Expr.Call(Expr.Coerce(args.[0], baseType), mi, [args.[1]]))
                    ()
                | _ -> ()
                spec.Content |> parseComplexTypeContentSpec
            | ComplexContentSpec.Restriction(_) ->
                ()
        | SimpleContent(_) ->
            ()
        | ComplexTypeContent.Particle(spec) ->
            spec |> parseComplexTypeContentSpec
    | SimpleType(spec) ->
        match spec with
        | SimpleTypeSpec.Restriction(_) ->
            ()

    let entity = Var("entity", typeof<XRoadEntity>)
    let entityExpr = Expr.Var(entity)
    let writer = Var("writer", typeof<XmlWriter>)
    let writerExpr = Expr.Var(writer)

    let serializerExpr =
        Expr.Lambda(entity,
            Expr.Lambda(writer,
                match serializeExp |> Seq.toList with
                | [] -> Expr.Value(())
                | exp::[] -> exp entityExpr writerExpr
                | exp::exps -> exps |> List.fold (fun acc e -> acc |> andThen (e entityExpr writerExpr)) (exp entityExpr writerExpr)))

    match typ with
    | ComplexType(spec) when spec.IsAbstract -> ()
    | _ -> let ctor = ProvidedConstructor([])
           ctor.InvokeCode <- (fun _ ->
                match name with
                | None -> <@@ box (XRoadEntity()) @@>
                | Some(name) ->
                    let nm, ns = name.LocalName, name.NamespaceName
                    <@@ let xre = XRoadEntity()
                        xre.TypeName <- (nm, ns)
                        xre.Serializer <- (%%serializerExpr: XRoadEntity -> XmlWriter -> unit) xre
                        box xre @@>)
           providedType.AddMember(ctor)
