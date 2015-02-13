module internal XRoad.ProducerDefinitionCodeDom

open Microsoft.CSharp
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml.Linq
open XRoadTypeProvider.Wsdl

let private compileAssembly code =
    let fileName = Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> sprintf "%A.dll")
    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(OutputAssembly=fileName, GenerateExecutable=false)
    let compilerResults = codeProvider.CompileAssemblyFromDom(parameters, [| code |])
    printfn "%A" compilerResults.Errors
    compilerResults.CompiledAssembly

let private makeStaticClass(className, attributes) =
    let targetClass = CodeTypeDeclaration(className)
    targetClass.IsClass <- true
    targetClass.TypeAttributes <- attributes
    targetClass.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" Environment.NewLine)) |> ignore
    targetClass.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
    targetClass

let makeProducerType (typeNamePath: string []) producerUri =
    let schema = resolveUri producerUri |> readSchema
    let typeCache = Dictionary<XName,CodeTypeDeclaration>()
    let namespaceCache = Dictionary<XNamespace,CodeTypeDeclaration>()

    let serviceTypesTy = makeStaticClass("DefinedTypes", TypeAttributes.Public)

    let targetClass = makeStaticClass(typeNamePath.[typeNamePath.Length - 1], TypeAttributes.Public)
    targetClass.Members.Add(serviceTypesTy) |> ignore

    schema.Services |> List.iter (fun service ->
        let serviceTy = makeStaticClass(service.Name, TypeAttributes.Public)
        service.Ports |> List.iter (fun port ->
            let portTy = CodeTypeDeclaration(port.Name, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTy.Members.Add(portTy) |> ignore

            match port.Documentation.TryGetValue("et") with
            | true, doc -> portTy.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
            | _ -> ()

            let addressField = CodeMemberField(typeof<string>, "address", InitExpression=CodePrimitiveExpression(port.Address))
            addressField.Attributes <- MemberAttributes.Private
            let addressFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), addressField.Name)
            let addressProperty = CodeMemberProperty(Name="Address", Type=CodeTypeReference(typeof<string>))
            addressProperty.Attributes <- MemberAttributes.Public
            addressProperty.GetStatements.Add(CodeMethodReturnStatement(addressFieldRef)) |> ignore
            addressProperty.SetStatements.Add(CodeAssignStatement(addressFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

            portTy.Members.Add(addressField) |> ignore
            portTy.Members.Add(addressProperty) |> ignore

            let producerField = CodeMemberField(typeof<string>, "producer", InitExpression=CodePrimitiveExpression(port.Producer))
            let producerFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), producerField.Name)
            let producerProperty = CodeMemberProperty(Name="Producer", Type=CodeTypeReference(typeof<string>))
            producerProperty.Attributes <- MemberAttributes.Public
            producerProperty.GetStatements.Add(CodeMethodReturnStatement(producerFieldRef)) |> ignore
            producerProperty.SetStatements.Add(CodeAssignStatement(producerFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

            portTy.Members.Add(producerField) |> ignore
            portTy.Members.Add(producerProperty) |> ignore

            port.Operations |> List.map (fun op -> ()) |> ignore // TODO!!
            )
        targetClass.Members.Add(serviceTy) |> ignore)

    let codeNamespace = CodeNamespace(String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1)))
    codeNamespace.Types.Add(targetClass) |> ignore

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore
    codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore

    compileAssembly(codeCompileUnit).GetType(sprintf "%s.%s" codeNamespace.Name targetClass.Name)
