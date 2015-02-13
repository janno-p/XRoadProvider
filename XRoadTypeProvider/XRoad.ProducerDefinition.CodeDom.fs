module internal XRoad.ProducerDefinitionCodeDom

open Microsoft.CSharp
open System
open System.CodeDom.Compiler
open System.IO

let makeProducerType (typeNamePath: string []) producerUri =
    let dllFile = Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> sprintf "%A.dll")
    use csc = new CSharpCodeProvider()
    let parameters = CompilerParameters(OutputAssembly=dllFile, CompilerOptions="/t:library")
    let typeName = typeNamePath.[typeNamePath.Length - 1]
    let typeNamespace = String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1))
    let code = sprintf "namespace %s { public class %s { } }" typeNamespace typeName
    let product = csc.CompileAssemblyFromSource(parameters, [| code |])
    let assembly = product.CompiledAssembly
    assembly.GetType(String.Join(".", typeNamePath))
