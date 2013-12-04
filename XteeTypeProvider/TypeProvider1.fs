// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Samples.FSharp.XteeTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes

[<TypeProvider>]
type public TypeProvider1() as this =
    inherit TypeProviderForNamespaces()
    
    // Get the assembly and namespace used to house the provided types
    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "Samples.ShareInfo.TPTest"
    let baseTy = typeof<obj>

    let newT = ProvidedTypeDefinition(thisAssembly, rootNamespace, "TPTestType", Some baseTy)
    
    // add other property and method definition
    let m = ProvidedMethod(
                methodName = "methodName",
                parameters = [],
                returnType = typeof<int>,
                IsStaticMethod = false,
                InvokeCode = fun args -> 
                    <@@ 1 + 1 @@>
                )

    let ctor = ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ (* base class initialization or null*) () @@>) 

    let prop2 = ProvidedProperty(propertyName = "Property1", 
                                 propertyType = typeof<string>, 
                                 IsStatic=false,
                                 GetterCode= (fun args -> <@@ "Hello!" @@>),
                                 SetterCode = (fun args -> <@@ printfn "setter code" @@>))
   

    do prop2.AddXmlDocDelayed(fun () -> "xml comment")    
    do 
        newT.AddMember(m)
        newT.AddMember(prop2)
        newT.AddMember(ctor)
    
    do this.AddNamespace(rootNamespace, [newT])

[<TypeProviderAssembly>]
do ()