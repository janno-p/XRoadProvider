module internal XRoad.CodeDom

open System.CodeDom
open System.Collections
open System.Diagnostics

let private typeRef<'T> = CodeTypeReference(typeof<'T>)
let private typeRefExpr<'T> = CodeTypeReferenceExpression(typeRef<'T>)

let private attrArg expr = CodeAttributeArgument(expr)
let private attrDecl<'T> = CodeAttributeDeclaration(typeRef<'T>)

let private enumExpr<'T> valueName = CodePropertyReferenceExpression(typeRefExpr<'T>, valueName)

let inline private add (i: 'TItem) (c: ^TColl when ^TColl :> CollectionBase) =
    (^TColl: (member Add: 'TItem -> int) (c, i)) |> ignore

module internal Attributes =
    let internal debuggerBrowsable() =
        let attribute = attrDecl<DebuggerBrowsableAttribute>
        attribute.Arguments |> add (enumExpr<DebuggerBrowsableState> "Never" |> attrArg)
        attribute

let makeChoiceType() =
    ()
