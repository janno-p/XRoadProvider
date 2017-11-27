module internal XRoad.EmitterDsl

open FSharp.Quotations.Patterns
open System
open System.Reflection
open System.Reflection.Emit

#if PRINT_IL
//let file = System.IO.File.OpenWrite("/home/janno/Desktop/debug.txt")
let file = System.IO.File.OpenWrite(@"C:\Users\Janno\Desktop\debug.txt")
file.SetLength(0L)
let stream = new System.IO.StreamWriter(file, System.Text.Encoding.UTF8)
let nextLabelId =
    let mutable id = 0
    (fun () -> id <- id + 1; id)
let labels = System.Collections.Concurrent.ConcurrentDictionary<Label,int>()
#endif

let (!@) expr =
    match expr with
    | Call(_, mi, _) -> mi
    | PropertyGet(_, pi, _) -> pi.GetGetMethod()
    | _ -> failwithf "Must be method call expression, but was `%A`." expr

let (!!@) expr =
    match expr with
    | NewObject(ci, _) -> ci
    | _ -> failwith "Must be constructor expression"
    
let rec private typeName (typ: Type) =
    if typ |> isNull then "" else
    let name = sprintf "%s%s" (if typ.Namespace |> isNull then "" else typ.Namespace + ".") typ.Name
    if not typ.IsGenericType then name else
    sprintf "%s<%s>" name (String.Join(",", typ.GetGenericArguments() |> Array.map typeName))
    
let private methodName (mi: MethodInfo) =
    let name = sprintf "%s.%s" (typeName mi.DeclaringType) mi.Name
    sprintf "%s(%s)" name (String.Join(",", mi.GetParameters() |> Array.map (fun p -> typeName p.ParameterType)))
    
let private fieldName (fi: FieldInfo) =
    sprintf "%s [%s]" fi.Name (typeName fi.DeclaringType)
    
let private ctorName (ci: ConstructorInfo) =
    sprintf "%s(%s)" (typeName ci.DeclaringType) (String.Join(",", ci.GetParameters() |> Array.map (fun p -> typeName p.ParameterType)))

let inline declareLocal typ (il: ILGenerator) =
    #if PRINT_IL
    let variable = il.DeclareLocal(typ)
    fprintfn stream "%-10s : [%d] (%s)" "@declare" variable.LocalIndex (typeName typ)
    variable
    #else
    il.DeclareLocal(typ)
    #endif

let inline setLabel label (il: ILGenerator) =
    #if PRINT_IL
    let id = labels.GetOrAdd(label, (fun _ -> nextLabelId()))
    fprintfn stream "%-10s : @{%A}" "@label" id
    #endif
    il.MarkLabel(label)
    il

let inline private emit opCode (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%A" opCode
    #endif
    il.Emit(opCode)
    il
    
let inline private emittyp opCode (typ: Type) (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : %s" (opCode.ToString()) (typeName typ)
    #endif
    il.Emit(opCode, typ)
    il
    
let inline private emitint opCode i (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : %s" (opCode.ToString()) (i.ToString())
    #endif
    il.Emit(opCode, int i)
    il

let inline private emitmi opCode (mi: MethodInfo) (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : %s" (opCode.ToString()) (methodName mi)
    #endif
    il.Emit(opCode, mi)
    il
    
let inline private emitfld opCode (fi: FieldInfo) (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : %s" (opCode.ToString()) (fieldName fi)
    #endif
    il.Emit(opCode, fi)
    il

let inline private emitlbl opCode (label: Label) (il: ILGenerator) =
    #if PRINT_IL
    let id = labels.GetOrAdd(label, (fun _ -> nextLabelId()))
    fprintfn stream "%-10s : @{%A}" (opCode.ToString()) id
    #endif
    il.Emit(opCode, label)
    il

let inline private emitstr opCode (value: string) (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : %s" (opCode.ToString()) value
    #endif
    il.Emit(opCode, value)
    il
    
let inline private emitvar opCode (var: LocalBuilder) (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : [%d] (%s)" (opCode.ToString()) var.LocalIndex (typeName var.LocalType)
    #endif
    il.Emit(opCode, var)
    il
    
let inline callCtor (typ: Type) args (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : %s(%s)" (OpCodes.Newobj.ToString()) (typeName typ) (String.Join(",", args |> List.map typeName))
    #endif
    il.Emit(OpCodes.Newobj, typ.GetConstructor(args |> List.toArray))
    il

let inline create (ci: ConstructorInfo) (il: ILGenerator) =
    #if PRINT_IL
    fprintfn stream "%-10s : %s" (OpCodes.Newobj.ToString()) (ctorName ci)
    #endif
    il.Emit(OpCodes.Newobj, ci)
    il

let inline createX expr il = il |> create (!!@ expr) 
let inline defineLabel (il: ILGenerator) = il.DefineLabel()
let inline defineLabel' (il: ILGenerator) = (il.DefineLabel(), il)
let inline declareLocalOf<'T> il = il |> declareLocal(typeof<'T>)
let inline loadArg0 (il: ILGenerator) = il |> emit OpCodes.Ldarg_0
let inline loadArg1 (il: ILGenerator) = il |> emit OpCodes.Ldarg_1
let inline loadArg2 (il: ILGenerator) = il |> emit OpCodes.Ldarg_2
let inline loadArg3 (il: ILGenerator) = il |> emit OpCodes.Ldarg_3
let inline call mi il = il |> emitmi OpCodes.Call mi
let inline callX expr il = il |> emitmi OpCodes.Call (!@ expr)
let inline stringEquals il = il |> callX <@ "" = "" @>
let inline stringFormat2 il = il |> callX <@ String.Format("", "") @>
let inline stringFormat3 il = il |> callX <@ String.Format("", "", "") @>
let inline callVirt mi il = il |> emitmi OpCodes.Callvirt mi
let inline callVirtX expr il = il |> callVirt (!@ expr)
let inline loadString value il = il |> emitstr OpCodes.Ldstr value
let inline noop il = il |> emit OpCodes.Nop
let inline dup il = il |> emit OpCodes.Dup
let inline pop il = il |> emit OpCodes.Pop
let inline throw il = il |> emit OpCodes.Throw
let inline loadNull il = il |> emit OpCodes.Ldnull
let inline equals il = il |> emit OpCodes.Ceq
let inline ret il = il |> emit OpCodes.Ret
let inline gotoF label il = il |> emitlbl OpCodes.Brfalse label
let inline gotoT label il = il |> emitlbl OpCodes.Brtrue label
let inline goto label il = il |> emitlbl OpCodes.Br label
let inline setVar var il = il |> emitvar OpCodes.Stloc var
let inline getVar var il = il |> emitvar OpCodes.Ldloc var
let inline getVarAddr var il = il |> emitvar OpCodes.Ldloca var
let inline getElem typ il = il |> emittyp OpCodes.Ldelem typ
let inline getElemRef il = il |> emit OpCodes.Ldelem_Ref
let inline toBox typ il = il |> emittyp OpCodes.Box typ
let inline fromBox typ il = il |> emittyp OpCodes.Unbox_Any typ
let inline castClass typ il = il |> emittyp OpCodes.Castclass typ
let inline getField fi il = il |> emitfld OpCodes.Ldfld fi
let inline initObj typ il = il |> emittyp OpCodes.Initobj typ
let inline loadInt i il = il |> emitint OpCodes.Ldc_I4 i
let inline loadInt0 il = il |> emit OpCodes.Ldc_I4_0
let inline loadInt1 il = il |> emit OpCodes.Ldc_I4_1
let inline loadInt2 il = il |> emit OpCodes.Ldc_I4_2
let inline add il = il |> emit OpCodes.Add
let inline div il = il |> emit OpCodes.Div
let inline setElem il = il |> emit OpCodes.Stelem_I4
let inline getLen il = il |> emit OpCodes.Ldlen
let inline castInt il = il |> emit OpCodes.Conv_I4
let inline lessThan il = il |> emit OpCodes.Clt
let inline createArrayOf<'T> il = il |> emittyp OpCodes.Newarr typeof<char>

let inline iif cond f (il: ILGenerator) = if cond then f il else il
let inline ifElse cond ftrue ffalse (il: ILGenerator) = if cond then ftrue il else ffalse il
let inline ifSome opt f (il: ILGenerator) = match opt with Some(o) -> f o il | None -> il
let inline ifSomeNone opt fsome fnone (il: ILGenerator) = match opt with Some(o) -> fsome o il | None -> fnone il

let afterLabel f il =
    let label = il |> defineLabel
    il |> setLabel label |> f label 

let beforeLabel f il =
    let label = il |> defineLabel
    il |> f label |> setLabel label

let withLabel f il =
    let label = il |> defineLabel
    il |> f label

let withLabels n f il =
    let labels = [
        for i in 1..n do
            yield il |> defineLabel
    ]
    il |> f labels
    
let useVar (v: Lazy<_>) f il = il |> f (il |> v.Value)

let defineMethod (mi: MethodInfo) f =
    match mi with
    | :? DynamicMethod as dyn -> dyn.GetILGenerator() |> f |> ignore
    | _ -> failwith "Cannot cast to dynamic method."

type Emitter = ILGenerator -> ILGenerator

type EmitBuilder() =
    member this.Bind(v, f) = id
    member this.Return(p) = id
    member this.Zero() = id

type EmitBuilder with
    [<CustomOperation("castclass", MaintainsVariableSpaceUsingBind = true)>]
    member this.CastClass(p: Emitter, t) = p >> castClass t
    [<CustomOperation("ldarg_0", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldarg_0(p: Emitter) = p >> loadArg0
    [<CustomOperation("ldarg_1", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldarg_1(p: Emitter) = p >> loadArg1
    [<CustomOperation("ldarg_2", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldarg_2(p: Emitter) = p >> loadArg2
    [<CustomOperation("ldarg_3", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldarg_3(p: Emitter) = p >> loadArg3
    [<CustomOperation("ldarg_4", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldarg_4(p: Emitter) = p >> emitint OpCodes.Ldarg 4us
    [<CustomOperation("callvirt", MaintainsVariableSpaceUsingBind = true)>]
    member this.Callvirt(p: Emitter, mi) = p >> callVirt mi
    [<CustomOperation("callvirt_expr", MaintainsVariableSpaceUsingBind = true)>]
    member this.Callvirt_expr(p: Emitter, e) = p >> callVirtX e
    [<CustomOperation("ldloc", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldloc(p: Emitter, v) = p >> getVar v
    [<CustomOperation("stloc", MaintainsVariableSpaceUsingBind = true)>]
    member this.Stloc(p: Emitter, v) = p >> setVar v
    [<CustomOperation("ceq", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ceq(p: Emitter) = p >> equals
    [<CustomOperation("ldc_node_type", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldc_i4_xnt(p: Emitter, i: Xml.XmlNodeType) = p >> loadInt i
    [<CustomOperation("ldc_i4", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldc_i4(p: Emitter, i) = p >> loadInt i
    [<CustomOperation("ldc_i4_0", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldc_i4_0(p: Emitter) = p >> loadInt0
    [<CustomOperation("ldc_i4_1", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldc_i4_1(p: Emitter) = p >> loadInt1
    [<CustomOperation("ldc_i4_2", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldc_i4_2(p: Emitter) = p >> loadInt2
    [<CustomOperation("brfalse", MaintainsVariableSpaceUsingBind = true)>]
    member this.Brfalse(p: Emitter, l) = p >> gotoF l
    [<CustomOperation("brtrue", MaintainsVariableSpaceUsingBind = true)>]
    member this.Brtrue(p: Emitter, l) = p >> gotoT l
    [<CustomOperation("br", MaintainsVariableSpaceUsingBind = true)>]
    member this.Br(p: Emitter, l) = p >> goto l
    [<CustomOperation("clt", MaintainsVariableSpaceUsingBind = true)>]
    member this.Clt(p: Emitter) = p >> lessThan
    [<CustomOperation("nop", MaintainsVariableSpaceUsingBind = true)>]
    member this.Nop(p: Emitter) = p >> noop
    [<CustomOperation("unbox", MaintainsVariableSpaceUsingBind = true)>]
    member this.Unbox(p: Emitter, t) = p >> fromBox t
    [<CustomOperation("set_marker", MaintainsVariableSpaceUsingBind = true)>]
    member this.MarkLabel(p: Emitter, l) = p >> setLabel l
    [<CustomOperation("define_label", MaintainsVariableSpaceUsingBind = true)>]
    member this.DefineLabel(p: Emitter, e) = p >> withLabel e
    [<CustomOperation("define_labels", MaintainsVariableSpaceUsingBind = true)>]
    member this.DefineLabels(p: Emitter, n, e) = p >> withLabels n e
    [<CustomOperation("declare_variable", MaintainsVariableSpaceUsingBind = true)>]
    member this.DeclareVariable(p: Emitter, v, f) = p >> useVar v f
    [<CustomOperation("merge", MaintainsVariableSpaceUsingBind = true)>]
    member this.Merge(p: Emitter, e) = p >> e
    [<CustomOperation("call", MaintainsVariableSpaceUsingBind = true)>]
    member this.Call(p: Emitter, mi) = p >> call mi
    [<CustomOperation("call_expr", MaintainsVariableSpaceUsingBind = true)>]
    member this.CallExpr(p: Emitter, e) = p >> callX e
    [<CustomOperation("ldstr", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldstr(p: Emitter, s) = p >> loadString s
    [<CustomOperation("string_equals", MaintainsVariableSpaceUsingBind = true)>]
    member this.StringEquals(p: Emitter) = p >> stringEquals
    [<CustomOperation("add", MaintainsVariableSpaceUsingBind = true)>]
    member this.Add(p: Emitter) = p >> add
    [<CustomOperation("div", MaintainsVariableSpaceUsingBind = true)>]
    member this.Div(p: Emitter) = p >> div
    [<CustomOperation("ret", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ret(p: Emitter) = p >> ret
    [<CustomOperation("pop", MaintainsVariableSpaceUsingBind = true)>]
    member this.Pop(p: Emitter) = p >> pop
    [<CustomOperation("throw", MaintainsVariableSpaceUsingBind = true)>]
    member this.Throw(p: Emitter) = p >> throw
    [<CustomOperation("ldnull", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldnull(p: Emitter) = p >> loadNull
    [<CustomOperation("newobj", MaintainsVariableSpaceUsingBind = true)>]
    member this.Newobj(p: Emitter, ci) = p >> create ci
    [<CustomOperation("dup", MaintainsVariableSpaceUsingBind = true)>]
    member this.Dup(p: Emitter) = p >> dup
    [<CustomOperation("newobj_expr", MaintainsVariableSpaceUsingBind = true)>]
    member this.NewobjExpr(p: Emitter, e) = p >> createX e
    [<CustomOperation("ldfld", MaintainsVariableSpaceUsingBind = true)>]
    member this.Ldfld(p: Emitter, f) = p >> getField f

let emit' = EmitBuilder()

let (|List1|) = function [a] -> (a) | _ -> failwith "invalid list"
let (|List2|) = function [a; b] -> (a, b) | _ -> failwith "invalid list"
let (|List3|) = function [a; b; c] -> (a, b, c) | _ -> failwith "invalid list"
let (|List4|) = function [a; b; c; d] -> (a, b, c, d) | _ -> failwith "invalid list"
