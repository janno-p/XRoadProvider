module internal XRoad.EmitterDsl

open Quotations.Patterns
open System
open System.Reflection
open System.Reflection.Emit

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

let inline declareLocal typ (il: ILGenerator) =
    il.DeclareLocal(typ)

let inline private setLabel label (il: ILGenerator) =
    il.MarkLabel(label)
    il

let inline private emit opCode (il: ILGenerator) =
    il.Emit(opCode)
    il
    
let inline private emittyp opCode (typ: Type) (il: ILGenerator) =
    il.Emit(opCode, typ)
    il
    
let inline private emitint opCode i (il: ILGenerator) =
    il.Emit(opCode, int i)
    il

let inline private emitmi opCode (mi: MethodInfo) (il: ILGenerator) =
    il.Emit(opCode, mi)
    il
    
let inline private emitfld opCode (fi: FieldInfo) (il: ILGenerator) =
    il.Emit(opCode, fi)
    il

let inline private emitlbl opCode (label: Label) (il: ILGenerator) =
    il.Emit(opCode, label)
    il

let inline private emitstr opCode (value: string) (il: ILGenerator) =
    il.Emit(opCode, value)
    il
    
let inline private emitvar opCode (var: LocalBuilder) (il: ILGenerator) =
    il.Emit(opCode, var)
    il
    
let inline private callCtor (typ: Type) args (il: ILGenerator) =
    il.Emit(OpCodes.Newobj, typ.GetConstructor(args |> List.toArray))
    il

let inline private create (ci: ConstructorInfo) (il: ILGenerator) =
    il.Emit(OpCodes.Newobj, ci)
    il

let inline private emitctor opCode (ci: ConstructorInfo) (il: ILGenerator) =
    il.Emit(opCode, ci)
    il

let inline declareLocalOf<'T> il = il |> declareLocal(typeof<'T>)

let inline private defineLabel (il: ILGenerator) = il.DefineLabel()

let defineMethod (mi: MethodInfo) f =
    match mi with
    | :? DynamicMethod as dyn -> dyn.GetILGenerator() |> f |> ignore
    | _ -> failwith "Cannot cast to dynamic method."

type Emitter = ILGenerator -> ILGenerator

type EmitBuilder() =
    member __.Bind(_, _) = id
    member __.Return(_) = id
    member __.Zero() = id

type EmitBuilder with
    [<CustomOperation("castclass", MaintainsVariableSpaceUsingBind = true)>]
    member __.CastClass(p: Emitter, t) = p >> emittyp OpCodes.Castclass t
    [<CustomOperation("ldarg_0", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg0(p: Emitter) = p >> emit OpCodes.Ldarg_0
    [<CustomOperation("ldarg_1", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg1(p: Emitter) = p >> emit OpCodes.Ldarg_1
    [<CustomOperation("ldarg_2", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg2(p: Emitter) = p >> emit OpCodes.Ldarg_2
    [<CustomOperation("ldarg_3", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg3(p: Emitter) = p >> emit OpCodes.Ldarg_3
    [<CustomOperation("ldarg_4", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg4(p: Emitter) = p >> emitint OpCodes.Ldarg 4us
    [<CustomOperation("callvirt", MaintainsVariableSpaceUsingBind = true)>]
    member __.Callvirt(p: Emitter, mi) = p >> emitmi OpCodes.Callvirt mi
    [<CustomOperation("callvirt_expr", MaintainsVariableSpaceUsingBind = true)>]
    member __.CallvirtExpr(p: Emitter, e) = p >> emitmi OpCodes.Callvirt (!@ e)
    [<CustomOperation("ldloc", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldloc(p: Emitter, v) = p >> emitvar OpCodes.Ldloc v
    [<CustomOperation("stloc", MaintainsVariableSpaceUsingBind = true)>]
    member __.Stloc(p: Emitter, v) = p >> emitvar OpCodes.Stloc v
    [<CustomOperation("ceq", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ceq(p: Emitter) = p >> emit OpCodes.Ceq
    [<CustomOperation("ldc_node_type", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcXmlNodeType(p: Emitter, i: Xml.XmlNodeType) = p >> emitint OpCodes.Ldc_I4 i
    [<CustomOperation("ldc_i4", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt(p: Emitter, i) = p >> emitint OpCodes.Ldc_I4 i
    [<CustomOperation("ldc_i4_0", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt0(p: Emitter) = p >> emit OpCodes.Ldc_I4_0
    [<CustomOperation("ldc_i4_1", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt1(p: Emitter) = p >> emit OpCodes.Ldc_I4_1
    [<CustomOperation("ldc_i4_2", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt2(p: Emitter) = p >> emit OpCodes.Ldc_I4_2
    [<CustomOperation("brfalse", MaintainsVariableSpaceUsingBind = true)>]
    member __.Brfalse(p: Emitter, l) = p >> emitlbl OpCodes.Brfalse l
    [<CustomOperation("brtrue", MaintainsVariableSpaceUsingBind = true)>]
    member __.Brtrue(p: Emitter, l) = p >> emitlbl OpCodes.Brtrue l
    [<CustomOperation("br", MaintainsVariableSpaceUsingBind = true)>]
    member __.Br(p: Emitter, l) = p >> emitlbl OpCodes.Br l
    [<CustomOperation("clt", MaintainsVariableSpaceUsingBind = true)>]
    member __.Clt(p: Emitter) = p >> emit OpCodes.Clt
    [<CustomOperation("nop", MaintainsVariableSpaceUsingBind = true)>]
    member __.Nop(p: Emitter) = p >> emit OpCodes.Nop
    [<CustomOperation("unbox", MaintainsVariableSpaceUsingBind = true)>]
    member __.Unbox(p: Emitter, t) = p >> emittyp OpCodes.Unbox_Any t
    [<CustomOperation("box", MaintainsVariableSpaceUsingBind = true)>]
    member __.Box(p: Emitter, t) = p >> emittyp OpCodes.Box t
    [<CustomOperation("set_marker", MaintainsVariableSpaceUsingBind = true)>]
    member __.MarkLabel(p: Emitter, l) = p >> setLabel l
    [<CustomOperation("define_label", MaintainsVariableSpaceUsingBind = true)>]
    member __.DefineLabel(p: Emitter, e) = p >> (fun il -> let label = il |> defineLabel in il |> e label)
    [<CustomOperation("define_labels_2", MaintainsVariableSpaceUsingBind = true)>]
    member __.DefineLabels2(p: Emitter, e) = p >> (fun il -> il |> e (il |> defineLabel) (il |> defineLabel))
    [<CustomOperation("define_labels_3", MaintainsVariableSpaceUsingBind = true)>]
    member __.DefineLabels3(p: Emitter, e) = p >> (fun il -> il |> e (il |> defineLabel) (il |> defineLabel) (il |> defineLabel))
    [<CustomOperation("define_labels_4", MaintainsVariableSpaceUsingBind = true)>]
    member __.DefineLabels4(p: Emitter, e) = p >> (fun il -> il |> e (il |> defineLabel) (il |> defineLabel) (il |> defineLabel) (il |> defineLabel))
    [<CustomOperation("declare_variable", MaintainsVariableSpaceUsingBind = true)>]
    member __.DeclareVariable(p: Emitter, (v: Lazy<_>), f) = p >> (fun il -> il |> f (il |> v.Value))
    [<CustomOperation("merge", MaintainsVariableSpaceUsingBind = true)>]
    member __.Merge(p: Emitter, e) = p >> e
    [<CustomOperation("call", MaintainsVariableSpaceUsingBind = true)>]
    member __.Call(p: Emitter, mi) = p >> emitmi OpCodes.Call mi
    [<CustomOperation("ctor", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ctor(p: Emitter, ci) = p >> emitctor OpCodes.Call ci
    [<CustomOperation("call_expr", MaintainsVariableSpaceUsingBind = true)>]
    member __.CallExpr(p: Emitter, e) = p >> emitmi OpCodes.Call (!@ e)
    [<CustomOperation("ldstr", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldstr(p: Emitter, s) = p >> emitstr OpCodes.Ldstr s
    [<CustomOperation("string_equals", MaintainsVariableSpaceUsingBind = true)>]
    member __.StringEquals(p: Emitter) = p >> emitmi OpCodes.Call (!@ <@ "" = "" @>)
    [<CustomOperation("add", MaintainsVariableSpaceUsingBind = true)>]
    member __.Add(p: Emitter) = p >> emit OpCodes.Add
    [<CustomOperation("div", MaintainsVariableSpaceUsingBind = true)>]
    member __.Div(p: Emitter) = p >> emit OpCodes.Div
    [<CustomOperation("ret", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ret(p: Emitter) = p >> emit OpCodes.Ret
    [<CustomOperation("pop", MaintainsVariableSpaceUsingBind = true)>]
    member __.Pop(p: Emitter) = p >> emit OpCodes.Pop
    [<CustomOperation("throw", MaintainsVariableSpaceUsingBind = true)>]
    member __.Throw(p: Emitter) = p >> emit OpCodes.Throw
    [<CustomOperation("ldnull", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldnull(p: Emitter) = p >> emit OpCodes.Ldnull
    [<CustomOperation("newobj", MaintainsVariableSpaceUsingBind = true)>]
    member __.Newobj(p: Emitter, ci) = p >> create ci
    [<CustomOperation("dup", MaintainsVariableSpaceUsingBind = true)>]
    member __.Dup(p: Emitter) = p >> emit OpCodes.Dup
    [<CustomOperation("newobj_expr", MaintainsVariableSpaceUsingBind = true)>]
    member __.NewobjExpr(p: Emitter, e) = p >> create (!!@ e)
    [<CustomOperation("ldfld", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldfld(p: Emitter, f) = p >> emitfld OpCodes.Ldfld f
    [<CustomOperation("ldlen", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldlen(p: Emitter) = p >> emit OpCodes.Ldlen
    [<CustomOperation("conv_i4", MaintainsVariableSpaceUsingBind = true)>]
    member __.ConvInt4(p: Emitter) = p >> emit OpCodes.Conv_I4
    [<CustomOperation("ldelem_ref", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdelemRef(p: Emitter) = p >> emit OpCodes.Ldelem_Ref
    [<CustomOperation("ldelem", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldelem(p: Emitter, t) = p >> emittyp OpCodes.Ldelem t
    [<CustomOperation("ldloca", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldloca(p: Emitter, v) = p >> emitvar OpCodes.Ldloca v
    [<CustomOperation("initobj", MaintainsVariableSpaceUsingBind = true)>]
    member __.Initobj(p: Emitter, t) = p >> emittyp OpCodes.Initobj t

let emit' = EmitBuilder()
