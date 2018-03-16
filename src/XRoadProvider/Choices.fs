namespace XRoad.Choices

open System.Runtime.InteropServices

type IChoiceOf1<'T1> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool

type IChoiceOf2<'T1, 'T2> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool
    abstract TryGetOption2: [<Out>] value: 'T2 byref -> bool

type IChoiceOf3<'T1, 'T2, 'T3> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool
    abstract TryGetOption2: [<Out>] value: 'T2 byref -> bool
    abstract TryGetOption3: [<Out>] value: 'T3 byref -> bool

type IChoiceOf4<'T1, 'T2, 'T3, 'T4> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool
    abstract TryGetOption2: [<Out>] value: 'T2 byref -> bool
    abstract TryGetOption3: [<Out>] value: 'T3 byref -> bool
    abstract TryGetOption4: [<Out>] value: 'T4 byref -> bool

type IChoiceOf5<'T1, 'T2, 'T3, 'T4, 'T5> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool
    abstract TryGetOption2: [<Out>] value: 'T2 byref -> bool
    abstract TryGetOption3: [<Out>] value: 'T3 byref -> bool
    abstract TryGetOption4: [<Out>] value: 'T4 byref -> bool
    abstract TryGetOption5: [<Out>] value: 'T5 byref -> bool

type IChoiceOf6<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool
    abstract TryGetOption2: [<Out>] value: 'T2 byref -> bool
    abstract TryGetOption3: [<Out>] value: 'T3 byref -> bool
    abstract TryGetOption4: [<Out>] value: 'T4 byref -> bool
    abstract TryGetOption5: [<Out>] value: 'T5 byref -> bool
    abstract TryGetOption6: [<Out>] value: 'T6 byref -> bool

type IChoiceOf7<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool
    abstract TryGetOption2: [<Out>] value: 'T2 byref -> bool
    abstract TryGetOption3: [<Out>] value: 'T3 byref -> bool
    abstract TryGetOption4: [<Out>] value: 'T4 byref -> bool
    abstract TryGetOption5: [<Out>] value: 'T5 byref -> bool
    abstract TryGetOption6: [<Out>] value: 'T6 byref -> bool
    abstract TryGetOption7: [<Out>] value: 'T7 byref -> bool

type IChoiceOf8<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'T8> =
    abstract TryGetOption1: [<Out>] value: 'T1 byref -> bool
    abstract TryGetOption2: [<Out>] value: 'T2 byref -> bool
    abstract TryGetOption3: [<Out>] value: 'T3 byref -> bool
    abstract TryGetOption4: [<Out>] value: 'T4 byref -> bool
    abstract TryGetOption5: [<Out>] value: 'T5 byref -> bool
    abstract TryGetOption6: [<Out>] value: 'T6 byref -> bool
    abstract TryGetOption7: [<Out>] value: 'T7 byref -> bool
    abstract TryGetOption8: [<Out>] value: 'T8 byref -> bool
