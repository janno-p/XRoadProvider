namespace XRoad.Choices

open System.Runtime.InteropServices

type IChoiceOf1<'a> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool

type IChoiceOf2<'a,'b> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool
    abstract TryGetOption2: [<Out>] value: 'b byref -> bool

type IChoiceOf3<'a,'b,'c> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool
    abstract TryGetOption2: [<Out>] value: 'b byref -> bool
    abstract TryGetOption3: [<Out>] value: 'c byref -> bool

type IChoiceOf4<'a,'b,'c,'d> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool
    abstract TryGetOption2: [<Out>] value: 'b byref -> bool
    abstract TryGetOption3: [<Out>] value: 'c byref -> bool
    abstract TryGetOption4: [<Out>] value: 'd byref -> bool

type IChoiceOf5<'a,'b,'c,'d,'e> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool
    abstract TryGetOption2: [<Out>] value: 'b byref -> bool
    abstract TryGetOption3: [<Out>] value: 'c byref -> bool
    abstract TryGetOption4: [<Out>] value: 'd byref -> bool
    abstract TryGetOption5: [<Out>] value: 'e byref -> bool

type IChoiceOf6<'a,'b,'c,'d,'e,'f> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool
    abstract TryGetOption2: [<Out>] value: 'b byref -> bool
    abstract TryGetOption3: [<Out>] value: 'c byref -> bool
    abstract TryGetOption4: [<Out>] value: 'd byref -> bool
    abstract TryGetOption5: [<Out>] value: 'e byref -> bool
    abstract TryGetOption6: [<Out>] value: 'f byref -> bool

type IChoiceOf7<'a,'b,'c,'d,'e,'f,'g> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool
    abstract TryGetOption2: [<Out>] value: 'b byref -> bool
    abstract TryGetOption3: [<Out>] value: 'c byref -> bool
    abstract TryGetOption4: [<Out>] value: 'd byref -> bool
    abstract TryGetOption5: [<Out>] value: 'e byref -> bool
    abstract TryGetOption6: [<Out>] value: 'f byref -> bool
    abstract TryGetOption7: [<Out>] value: 'g byref -> bool

type IChoiceOf8<'a,'b,'c,'d,'e,'f,'g,'h> =
    abstract TryGetOption1: [<Out>] value: 'a byref -> bool
    abstract TryGetOption2: [<Out>] value: 'b byref -> bool
    abstract TryGetOption3: [<Out>] value: 'c byref -> bool
    abstract TryGetOption4: [<Out>] value: 'd byref -> bool
    abstract TryGetOption5: [<Out>] value: 'e byref -> bool
    abstract TryGetOption6: [<Out>] value: 'f byref -> bool
    abstract TryGetOption7: [<Out>] value: 'g byref -> bool
    abstract TryGetOption8: [<Out>] value: 'h byref -> bool
