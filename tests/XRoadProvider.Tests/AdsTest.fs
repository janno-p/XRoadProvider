module XRoadProvider.Tests.AdsTest

open Expecto
open Microsoft.FSharp.Core.CompilerServices
open NodaTime
open System
open System.Numerics
open XRoad
open XRoad.Serialization.Attributes

module DefinedTypes =
    [<XRoadType>]
    type ADSaadrmuudatused_muudetudPaevadType (value: int) =
        private new () = ADSaadrmuudatused_muudetudPaevadType(0)

        [<XRoadElement(MergeContent=true)>]
        member val BaseValue = value with get, set

    [<XRoadType>]
    type ADSaadrmuudatused_maxarvType (value: bigint) =
        private new () = ADSaadrmuudatused_maxarvType(0I)

        [<XRoadElement(MergeContent=true)>]
        member val BaseValue = value with get, set
        
    [<XRoadType("adsobjliikKlassifikaator", LayoutKind.Sequence, Namespace="http://www.maaamet.ee"); TypeProviderXmlDoc("adsobjliik klassifikaator")>]
    type adsobjliikKlassifikaator (value: string) =
        private new() = adsobjliikKlassifikaator(null)

        [<XRoadElement(MergeContent=true)>]
        member val BaseValue = value with get, set

        static member AY = adsobjliikKlassifikaator("AY")
        static member CU = adsobjliikKlassifikaator("CU")
        static member EE = adsobjliikKlassifikaator("EE")
        static member ER = adsobjliikKlassifikaator("ER")
        static member KN = adsobjliikKlassifikaator("KN")
        static member LO = adsobjliikKlassifikaator("LO")
        static member LP = adsobjliikKlassifikaator("LP")
        static member ME = adsobjliikKlassifikaator("ME")
        static member MK = adsobjliikKlassifikaator("MK")
        static member MR = adsobjliikKlassifikaator("MR")
        static member OV = adsobjliikKlassifikaator("OV")
        static member TT = adsobjliikKlassifikaator("TT")
        static member VK = adsobjliikKlassifikaator("VK")
        
    [<XRoadType("adsSyndmusType", LayoutKind.Sequence, Namespace="http://www.maaamet.ee")>]
    type adsSyndmusType (value: string) =
        private new () = adsSyndmusType(null)

        [<XRoadElement(MergeContent=true)>]
        member val BaseValue = value with get, set

        static member I = adsSyndmusType("I")
        static member U = adsSyndmusType("U")
        static member D = adsSyndmusType("D")
        static member R = adsSyndmusType("R")
        static member S = adsSyndmusType("S")
        static member H = adsSyndmusType("H")
        static member P = adsSyndmusType("P")

    [<XRoadType>]
    type adrSeosedAdrseos () =
        [<XRoadElement; TypeProviderXmlDoc("Objekti liigi nimi klassifikaatorist")>]
        member val objektiLiik = Unchecked.defaultof<adsobjliikKlassifikaator> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti ADS süsteemi kood")>]
        member val adsOid = Unchecked.defaultof<string> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti versioonitunnus ADS süsteemis")>]
        member val adobId = Unchecked.defaultof<string> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti tunnus originaalregistris")>]
        member val origTunnus = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekt on unikaalaadressi tunnusega")>]
        member val unikaalne = Optional.Option.None<bool>() with get, set

    [<XRoadType("adsTaseType", LayoutKind.Sequence, Namespace="http://www.maaamet.ee")>]
    type adsTaseType () =
        [<XRoadElement; TypeProviderXmlDoc("Komponendi kood")>]
        member val kood = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Komponendi nimi")>]
        member val nimetus = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Komponendi nimi koos liigisõnaga")>]
        member val nimetus_liigiga = Optional.Option.None<string>() with get, set

    [<XRoadType>]
    type muudatusedMuudatus () =
        [<XRoadElement; TypeProviderXmlDoc("Logikirje ID")>]
        member val logId = Unchecked.defaultof<bigint> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Logikirje tekkimise aeg, muudatuse tegemise aeg koos kellaajaga")>]
        member val logStamp = Unchecked.defaultof<LocalDateTime> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Sündmus")>]
        member val syndmus = Unchecked.defaultof<adsSyndmusType> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Koodaadress")>]
        member val koodAadress = Unchecked.defaultof<string> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadressi unikaalne võti")>]
        member val adrId = Unchecked.defaultof<bigint> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Normaliseeritud aadressitekst")>]
        member val taisAadress = Unchecked.defaultof<string> with get, set
        [<XRoadElement; TypeProviderXmlDoc("Normaliseeritud lähiaadress")>]
        member val lahiAadress = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadressi eelmise versiooni number muutmise korral")>]
        member val vanaAdrId = Optional.Option.None<bigint>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadressi eelmine täielik nimekuju muutmise korral")>]
        member val vanaTaisAadress = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Eelmine lähiaadress muutmise korral")>]
        member val vanaLahiAadress = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadressi esinduspunkti x-koordinaat")>]
        member val esindusPunktX = Optional.Option.None<double>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadressi esinduspunkti y-koordinaat")>]
        member val esindusPunktY = Optional.Option.None<double>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadressi võimalikeks eellasteks olevate aadresside ADR_ID väärtuste loetelu")>]
        member val eellased = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadressi võimalikeks järglaseks olevate aadresside ADR_ID väärtuste loetelu")>]
        member val jarglased = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Aadress on kehtetu ja ta pole seotud ühegi kehtiva objektiga")>]
        member val poleSeotud = false with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti liigi nimi klassifikaatorist")>]
        member val objektiLiik = Optional.Option.None<adsobjliikKlassifikaator>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti ADS süsteemi kood")>]
        member val adsOid = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti versioonitunnus ADS süsteemis")>]
        member val adobId = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti tunnus originaalregistris")>]
        member val origTunnus = Optional.Option.None<string>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti tsentroidipunkti x-koordinaat")>]
        member val punktX = Optional.Option.None<double>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Objekti tsentroidipunkti y-koordinaat")>]
        member val punktY = Optional.Option.None<double>() with get, set
        [<XRoadElement; XRoadCollection("adrSeos")>]
        member val adrSeosed = Optional.Option.None<adrSeosedAdrseos[]>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 1")>]
        member val adsTase1 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 2")>]
        member val adsTase2 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 3")>]
        member val adsTase3 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 4")>]
        member val adsTase4 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 5")>]
        member val adsTase5 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 6")>]
        member val adsTase6 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 7")>]
        member val adsTase7 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Tase 8")>]
        member val adsTase8 = Optional.Option.None<adsTaseType>() with get, set
        [<XRoadElement; TypeProviderXmlDoc("Taseme number, mis põhjustas aadressi muudatuse või tühistamise (U ja D sündmuse korral)")>]
        member val muudetudTase = Optional.Option.None<bigint>() with get, set

    [<XRoadType>]
    type faultType () =
        [<TypeProviderXmlDoc("Vea kood"); XRoadElement>]
        member val faultCode = Unchecked.defaultof<string> with get, set
        
        [<TypeProviderXmlDoc("Vea kirjeldus"); XRoadElement>]
        member val faultString = Unchecked.defaultof<string> with get, set

    [<XRoadType("ADSaadrmuudatusedResponse", LayoutKind.Sequence, Namespace="http://www.maaamet.ee")>]
    type ADSaadrmuudatusedResponse () =
        [<XRoadElement; XRoadCollection("muudatus")>]
        member val muudatused = Optional.Option.None<muudatusedMuudatus[]>() with get, set

        [<XRoadElement>]
        member val fault = Optional.Option.None<faultType>() with get, set

type AdsAadrMuudatusedService () as this =
    inherit AbstractEndpointDeclaration(Uri("http://mixerlivebal.webdb.maaamet.ee/xtee-proxy"))

    [<XRoadOperation("ADSaadrmuudatused", "v4", XRoad.Serialization.Attributes.XRoadProtocol.Version40, ProtocolVersion="4.0")>]
    [<TypeProviderXmlDoc("Aadressi muudatuste päring")>]
    [<XRoadRequiredHeaders("http://x-road.eu/xsd/xroad.xsd", "protocolVersion", "issue", "userId", "id", "service", "client")>]
    [<XRoadRequest("ADSaadrmuudatused", "http://www.maaamet.ee")>]
    [<XRoadResponse("ADSaadrmuudatusedResponse", "http://www.maaamet.ee")>]
    member __.ADSaadrmuudatused(header: XRoadHeader,
                                  [<XRoadElement>] muudetudAlates: Optional.Option<LocalDate>,
                                  [<XRoadElement>] muudetudPaevad: Optional.Option<DefinedTypes.ADSaadrmuudatused_muudetudPaevadType>,
                                  [<XRoadElement>] logId: Optional.Option<bigint>,
                                  [<XRoadElement>] maxarv: Optional.Option<DefinedTypes.ADSaadrmuudatused_maxarvType>,
                                  [<XRoadElement>] pSyndmused: Optional.Option<bool>,
                                  [<XRoadElement>] sSyndmused: Optional.Option<bool>,
                                  [<XRoadElement>] objekt: Optional.Option<bool>,
                                  [<XRoadElement>] seosed: Optional.Option<bool>,
                                  [<XRoadElement>] aadressKomp: Optional.Option<bool>,
                                  [<XRoadElement>] aadressJarglased: Optional.Option<bool>) =
        XRoadUtil.MakeServiceCall(
            this,
            "ADSaadrmuudatused",
            header,
            [| muudetudAlates; muudetudPaevad; logId; maxarv; pSyndmused; sSyndmused; objekt; seosed; aadressKomp; aadressJarglased |])
        |> unbox<DefinedTypes.ADSaadrmuudatusedResponse>

let getResponse<'T> = SerializationUtil.getResponse<'T> typeof<AdsAadrMuudatusedService> (SerializerContext())
let internal serialize = SerializationUtil.serialize typeof<AdsAadrMuudatusedService> "http://www.maaamet.ee"
let serialize' = serialize (SerializerContext())

let none<'T> () = Optional.Option.None<'T>()

let [<Tests>] tests =
    testList "Ads service tests" [
        test "can serialize empty Ads service request" {
            let xml = serialize' "ADSaadrmuudatused" [| none<LocalDate>(); none<DefinedTypes.ADSaadrmuudatused_muudetudPaevadType>(); none<bigint>(); none<DefinedTypes.ADSaadrmuudatused_maxarvType>(); none<bool>(); none<bool>(); none<bool>(); none<bool>(); none<bool>(); none<bool>() |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://www.maaamet.ee"" xmlns:test=""testns""><tns:ADSaadrmuudatused /></Body>" ""
        }
        
        test "can deserialize response content" {
            let xml = @"<Body><tns:ADSaadrmuudatusedResponse xmlns:tns=""http://www.maaamet.ee""><muudatused><muudatus><logId>6231164</logId><logStamp>2017-11-15T02:09:51</logStamp><syndmus>D</syndmus><koodAadress>377187361000000007811000000000000</koodAadress><adrId>187082</adrId><taisAadress>Harju maakond, Saku vald, Saku alevik, Tiina</taisAadress><lahiAadress>Tiina</lahiAadress><esindusPunktX>6573428.3</esindusPunktX><esindusPunktY>538636.95</esindusPunktY><poleSeotud>true</poleSeotud></muudatus><muudatus><logId>6231166</logId><logStamp>2017-11-15T02:10:29</logStamp><syndmus>D</syndmus><koodAadress>37198681400000POE00009MAEEBF30000</koodAadress><adrId>1917550</adrId><taisAadress>Harju maakond, Harku vald, Rannamõisa küla, Andruse põik 1-19</taisAadress><lahiAadress>Andruse põik 1-19</lahiAadress><esindusPunktX>6589070.49</esindusPunktX><esindusPunktY>529179.23</esindusPunktY><poleSeotud>true</poleSeotud></muudatus></muudatused></tns:ADSaadrmuudatusedResponse></Body>"
            let result: DefinedTypes.ADSaadrmuudatusedResponse = getResponse "ADSaadrmuudatused" xml
            Expect.isFalse result.fault.HasValue ""
            Expect.isTrue result.muudatused.HasValue ""
            let arr = result.muudatused.ValueOr [||]
            Expect.equal arr.Length 2 ""
        }
    ]
