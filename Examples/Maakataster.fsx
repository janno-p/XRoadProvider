#r @"..\XteeTypeProvider\bin\Debug\XteeTypeProvider.dll"

open XteeTypeProvider

type Provider = XteeTypeProvider<"Maakataster.wsdl">
type Maakataster = Provider.``http://producers.maakataster.xtee.riik.ee/producer/maakataster``
