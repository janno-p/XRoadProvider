#r @"..\XteeTypeProvider\bin\Debug\XteeTypeProvider.dll"

open XteeTypeProvider

type Provider = XteeTypeProvider< @"E:\Work\XteeTypeProvider\Examples\Maakataster.wsdl" >
type Maakataster = Provider.``http://producers.maakataster.xtee.riik.ee/producer/maakataster``

Provider.ky()
Provider.legacy1()
Provider.uploadMime()
