namespace XRoadProvider.Tests

module SoapUtil =
    open XRoad

    let makeSoapResponse content =
        sprintf @"<?xml version=""1.0"" encoding=""utf-8""?>
<soapenv:Envelope xmlns:soapenv=""%s"">
    <soapenv:Body>
        %s
    </soapenv:Body>
</soapenv:Envelope>" XmlNamespace.SoapEnv content
