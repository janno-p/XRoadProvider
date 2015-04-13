#I "packages/FAKE/tools"

#r "FakeLib"

open Fake
open Fake.ReleaseNotesHelper
open System.IO

let project = "XRoadProvider"
let summary = "Type providers for generating types and service interfaces for XRoad producers."
let description = "Type providers for generating types and service interfaces for XRoad producers."
let authors = [ "Janno Põldma" ]
let tags = "F# fsharp x-road xroad typeproviders x-tee xtee"

let buildDir = "bin"
let packagingDir = "release"
let releaseNotes = parseReleaseNotes (File.ReadAllLines "RELEASENOTES.md")

Target "Clean" (fun _ ->
    CleanDirs [buildDir; packagingDir]
    )

Target "Build" (fun _ ->
    !! (project + ".sln")
    |> MSBuildRelease buildDir "Rebuild"
    |> ignore
    )

Target "CreatePackage" (fun _->
    let libDir = packagingDir @@ "lib" @@ "net40"

    CleanDir packagingDir

    CopyDir libDir buildDir (fun _ -> true)

    NuGet (fun p ->
        { p with
            Authors = authors
            Project = project
            Description = description
            OutputPath = packagingDir
            Summary = summary
            WorkingDir = packagingDir
            Version = releaseNotes.NugetVersion
            ReleaseNotes = releaseNotes.Notes |> toLines
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Tags = tags
            Dependencies = []
            ToolPath = "packages" @@ "NuGet.CommandLine" @@ "tools" @@ "NuGet.exe" })
        (project + ".nuspec")
    )

Target "All" DoNothing
Target "Release" DoNothing

"Clean"
    ==> "Build"
    ==> "All"

"All"
    ==> "CreatePackage"
    ==> "Release"

RunTargetOrDefault "All"
