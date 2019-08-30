#r "paket: groupref Build //"

#load "./.fake/build.fsx/intellisense.fsx"
#load "./paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"

#if INTERACTIVE
#r "netstandard"
#endif

open Fake.Core
open Fake.Core.TargetOperators
open Fake.Documentation
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open Octokit
open System

Target.initEnvironment()

let [<Literal>] ProjectName = "XRoadProvider"
let [<Literal>] Summary = "Type providers for generating types and service interfaces for XRoad producers."
let [<Literal>] Description = "Type providers for generating types and service interfaces for XRoad producers."
let [<Literal>] Tags = "F# fsharp x-road xroad typeproviders x-tee xtee"
let [<Literal>] GitOwner = "janno-p"

let authors = [ "Janno Põldma" ]

let gitHome = "https://github.com/" + GitOwner
let gitName = ProjectName

let enableNet40 = Environment.environVarAsBool "DisableNet40" |> not

let projects = [
    yield ProjectName
    if enableNet40 then
        yield sprintf "%s.net40" ProjectName
]

let testProjects = [
    yield sprintf "%s.Tests" ProjectName
    if enableNet40 then
        yield sprintf "%s.Tests.net40" ProjectName
]

let projectPath = __SOURCE_DIRECTORY__ </> "src"
let testProjectPath = __SOURCE_DIRECTORY__ </> "tests"
let testAssemblies = testProjectPath </> "**" </> "bin" </> "Debug" </> "**" </> "*Tests*.exe"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let docfxToolPath = __SOURCE_DIRECTORY__ </> "paket-files" </> "build" </> "github.com" </> "docfx.exe"
let tempDocsDir = "temp" </> "gh-pages"

Target.description "Generate assembly info files with the right version & up-to-date information"
Target.create "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ AssemblyInfo.Title (projectName)
          AssemblyInfo.Product ProjectName
          AssemblyInfo.Description Summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion
          AssemblyInfo.InformationalVersion release.NugetVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.fsproj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (_, _, folderName, attributes) -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes)
)

Target.description "Copies binaries from default VS location to exepcted bin folder, but keeps a subdirectory structure for each project in the src folder to support multiple project outputs"
Target.create "CopyBinaries" (fun _ ->
    !! "src/**/*.fsproj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin" </> "Release", "bin"))
    |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
)

Target.description "Clean build results"
Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"]
)

Target.description "Build test project"
Target.create "BuildDebug" (fun _ ->
    testProjects
    |> Seq.map ((</>) testProjectPath)
    |> Seq.iter (fun path ->
        DotNet.restore id path
        DotNet.build (fun p -> { p with Configuration = DotNet.BuildConfiguration.Debug }) path
    )
)

Target.description "Build library for release"
Target.create "Build" (fun _ ->
    projects
    |> Seq.map ((</>) projectPath)
    |> Seq.iter (fun path ->
        DotNet.restore id path
        DotNet.build
            (fun p ->
                { p with
                    Common = { p.Common with CustomParams = Some(sprintf "/p:Version=%s" release.NugetVersion) }
                    Configuration = DotNet.BuildConfiguration.Release })
            path
    )
)

Target.description "Run the unit tests using test runner"
Target.create "RunTests" (fun _ ->
    if Environment.isUnix then
        !! testAssemblies
        |> Seq.iter (fun testExe ->
            let result =
                Command.RawCommand(testExe, Arguments.Empty)
                |> CreateProcess.fromCommand
                |> CreateProcess.withFramework
                |> CreateProcess.withTimeout TimeSpan.MaxValue
                |> Proc.run
            if result.ExitCode <> 0 then failwithf "Running test assembly '%s' failed." testExe
        )
    else
        Expecto.run id (!! testAssemblies)
)

Target.description "Build a NuGet package"
Target.create "NuGet" (fun _ ->
    Shell.copyDir ("temp" </> "lib") "bin" FileFilter.allFiles

    NuGet.NuGet.NuGet (fun p ->
        { p with
            Authors = authors
            Project = ProjectName
            Summary = Summary
            Description = Description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = Tags
            WorkingDir = "temp"
            OutputPath = "bin"
            Dependencies = [] })
        (ProjectName + ".nuspec")
)

Target.create "PublishNuget" (fun _ ->
    let apiKey = Environment.environVarOrFail "nugetkey"
    Paket.push(fun p ->
        { p with
            ApiKey = apiKey
            WorkingDir = "bin" })
)

Target.create "GenerateHelp" (fun _ ->
    Shell.rm "docs/articles/release-notes.md"
    Shell.copyFile "docs/articles/" "RELEASE_NOTES.md"
    Shell.rename "docs/articles/release-notes.md" "docs/articles/RELEASE_NOTES.md"

    Shell.rm "docs/articles/license.md"
    Shell.copyFile "docs/articles/" "LICENSE.md"
    Shell.rename "docs/articles/license.md" "docs/articles/LICENSE.md"
)

Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs [ tempDocsDir ]
)

Target.create "Serve" (fun _ ->
    DocFx.exec
        (fun p ->
            { p with
                DocFxPath = docfxToolPath
                Timeout = TimeSpan.MaxValue })
        "serve"
        tempDocsDir
)

Target.description "Generate the documentation"
Target.create "GenerateDocs" (fun _ ->
    DocFx.exec
        (fun p -> { p with DocFxPath = docfxToolPath })
        (__SOURCE_DIRECTORY__ </> "docs" </> "docfx.json")
        ""
)

Target.create "ReleaseDocs" (fun _ ->
    Shell.cleanDirs [ tempDocsDir ]
    Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
    DocFx.exec
        (fun p -> { p with DocFxPath = docfxToolPath })
        (__SOURCE_DIRECTORY__ </> "docs" </> "docfx.json")
        ""
    Git.Staging.stageAll tempDocsDir
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Git.Branches.push tempDocsDir
)

Target.create "Release" (fun _ ->
    let user = Environment.environVarOrFail "github-user"
    let pw = Environment.environVarOrFail "github-pw"
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(GitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    createClient user pw
    |> createDraft GitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> releaseDraft
    |> Async.RunSynchronously
)

Target.create "BuildPackage" ignore
Target.create "All" ignore

"Clean"
  ==> "AssemblyInfo"
  ==> "BuildDebug"
  ==> "RunTests"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "GenerateDocs"
  ==> "All"
  =?> ("ReleaseDocs", BuildServer.isLocalBuild)

"All"
  ==> "NuGet"
  ==> "BuildPackage"

"CleanDocs"
  ==> "GenerateHelp"
  ==> "GenerateDocs"

"GenerateHelp"
  ==> "Serve"

"ReleaseDocs"
  ==> "Release"

"BuildPackage"
  ==> "PublishNuget"
  ==> "Release"

Target.runOrDefault "All"
