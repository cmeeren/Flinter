module Consistency

open System.IO
open System.Reflection
open Xunit
open FluentAssertions


let testProjectPath =
    // Can't use __SOURCE_DIRECTORY__ because that fails on CI, likely due to DotNet.ReproducibleBuilds
    Path.Combine(Assembly.GetExecutingAssembly().Location, "..", "..", "..", "..")

let testProjectAnalyzersPath = Path.Combine(testProjectPath, "Analyzers")
let analyzerProjectPath = Path.Combine(testProjectPath, "..", "Flinter")
let analyzerProjectAnalyzersPath = Path.Combine(analyzerProjectPath, "Analyzers")


[<Fact>]
let ``All analyzer folders should start with a valid prefix`` () =
    let analyzerFolderNames =
        DirectoryInfo(analyzerProjectAnalyzersPath).GetDirectories()
        |> Array.map (fun di -> di.Name)

    analyzerFolderNames
        .Should()
        .AllSatisfy((fun folderName -> folderName.Should().MatchRegex(@"^FLN\d\d_", "") |> ignore), "")


[<Fact>]
let ``Analyzer folder prefixes should be distinct`` () =
    let analyzerFolderPrefixes =
        DirectoryInfo(analyzerProjectAnalyzersPath).GetDirectories()
        |> Array.map (fun di -> di.Name.Split('_')[0])

    analyzerFolderPrefixes.Should().OnlyHaveUniqueItems("")


[<Fact>]
let ``All analyzer files should start with a valid prefix`` () =
    let analyzerFileNames =
        DirectoryInfo(analyzerProjectAnalyzersPath).GetDirectories()
        |> Array.collect (fun di -> di.GetFiles())
        |> Array.map (fun fi -> fi.Name)

    analyzerFileNames
        .Should()
        .AllSatisfy((fun fileName -> fileName.Should().MatchRegex(@"^FLN\d\d\d\d_", "") |> ignore), "")


[<Fact>]
let ``Analyzer file prefixes should be distinct`` () =
    let analyzerFilePrefixes =
        DirectoryInfo(analyzerProjectAnalyzersPath).GetDirectories()
        |> Array.collect (fun di -> di.GetFiles())
        |> Array.map (fun fi -> fi.Name.Split('_')[0])

    analyzerFilePrefixes.Should().OnlyHaveUniqueItems("")


[<Fact>]
let ``All analyzer file prefixes should match the folder prefix`` () =
    let analyzerFolderPrefixesAndFileNames =
        DirectoryInfo(analyzerProjectAnalyzersPath).GetDirectories()
        |> Array.map (fun di -> di.Name.Split('_')[0], (di.GetFiles() |> Array.map (fun fi -> fi.Name)))

    analyzerFolderPrefixesAndFileNames
        .Should()
        .AllSatisfy(
            (fun (folderPrefix, fileNames) ->
                fileNames
                    .Should()
                    .AllSatisfy((fun fileName -> fileName.Should().StartWith(folderPrefix, "") |> ignore), "")
                |> ignore),
            ""
        )


[<Fact>]
let ``All analyzer files should start with a module name identical to the file name`` () =
    let analyzerFileNamesAndFirstLines =
        DirectoryInfo(analyzerProjectAnalyzersPath).GetDirectories()
        |> Array.collect (fun di -> di.GetFiles())
        |> Array.map (fun fi -> Path.GetFileNameWithoutExtension(fi.Name), File.ReadLines(fi.FullName) |> Seq.head)

    analyzerFileNamesAndFirstLines
        .Should()
        .AllSatisfy(
            (fun (fileNameWithoutExt, firstLine) ->
                firstLine.Should().Be($"module Flinter.Analyzers.{fileNameWithoutExt}", "")
                |> ignore),
            ""
        )


[<Fact>]
let ``All analyzer files should have a corresponding test file`` () =
    let testFolderAndFileNames =
        DirectoryInfo(testProjectAnalyzersPath).GetDirectories()
        |> Array.map (fun di -> di.Name, (di.GetFiles() |> Array.map (fun fi -> fi.Name)))

    let analyzerFolderAndFileNames =
        DirectoryInfo(analyzerProjectAnalyzersPath).GetDirectories()
        |> Array.map (fun di -> di.Name, (di.GetFiles() |> Array.map (fun fi -> fi.Name)))

    testFolderAndFileNames.Should().BeEquivalentTo(analyzerFolderAndFileNames, "")
