[<AutoOpen>]
module AnalyzerBootstrap

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text


/// Much of this is from:
//   - https://github.com/Zaid-Ajaj/Npgsql.FSharp.Analyzer/blob/798ab4fe989690688d61ed1196ca79bb4e1aca37/tests/NpgsqlFSharpAnalyzer.Tests/Analyzer.fs
//   - https://github.com/aaronpowell/FSharp.CosmosDb/blob/8d1cb92d294d56686ffa50eca56f123cf209eb96/tests/FSharp.CosmosDb.Analyzer.Tests/AnalyzerBootstrap.fs
//   - https://github.com/BinaryDefense/BinaryDefense.FSharp.Analyzers/blob/f9c5393df188ca75d22da7b4db0f81cc01e9d562/tests/BinaryDefense.FSharp.Analyzers.Tests/Analyzer.fs
//
// Relevant issue: https://github.com/ionide/FSharp.Analyzers.SDK/issues/18

module Context =

    let private filename = "script.fsx"


    let private parseFile source = async {
        let checker = FSharpChecker.Create()
        let sourceText = SourceText.ofString source
        let otherOpts = [| "--targetprofile:netstandard" |]

        let! projectOptions, _ =
            checker.GetProjectOptionsFromScript(
                filename,
                sourceText,
                assumeDotNetFramework = false,
                otherFlags = otherOpts
            )

        let parsingOptions, _diagnostics =
            checker.GetParsingOptionsFromProjectOptions(projectOptions)

        return! checker.ParseFile(filename, sourceText, parsingOptions)
    }


    let private typeCheck parseResults source = async {
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let sourceText = SourceText.ofString source
        let otherOpts = [| "--targetprofile:netstandard" |]

        let! projectOptions, _ =
            checker.GetProjectOptionsFromScript(
                filename,
                sourceText,
                assumeDotNetFramework = false,
                otherFlags = otherOpts
            )

        let! answer = checker.CheckFileInProject(parseResults, filename, 1, sourceText, projectOptions)

        return
            match answer with
            | FSharpCheckFileAnswer.Succeeded results -> results
            | FSharpCheckFileAnswer.Aborted -> failwith "Type check abandoned"
    }


    let private entityCache = EntityCache()


    let private getAllEntities (checkResults: FSharpCheckFileResults) (publicOnly: bool) : AssemblySymbol list =
        try
            let res = [
                yield!
                    AssemblyContent.GetAssemblySignatureContent
                        AssemblyContentType.Full
                        checkResults.PartialAssemblySignature

                let ctx = checkResults.ProjectContext

                let assembliesByFileName =
                    ctx.GetReferencedAssemblies()
                    |> Seq.groupBy (fun assembly -> assembly.FileName)
                    |> Seq.map (fun (fileName, assemblies) -> fileName, List.ofSeq assemblies)
                    |> Seq.toList
                    |> List.rev // If mscorlib.dll is the first, then FSC raises exception when we try to get Content.Entities from it.

                for fileName, signatures in assembliesByFileName do
                    let contentType =
                        if publicOnly then
                            AssemblyContentType.Public
                        else
                            AssemblyContentType.Full

                    let content =
                        AssemblyContent.GetAssemblyContent entityCache.Locking contentType fileName signatures

                    yield! content
            ]

            res
        with _ -> []


    let processTestSource (source: string) =
        source.ReplaceLineEndings()
        |> String.removePrefix Environment.NewLine
        |> String.deIndent


    let fromProcessedTestSource source : Context =
        let parseResults = parseFile source |> Async.RunSynchronously

        let typeCheckResults = typeCheck parseResults source |> Async.RunSynchronously

        {
            FileName = filename
            Content = source.Split(Environment.NewLine)
            ParseTree = parseResults.ParseTree
            TypedTree = typeCheckResults.ImplementationFile.Value
            Symbols = typeCheckResults.PartialAssemblySignature.Entities |> Seq.toList
            GetAllEntities = getAllEntities typeCheckResults
        }


    let fromTestSource (source: string) : Context =
        source |> processTestSource |> fromProcessedTestSource
