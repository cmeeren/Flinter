[<AutoOpen>]
module Flinter.Analyzers.Helpers

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open FSharp.Analyzers.SDK

open Flinter.Ast


type Message with

    static member Create
        (
            message,
            range,
            severity,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] ruleFilePath: string
        ) =
        let fn = Path.GetFileNameWithoutExtension(ruleFilePath)
        let parts = fn.Split('_')

        {
            Code = parts[0]
            Type = parts[1]
            Message = message
            Severity = severity
            Range = range
            Fixes = []
        }

    static member Create(message, range, [<CallerFilePath; Optional; DefaultParameterValue("")>] ruleFilePath: string) =
        Message.Create(message, range, Severity.Warning, ruleFilePath)


let createUntypedAnalyzer (getVisitor: _ -> #UntypedAstVisitor) : Analyzer =
    fun (context: Context) ->
        let messages = ResizeArray()
        let visitor = getVisitor messages.Add
        visitor.VisitParsedInput context.ParseTree
        messages |> Seq.toList


let createTypedAnalyzer (getVisitor: _ -> #TypedAstVisitor) : Analyzer =
    fun (context: Context) ->
        let messages = ResizeArray()
        let visitor = getVisitor (context, messages.Add)
        visitor.VisitFSharpImplementationFileContents context.TypedTree
        messages |> Seq.toList
