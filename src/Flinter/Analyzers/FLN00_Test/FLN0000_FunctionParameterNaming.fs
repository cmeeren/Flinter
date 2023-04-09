module Flinter.Analyzers.FLN0000_FunctionParameterNaming

open System

open FSharp.Compiler.Syntax

open FSharp.Analyzers.SDK

open Flinter.Ast
open Flinter.Analyzers


let private createMsg (i: Ident) =
    Message.Create($"The parameter name '%s{i.idText}' does not follow the rule for parameter names.", i.idRange)


let private isInvalid (i: Ident) = i.idText.Chars(0) |> Char.IsUpper


type private Visitor(addMsg) =
    inherit UntypedAstVisitor()

    override this.VisitSynBinding(node, path) =
        match node with
        | SynBinding(_, _, _, _, _, _, SynValData(_, SynValInfo(curriedArgInfos, _), _), _, _, _, _, _) ->
            curriedArgInfos
            |> List.iter (List.iter (fun i -> i.Ident |> Option.filter isInvalid |> Option.iter (createMsg >> addMsg)))

        base.VisitSynBinding(node, path)


[<Analyzer>]
let analyze: Analyzer = createUntypedAnalyzer Visitor
