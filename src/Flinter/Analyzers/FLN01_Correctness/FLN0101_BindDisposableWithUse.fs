module Flinter.Analyzers.FLN0101_BindDisposableWithUse


open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax

open FSharp.Analyzers.SDK

open Flinter.Ast
open Flinter.Analyzers


let private createMsg exprRange (exprType: FSharpType) =
    Message.Create(
        $"This value has type '%s{exprType.TypeDefinition.FullName}' which implements IDisposable, and should therefore be bound with 'use' instead of 'let'.",
        exprRange
    )

let private isDisposable (ty: FSharpType) =
    ty.AllInterfaces
    |> Seq.exists (fun i -> i.HasTypeDefinition && i.TypeDefinition.TryFullName = Some "System.IDisposable")


let private exemptedTypes =
    set [
        "System.IO.MemoryStream"
        "System.IO.StringReader"
        "System.IO.StringWriter"
        "System.IO.UnmanagedMemoryStream"
        "System.IO.UnmanagedMemoryAccessor"
    ]


let isExempted (ty: FSharpType) =
    ty.HasTypeDefinition
    && ty.TypeDefinition.TryFullName
       |> Option.map exemptedTypes.Contains
       |> Option.defaultValue false


// TODO: Check coverage


type private IsExpressionUseBoundVisitor(range) =
    inherit SyntaxVisitorBase<bool>()

    override _.VisitExpr(_, _, defaultTraverse, synExpr) =
        match synExpr with
        | SynExpr.LetOrUse(isUse = isUse; bindings = bindings) when
            bindings |> List.exists (fun b -> b.RangeOfHeadPattern = range)
            ->
            Some isUse
        | SynExpr.LetOrUseBang(isUse = isUse; pat = pat) when pat.Range = range -> Some isUse
        | _ -> defaultTraverse synExpr


type private Visitor(ctx: Context, addMsg) =
    inherit TypedAstVisitor()

    let exprIsUseBound (v: FSharpMemberOrFunctionOrValue) =
        SyntaxTraversal.Traverse(
            v.DeclarationLocation.Start,
            ctx.ParseTree,
            IsExpressionUseBoundVisitor(v.DeclarationLocation)
        )
        |> Option.defaultValue false

    let addMsgIfRelevant (v: FSharpMemberOrFunctionOrValue) =
        if not (exprIsUseBound v) then
            v.FullTypeSafe
            |> Option.filter isDisposable
            |> Option.filter (not << isExempted)
            |> Option.iter (createMsg v.DeclarationLocation >> addMsg)

    override this.VisitFSharpExpr(node, path) =
        match node with
        | FSharpExprPatterns.LetRec(recursiveBindings, _) -> recursiveBindings |> List.iter (fst >> addMsgIfRelevant)
        | FSharpExprPatterns.Let((v, _), _) when v.IsValue -> addMsgIfRelevant v
        | _ -> ()

        base.VisitFSharpExpr(node, path)

// TODO: Specify analyzer name in attribute? What are these names used for?
[<Analyzer>]
let analyze: Analyzer = createTypedAnalyzer Visitor
