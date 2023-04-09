namespace Flinter.Ast

open FSharp.Compiler.Symbols


/// Used to track route during traversal AST
[<RequireQualifiedAccess>]
type TypedAstNode =
    | FSharpEntity of FSharpEntity
    | FSharpExpr of FSharpExpr
    | FSharpImplementationFileContents of FSharpImplementationFileContents
    | FSharpImplementationFileDeclaration of FSharpImplementationFileDeclaration
    | FSharpMemberOrFunctionOrValue of FSharpMemberOrFunctionOrValue


type TypedAstVisitor() =


    abstract member VisitFSharpEntity: node: FSharpEntity * path: TypedAstNode list -> unit

    default this.VisitFSharpEntity(node: FSharpEntity, path: TypedAstNode list) =
        let withPath n =
            n, TypedAstNode.FSharpEntity node :: path

        // TODO: Verify whether this is correct
        node.MembersFunctionsAndValues
        |> Seq.iter (withPath >> this.VisitFSharpMemberOrFunctionOrValue)

        // TODO: Verify whether this is correct
        node.NestedEntities |> Seq.iter (withPath >> this.VisitFSharpEntity)


    abstract member VisitFSharpExpr: node: FSharpExpr * path: TypedAstNode list -> unit

    default this.VisitFSharpExpr(node: FSharpExpr, path: TypedAstNode list) =
        let withPath n = n, TypedAstNode.FSharpExpr node :: path
        // TODO: Verify
        node.ImmediateSubExpressions |> List.iter (withPath >> this.VisitFSharpExpr)


    abstract member VisitFSharpImplementationFileContents: node: FSharpImplementationFileContents -> unit

    default this.VisitFSharpImplementationFileContents(node: FSharpImplementationFileContents) =
        let withPath n =
            n, [ TypedAstNode.FSharpImplementationFileContents node ]

        node.Declarations
        |> List.iter (withPath >> this.VisitFSharpImplementationFileDeclaration)


    abstract member VisitFSharpImplementationFileDeclaration:
        node: FSharpImplementationFileDeclaration * path: TypedAstNode list -> unit

    default this.VisitFSharpImplementationFileDeclaration
        (
            node: FSharpImplementationFileDeclaration,
            path: TypedAstNode list
        ) =
        let withPath n =
            n, TypedAstNode.FSharpImplementationFileDeclaration node :: path

        match node with
        | FSharpImplementationFileDeclaration.Entity(entity = entity; declarations = declarations) ->
            entity |> withPath |> this.VisitFSharpEntity

            declarations
            |> List.iter (withPath >> this.VisitFSharpImplementationFileDeclaration)
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(
            value = value; curriedArgs = curriedArgs; body = body) ->
            value |> withPath |> this.VisitFSharpMemberOrFunctionOrValue

            curriedArgs
            |> List.iter (List.iter (withPath >> this.VisitFSharpMemberOrFunctionOrValue))

            body |> withPath |> this.VisitFSharpExpr
        | FSharpImplementationFileDeclaration.InitAction action -> action |> withPath |> this.VisitFSharpExpr


    abstract member VisitFSharpMemberOrFunctionOrValue:
        node: FSharpMemberOrFunctionOrValue * path: TypedAstNode list -> unit

    default this.VisitFSharpMemberOrFunctionOrValue(node: FSharpMemberOrFunctionOrValue, path: TypedAstNode list) =
        let _withPath n =
            n, TypedAstNode.FSharpMemberOrFunctionOrValue node :: path

        () // TODO
