namespace Flinter.Ast

open FSharp.Compiler.Syntax


/// Used to track route during traversal AST
[<RequireQualifiedAccess>]
type UntypedAstNode =
    | ParsedHashDirective of ParsedHashDirective
    | ParsedImplFileInput of ParsedImplFileInput
    | ParsedInput of ParsedInput
    | SynArgInfo of SynArgInfo
    | SynArgPats of SynArgPats
    | SynAttribute of SynAttribute
    | SynAttributeList of SynAttributeList
    | SynBinding of SynBinding
    | SynBindingReturnInfo of SynBindingReturnInfo
    | SynComponentInfo of SynComponentInfo
    | SynConst of SynConst
    | SynEnumCase of SynEnumCase
    | SynExceptionDefn of SynExceptionDefn
    | SynExceptionDefnRepr of SynExceptionDefnRepr
    | SynExpr of SynExpr
    | SynExprAndBang of SynExprAndBang
    | SynExprRecordField of SynExprRecordField
    | SynField of SynField
    | SynInterfaceImpl of SynInterfaceImpl
    | SynInterpolatedStringPart of SynInterpolatedStringPart
    | SynMatchClause of SynMatchClause
    | SynMeasure of SynMeasure
    | SynMemberDefn of SynMemberDefn
    | SynMemberSig of SynMemberSig
    | SynModuleDecl of SynModuleDecl
    | SynModuleOrNamespace of SynModuleOrNamespace
    | SynOpenDeclTarget of SynOpenDeclTarget
    | SynPat of SynPat
    | SynSimplePats of SynSimplePats
    | SynSimplePat of SynSimplePat
    | SynTupleTypeSegment of SynTupleTypeSegment
    | SynTyparDecl of SynTyparDecl
    | SynTyparDecls of SynTyparDecls
    | SynType of SynType
    | SynTypeConstraint of SynTypeConstraint
    | SynTypeDefn of SynTypeDefn
    | SynTypeDefnKind of SynTypeDefnKind
    | SynTypeDefnRepr of SynTypeDefnRepr
    | SynTypeDefnSig of SynTypeDefnSig
    | SynTypeDefnSigRepr of SynTypeDefnSigRepr
    | SynTypeDefnSimpleRepr of SynTypeDefnSimpleRepr
    | SynUnionCase of SynUnionCase
    | SynUnionCaseKind of SynUnionCaseKind
    | SynValData of SynValData
    | SynValInfo of SynValInfo
    | SynValSig of SynValSig
    | SynValTyparDecls of SynValTyparDecls


type UntypedAstVisitor() =


    abstract member VisitParsedHashDirective: node: ParsedHashDirective * path: UntypedAstNode list -> unit

    default this.VisitParsedHashDirective(node: ParsedHashDirective, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.ParsedHashDirective node :: path

        match node with
        | ParsedHashDirective.ParsedHashDirective(args = args) ->
            args |> List.iter (withPath >> this.VisitParsedHashDirectiveArgument)


    abstract member VisitParsedHashDirectiveArgument:
        node: ParsedHashDirectiveArgument * path: UntypedAstNode list -> unit

    default this.VisitParsedHashDirectiveArgument(_node: ParsedHashDirectiveArgument, _path: UntypedAstNode list) = ()


    abstract member VisitParsedImplFileInput: node: ParsedImplFileInput * path: UntypedAstNode list -> unit

    default this.VisitParsedImplFileInput(node: ParsedImplFileInput, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.ParsedImplFileInput node :: path

        match node with
        | ParsedImplFileInput(contents = contents) -> contents |> List.iter (withPath >> this.VisitSynModuleOrNamespace)


    abstract member VisitParsedInput: node: ParsedInput * path: UntypedAstNode list -> unit

    default this.VisitParsedInput(node: ParsedInput, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.ParsedInput node :: path

        match node with
        | ParsedInput.ImplFile input -> input |> withPath |> this.VisitParsedImplFileInput
        // Ignore signature files, since the code they contain is a subset of the implementation files and therefore not
        // necessary to lint.
        | ParsedInput.SigFile _ -> ()


    abstract member VisitSynArgInfo: node: SynArgInfo * path: UntypedAstNode list -> unit

    default this.VisitSynArgInfo(node: SynArgInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynArgInfo node :: path

        match node with
        | SynArgInfo.SynArgInfo(attributes = attributes) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)


    abstract member VisitSynArgPats: node: SynArgPats * path: UntypedAstNode list -> unit

    default this.VisitSynArgPats(node: SynArgPats, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynArgPats node :: path

        match node with
        | SynArgPats.Pats(pats = pats) -> pats |> List.iter (withPath >> this.VisitSynPat)
        | SynArgPats.NamePatPairs(pats = pats) ->
            pats |> List.iter (fun (_, _, pat) -> pat |> withPath |> this.VisitSynPat)


    abstract member VisitSynAttribute: node: SynAttribute * path: UntypedAstNode list -> unit

    default this.VisitSynAttribute(node: SynAttribute, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynAttribute node :: path

        node.ArgExpr |> withPath |> this.VisitSynExpr


    abstract member VisitSynAttributeList: node: SynAttributeList * path: UntypedAstNode list -> unit

    default this.VisitSynAttributeList(node: SynAttributeList, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynAttributeList node :: path

        node.Attributes |> List.iter (withPath >> this.VisitSynAttribute)


    abstract member VisitSynBinding: node: SynBinding * path: UntypedAstNode list -> unit

    default this.VisitSynBinding(node: SynBinding, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynBinding node :: path

        match node with
        | SynBinding.SynBinding(
            attributes = attributes; valData = valData; headPat = headPat; expr = expr; returnInfo = returnInfo) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            valData |> withPath |> this.VisitSynValData
            headPat |> withPath |> this.VisitSynPat
            expr |> withPath |> this.VisitSynExpr
            returnInfo |> Option.iter (withPath >> this.VisitSynBindingReturnInfo)


    abstract member VisitSynBindingReturnInfo: node: SynBindingReturnInfo * path: UntypedAstNode list -> unit

    default this.VisitSynBindingReturnInfo(node: SynBindingReturnInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynBindingReturnInfo node :: path

        match node with
        | SynBindingReturnInfo.SynBindingReturnInfo(typeName = typeName; attributes = attributes) ->
            typeName |> withPath |> this.VisitSynType
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)


    abstract member VisitSynComponentInfo: node: SynComponentInfo * path: UntypedAstNode list -> unit

    default this.VisitSynComponentInfo(node: SynComponentInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynComponentInfo node :: path

        match node with
        | SynComponentInfo.SynComponentInfo(attributes = attributes; typeParams = typeParams; constraints = constraints) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            typeParams |> Option.iter (withPath >> this.VisitSynTyparDecls)
            constraints |> List.iter (withPath >> this.VisitSynTypeConstraint)


    abstract member VisitSynConst: node: SynConst * path: UntypedAstNode list -> unit

    default this.VisitSynConst(node: SynConst, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynConst node :: path

        match node with
        | SynConst.Unit -> ()
        | SynConst.Bool _ -> ()
        | SynConst.SByte _ -> ()
        | SynConst.Byte _ -> ()
        | SynConst.Int16 _ -> ()
        | SynConst.UInt16 _ -> ()
        | SynConst.Int32 _ -> ()
        | SynConst.UInt32 _ -> ()
        | SynConst.Int64 _ -> ()
        | SynConst.UInt64 _ -> ()
        | SynConst.IntPtr _ -> ()
        | SynConst.UIntPtr _ -> ()
        | SynConst.Single _ -> ()
        | SynConst.Double _ -> ()
        | SynConst.Char _ -> ()
        | SynConst.Decimal _ -> ()
        | SynConst.UserNum _ -> ()
        | SynConst.String _ -> ()
        | SynConst.Bytes _ -> ()
        | SynConst.UInt16s _ -> ()
        | SynConst.Measure(constant, _, measure) ->
            constant |> withPath |> this.VisitSynConst
            measure |> withPath |> this.VisitSynMeasure
        | SynConst.SourceIdentifier _ -> ()


    abstract member VisitSynEnumCase: node: SynEnumCase * path: UntypedAstNode list -> unit

    default this.VisitSynEnumCase(node: SynEnumCase, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynEnumCase node :: path

        match node with
        | SynEnumCase.SynEnumCase(attributes = attributes; value = value) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            value |> withPath |> this.VisitSynConst


    abstract member VisitSynExceptionDefn: node: SynExceptionDefn * path: UntypedAstNode list -> unit

    default this.VisitSynExceptionDefn(node: SynExceptionDefn, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExceptionDefn node :: path

        match node with
        | SynExceptionDefn(exnRepr = exnRepr; members = members) ->
            exnRepr |> withPath |> this.VisitSynExceptionDefnRepr
            members |> List.iter (withPath >> this.VisitSynMemberDefn)


    abstract member VisitSynExceptionDefnRepr: node: SynExceptionDefnRepr * path: UntypedAstNode list -> unit

    default this.VisitSynExceptionDefnRepr(node: SynExceptionDefnRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExceptionDefnRepr node :: path

        match node with
        | SynExceptionDefnRepr.SynExceptionDefnRepr(attributes = attributes; caseName = caseName) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            caseName |> withPath |> this.VisitSynUnionCase


    abstract member VisitSynExpr: node: SynExpr * path: UntypedAstNode list -> unit

    default this.VisitSynExpr(node: SynExpr, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynExpr node :: path

        match node with
        | SynExpr.Paren(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.Quote(operator = operator; quotedExpr = quotedExpr) ->
            operator |> withPath |> this.VisitSynExpr
            quotedExpr |> withPath |> this.VisitSynExpr

        | SynExpr.Const(constant = constant) -> constant |> withPath |> this.VisitSynConst

        | SynExpr.Typed(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.VisitSynExpr
            targetType |> withPath |> this.VisitSynType

        | SynExpr.Tuple(exprs = exprs) -> exprs |> List.iter (withPath >> this.VisitSynExpr)

        | SynExpr.AnonRecd(copyInfo = copyInfo; recordFields = recordFields) ->
            copyInfo |> Option.iter (fst >> withPath >> this.VisitSynExpr)

            recordFields
            |> List.iter (fun (_, _, expr) -> expr |> withPath |> this.VisitSynExpr)

        | SynExpr.ArrayOrList(exprs = exprs) -> exprs |> List.iter (withPath >> this.VisitSynExpr)

        | SynExpr.Record(baseInfo = baseInfo; copyInfo = copyInfo; recordFields = recordFields) ->
            baseInfo
            |> Option.iter (fun (ty, expr, _, _, _) ->
                ty |> withPath |> this.VisitSynType
                expr |> withPath |> this.VisitSynExpr)

            copyInfo |> Option.iter (fst >> withPath >> this.VisitSynExpr)
            recordFields |> List.iter (withPath >> this.VisitSynExprRecordField)

        | SynExpr.New(targetType = targetType; expr = expr) ->
            targetType |> withPath |> this.VisitSynType
            expr |> withPath |> this.VisitSynExpr

        | SynExpr.ObjExpr(
            objType = objType; argOptions = argOptions; bindings = bindings; members = members; extraImpls = extraImpls) ->
            objType |> withPath |> this.VisitSynType
            argOptions |> Option.iter (fst >> withPath >> this.VisitSynExpr)
            bindings |> List.iter (withPath >> this.VisitSynBinding)
            members |> List.iter (withPath >> this.VisitSynMemberDefn)
            extraImpls |> List.iter (withPath >> this.VisitSynInterfaceImpl)

        | SynExpr.While(whileExpr = whileExpr; doExpr = doExpr) ->
            whileExpr |> withPath |> this.VisitSynExpr
            doExpr |> withPath |> this.VisitSynExpr

        | SynExpr.For(identBody = identBody; toBody = toBody; doBody = doBody) ->
            identBody |> withPath |> this.VisitSynExpr
            toBody |> withPath |> this.VisitSynExpr
            doBody |> withPath |> this.VisitSynExpr

        | SynExpr.ForEach(pat = pat; enumExpr = enumExpr; bodyExpr = bodyExpr) ->
            pat |> withPath |> this.VisitSynPat
            enumExpr |> withPath |> this.VisitSynExpr
            bodyExpr |> withPath |> this.VisitSynExpr

        | SynExpr.ArrayOrListComputed(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.IndexRange(expr1 = expr1; expr2 = expr2) ->
            expr1 |> Option.iter (withPath >> this.VisitSynExpr)
            expr2 |> Option.iter (withPath >> this.VisitSynExpr)

        | SynExpr.IndexFromEnd(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.ComputationExpr(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.Lambda(parsedData = parsedData) ->
            // TODO: Can parsedData be None?
            // TODO: Should we ever use 'args'/'body' (i.e., after transformation from 'function' to 'fun v -> match v with')?
            //   https://stackoverflow.com/questions/74924482/in-synexpr-lambda-what-is-the-usage-of-args-body-vs-parseddata
            parsedData
            |> Option.iter (fun (args, body) ->
                args |> List.iter (withPath >> this.VisitSynPat)
                body |> withPath |> this.VisitSynExpr)

        | SynExpr.MatchLambda(matchClauses = matchClauses) ->
            matchClauses |> List.iter (withPath >> this.VisitSynMatchClause)

        | SynExpr.Match(expr = expr; clauses = clauses) ->
            expr |> withPath |> this.VisitSynExpr
            clauses |> List.iter (withPath >> this.VisitSynMatchClause)

        | SynExpr.Do(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.Assert(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
            if isInfix then
                // Reverse the expressions to match source order (not sure this is important in the context of Flinter)
                argExpr |> withPath |> this.VisitSynExpr
                funcExpr |> withPath |> this.VisitSynExpr
            else
                funcExpr |> withPath |> this.VisitSynExpr
                argExpr |> withPath |> this.VisitSynExpr

        | SynExpr.TypeApp(expr = expr; typeArgs = typeArgs) ->
            expr |> withPath |> this.VisitSynExpr
            typeArgs |> List.iter (withPath >> this.VisitSynType)

        | SynExpr.LetOrUse(bindings = bindings; body = body) ->
            bindings |> List.iter (withPath >> this.VisitSynBinding)
            body |> withPath |> this.VisitSynExpr

        | SynExpr.TryWith(tryExpr = tryExpr; withCases = withCases) ->
            tryExpr |> withPath |> this.VisitSynExpr
            withCases |> List.iter (withPath >> this.VisitSynMatchClause)

        | SynExpr.TryFinally(tryExpr = tryExpr; finallyExpr = finallyExpr) ->
            tryExpr |> withPath |> this.VisitSynExpr
            finallyExpr |> withPath |> this.VisitSynExpr

        | SynExpr.Lazy(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.Sequential(expr1 = expr1; expr2 = expr2) ->
            expr1 |> withPath |> this.VisitSynExpr
            expr2 |> withPath |> this.VisitSynExpr

        | SynExpr.IfThenElse(ifExpr = ifExpr; thenExpr = thenExpr; elseExpr = elseExpr) ->
            ifExpr |> withPath |> this.VisitSynExpr
            thenExpr |> withPath |> this.VisitSynExpr
            elseExpr |> Option.iter (withPath >> this.VisitSynExpr)

        | SynExpr.Typar(typar = typar) -> typar |> withPath |> this.VisitSynTypar

        | SynExpr.Ident _ -> ()

        | SynExpr.LongIdent _ -> ()

        | SynExpr.LongIdentSet(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.DotGet(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.DotSet(targetExpr = targetExpr; rhsExpr = rhsExpr) ->
            targetExpr |> withPath |> this.VisitSynExpr
            rhsExpr |> withPath |> this.VisitSynExpr

        | SynExpr.Set(targetExpr = targetExpr; rhsExpr = rhsExpr) ->
            targetExpr |> withPath |> this.VisitSynExpr
            rhsExpr |> withPath |> this.VisitSynExpr

        | SynExpr.DotIndexedGet(objectExpr = objectExpr; indexArgs = indexArgs) ->
            objectExpr |> withPath |> this.VisitSynExpr
            indexArgs |> withPath |> this.VisitSynExpr

        | SynExpr.DotIndexedSet(objectExpr = objectExpr; indexArgs = indexArgs; valueExpr = valueExpr) ->
            objectExpr |> withPath |> this.VisitSynExpr
            indexArgs |> withPath |> this.VisitSynExpr
            valueExpr |> withPath |> this.VisitSynExpr

        | SynExpr.NamedIndexedPropertySet(expr1 = expr1; expr2 = expr2) ->
            expr1 |> withPath |> this.VisitSynExpr
            expr2 |> withPath |> this.VisitSynExpr

        | SynExpr.DotNamedIndexedPropertySet(targetExpr = targetExpr; argExpr = argExpr; rhsExpr = rhsExpr) ->
            targetExpr |> withPath |> this.VisitSynExpr
            argExpr |> withPath |> this.VisitSynExpr
            rhsExpr |> withPath |> this.VisitSynExpr

        | SynExpr.TypeTest(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.VisitSynExpr
            targetType |> withPath |> this.VisitSynType

        | SynExpr.Upcast(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.VisitSynExpr
            targetType |> withPath |> this.VisitSynType

        | SynExpr.Downcast(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.VisitSynExpr
            targetType |> withPath |> this.VisitSynType

        | SynExpr.InferredUpcast(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.InferredDowncast(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.Null _ -> ()

        | SynExpr.AddressOf(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.TraitCall(supportTys = supportTys; traitSig = traitSig; argExpr = argExpr) ->
            supportTys |> List.iter (withPath >> this.VisitSynType)
            traitSig |> withPath |> this.VisitSynMemberSig
            argExpr |> withPath |> this.VisitSynExpr

        | SynExpr.JoinIn(lhsExpr = lhsExpr; rhsExpr = rhsExpr) ->
            lhsExpr |> withPath |> this.VisitSynExpr
            rhsExpr |> withPath |> this.VisitSynExpr

        | SynExpr.ImplicitZero _ -> ()

        // TODO: Should this be ignored?
        | SynExpr.SequentialOrImplicitYield(expr1 = expr1; expr2 = expr2; ifNotStmt = ifNotStmt) ->
            expr1 |> withPath |> this.VisitSynExpr
            expr2 |> withPath |> this.VisitSynExpr
            // TODO: What is this?
            ifNotStmt |> withPath |> this.VisitSynExpr

        | SynExpr.YieldOrReturn(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.YieldOrReturnFrom(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.LetOrUseBang(pat = pat; rhs = rhs; andBangs = andBangs; body = body) ->
            pat |> withPath |> this.VisitSynPat
            rhs |> withPath |> this.VisitSynExpr
            andBangs |> List.iter (withPath >> this.VisitSynExprAndBang)
            body |> withPath |> this.VisitSynExpr

        | SynExpr.MatchBang(expr = expr; clauses = clauses) ->
            expr |> withPath |> this.VisitSynExpr
            clauses |> List.iter (withPath >> this.VisitSynMatchClause)

        | SynExpr.DoBang(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.LibraryOnlyILAssembly _ -> ()

        | SynExpr.LibraryOnlyStaticOptimization _ -> ()

        | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> ()

        | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> ()

        | SynExpr.ArbitraryAfterError _ -> ()

        // TODO: Should this be ignored?
        | SynExpr.FromParseError(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        // TODO: Should this be ignored?
        | SynExpr.DiscardAfterMissingQualificationAfterDot(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.Fixed(expr = expr) -> expr |> withPath |> this.VisitSynExpr

        | SynExpr.InterpolatedString(contents = contents) ->
            contents |> List.iter (withPath >> this.VisitSynInterpolatedStringPart)

        // TODO: What is this? Should it be ignored?
        | SynExpr.DebugPoint(innerExpr = innerExpr) -> innerExpr |> withPath |> this.VisitSynExpr

        | SynExpr.Dynamic(funcExpr = funcExpr; argExpr = argExpr) ->
            funcExpr |> withPath |> this.VisitSynExpr
            argExpr |> withPath |> this.VisitSynExpr


    abstract member VisitSynExprAndBang: node: SynExprAndBang * path: UntypedAstNode list -> unit

    default this.VisitSynExprAndBang(node: SynExprAndBang, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExprAndBang node :: path

        match node with
        | SynExprAndBang.SynExprAndBang(pat = pat; body = body) ->
            pat |> withPath |> this.VisitSynPat
            body |> withPath |> this.VisitSynExpr


    abstract member VisitSynExprRecordField: node: SynExprRecordField * path: UntypedAstNode list -> unit

    default this.VisitSynExprRecordField(node: SynExprRecordField, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExprRecordField node :: path

        match node with
        | SynExprRecordField.SynExprRecordField(expr = expr) -> expr |> Option.iter (withPath >> this.VisitSynExpr)


    abstract member VisitSynField: node: SynField * path: UntypedAstNode list -> unit

    default this.VisitSynField(node: SynField, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynField node :: path

        match node with
        | SynField.SynField(attributes = attributes; fieldType = fieldType) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            fieldType |> withPath |> this.VisitSynType


    abstract member VisitSynInterfaceImpl: node: SynInterfaceImpl * path: UntypedAstNode list -> unit

    default this.VisitSynInterfaceImpl(node: SynInterfaceImpl, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynInterfaceImpl node :: path

        match node with
        | SynInterfaceImpl.SynInterfaceImpl(interfaceTy = interfaceTy; bindings = bindings; members = members) ->
            interfaceTy |> withPath |> this.VisitSynType
            bindings |> List.iter (withPath >> this.VisitSynBinding)
            members |> List.iter (withPath >> this.VisitSynMemberDefn)


    abstract member VisitSynInterpolatedStringPart: node: SynInterpolatedStringPart * path: UntypedAstNode list -> unit

    default this.VisitSynInterpolatedStringPart(node: SynInterpolatedStringPart, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynInterpolatedStringPart node :: path

        match node with
        | SynInterpolatedStringPart.String _ -> ()
        | SynInterpolatedStringPart.FillExpr(fillExpr = fillExpr) -> fillExpr |> withPath |> this.VisitSynExpr


    abstract member VisitSynMatchClause: node: SynMatchClause * path: UntypedAstNode list -> unit

    default this.VisitSynMatchClause(node: SynMatchClause, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMatchClause node :: path

        match node with
        | SynMatchClause.SynMatchClause(pat = pat; whenExpr = whenExpr; resultExpr = resultExpr) ->
            pat |> withPath |> this.VisitSynPat
            whenExpr |> Option.iter (withPath >> this.VisitSynExpr)
            resultExpr |> withPath |> this.VisitSynExpr


    abstract member VisitSynMeasure: node: SynMeasure * path: UntypedAstNode list -> unit

    default this.VisitSynMeasure(node: SynMeasure, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMeasure node :: path

        match node with
        | SynMeasure.Named _ -> ()
        | SynMeasure.Product(measure1 = measure1; measure2 = measure2) ->
            measure1 |> withPath |> this.VisitSynMeasure
            measure2 |> withPath |> this.VisitSynMeasure
        | SynMeasure.Seq(measures = measures) -> measures |> List.iter (withPath >> this.VisitSynMeasure)
        | SynMeasure.Divide(measure1 = measure1; measure2 = measure2) ->
            measure1 |> withPath |> this.VisitSynMeasure
            measure2 |> withPath |> this.VisitSynMeasure
        | SynMeasure.Power(measure = measure) -> measure |> withPath |> this.VisitSynMeasure
        | SynMeasure.One -> ()
        | SynMeasure.Var(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynMeasure.Anon _ -> ()
        | SynMeasure.Paren(measure = measure) -> measure |> withPath |> this.VisitSynMeasure


    abstract member VisitSynMemberDefn: node: SynMemberDefn * path: UntypedAstNode list -> unit

    default this.VisitSynMemberDefn(node: SynMemberDefn, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMemberDefn node :: path

        match node with
        | SynMemberDefn.Open(target = target) -> target |> withPath |> this.VisitSynOpenDeclTarget
        | SynMemberDefn.Member(memberDefn = memberDefn) -> memberDefn |> withPath |> this.VisitSynBinding
        | SynMemberDefn.GetSetMember(memberDefnForGet = memberDefnForGet; memberDefnForSet = memberDefnForSet) ->
            memberDefnForGet |> Option.iter (withPath >> this.VisitSynBinding)
            memberDefnForSet |> Option.iter (withPath >> this.VisitSynBinding)
        // TODO: Should this be ignored?
        | SynMemberDefn.ImplicitCtor(attributes = attributes; ctorArgs = ctorArgs) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            ctorArgs |> withPath |> this.VisitSynSimplePats
        | SynMemberDefn.ImplicitInherit(inheritType = inheritType; inheritArgs = inheritArgs) ->
            inheritType |> withPath |> this.VisitSynType
            inheritArgs |> withPath |> this.VisitSynExpr
        | SynMemberDefn.LetBindings(bindings = bindings) -> bindings |> List.iter (withPath >> this.VisitSynBinding)
        | SynMemberDefn.AbstractSlot(slotSig = slotSig) -> slotSig |> withPath |> this.VisitSynValSig
        | SynMemberDefn.Interface(interfaceType = interfaceType; members = members) ->
            interfaceType |> withPath |> this.VisitSynType
            members |> Option.iter (List.iter (withPath >> this.VisitSynMemberDefn))
        | SynMemberDefn.Inherit(baseType = baseType) -> baseType |> withPath |> this.VisitSynType
        | SynMemberDefn.ValField(fieldInfo = fieldInfo) -> fieldInfo |> withPath |> this.VisitSynField
        | SynMemberDefn.NestedType(typeDefn = typeDefn) -> typeDefn |> withPath |> this.VisitSynTypeDefn
        | SynMemberDefn.AutoProperty(attributes = attributes; typeOpt = typeOpt; synExpr = synExpr) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            typeOpt |> Option.iter (withPath >> this.VisitSynType)
            synExpr |> withPath |> this.VisitSynExpr


    abstract member VisitSynMemberSig: node: SynMemberSig * path: UntypedAstNode list -> unit

    default this.VisitSynMemberSig(node: SynMemberSig, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMemberSig node :: path

        match node with
        | SynMemberSig.Member(memberSig = memberSig) -> memberSig |> withPath |> this.VisitSynValSig
        | SynMemberSig.Interface(interfaceType = interfaceType) -> interfaceType |> withPath |> this.VisitSynType
        | SynMemberSig.Inherit(inheritedType = inheritedType) -> inheritedType |> withPath |> this.VisitSynType
        | SynMemberSig.ValField(field = field) -> field |> withPath |> this.VisitSynField
        | SynMemberSig.NestedType(nestedType = nestedType) -> nestedType |> withPath |> this.VisitSynTypeDefnSig


    abstract member VisitSynModuleDecl: node: SynModuleDecl * path: UntypedAstNode list -> unit

    default this.VisitSynModuleDecl(node: SynModuleDecl, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynModuleDecl node :: path

        match node with
        | SynModuleDecl.Attributes(attributes = attributes) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
        | SynModuleDecl.Exception(exnDefn = exnDefn) -> exnDefn |> withPath |> this.VisitSynExceptionDefn
        | SynModuleDecl.Expr(expr = expr) -> expr |> withPath |> this.VisitSynExpr
        | SynModuleDecl.HashDirective(hashDirective = hashDirective) ->
            hashDirective |> withPath |> this.VisitParsedHashDirective
        | SynModuleDecl.Let(bindings = bindings) -> bindings |> List.iter (withPath >> this.VisitSynBinding)
        | SynModuleDecl.ModuleAbbrev _ -> ()
        | SynModuleDecl.NamespaceFragment(fragment = fragment) -> fragment |> withPath |> this.VisitSynModuleOrNamespace
        | SynModuleDecl.NestedModule(moduleInfo = moduleInfo; decls = decls) ->
            moduleInfo |> withPath |> this.VisitSynComponentInfo
            decls |> List.iter (withPath >> this.VisitSynModuleDecl)
        | SynModuleDecl.Open(target = target) -> target |> withPath |> this.VisitSynOpenDeclTarget
        | SynModuleDecl.Types(typeDefns = typeDefns) -> typeDefns |> List.iter (withPath >> this.VisitSynTypeDefn)


    abstract member VisitSynModuleOrNamespace: node: SynModuleOrNamespace * path: UntypedAstNode list -> unit

    default this.VisitSynModuleOrNamespace(node: SynModuleOrNamespace, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynModuleOrNamespace node :: path

        match node with
        | SynModuleOrNamespace.SynModuleOrNamespace(decls = decls; attribs = attribs) ->
            attribs |> List.iter (withPath >> this.VisitSynAttributeList)
            decls |> List.iter (withPath >> this.VisitSynModuleDecl)


    abstract member VisitSynOpenDeclTarget: node: SynOpenDeclTarget * path: UntypedAstNode list -> unit

    default this.VisitSynOpenDeclTarget(node: SynOpenDeclTarget, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynOpenDeclTarget node :: path

        match node with
        | SynOpenDeclTarget.ModuleOrNamespace _ -> ()
        | SynOpenDeclTarget.Type(typeName = typeName) -> typeName |> withPath |> this.VisitSynType


    abstract member VisitSynPat: node: SynPat * path: UntypedAstNode list -> unit

    default this.VisitSynPat(node: SynPat, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynPat node :: path

        match node with
        | SynPat.Const(constant = constant) -> constant |> withPath |> this.VisitSynConst
        | SynPat.Wild _ -> ()
        | SynPat.Named _ -> ()
        | SynPat.Typed(pat = pat; targetType = targetType) ->
            pat |> withPath |> this.VisitSynPat
            targetType |> withPath |> this.VisitSynType
        | SynPat.Attrib(pat = pat; attributes = attributes) ->
            pat |> withPath |> this.VisitSynPat
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
        | SynPat.Or(lhsPat = lhsPat; rhsPat = rhsPat) ->
            lhsPat |> withPath |> this.VisitSynPat
            rhsPat |> withPath |> this.VisitSynPat
        | SynPat.Ands(pats = pats) -> pats |> List.iter (withPath >> this.VisitSynPat)
        | SynPat.As(lhsPat = lhsPat; rhsPat = rhsPat) ->
            lhsPat |> withPath |> this.VisitSynPat
            rhsPat |> withPath |> this.VisitSynPat
        | SynPat.LongIdent(typarDecls = typarDecls; argPats = argPats) ->
            typarDecls |> Option.iter (withPath >> this.VisitSynValTyparDecls)
            argPats |> withPath |> this.VisitSynArgPats
        | SynPat.Tuple(elementPats = elementPats) -> elementPats |> List.iter (withPath >> this.VisitSynPat)
        | SynPat.Paren(pat = pat) -> pat |> withPath |> this.VisitSynPat
        | SynPat.ArrayOrList(elementPats = elementPats) -> elementPats |> List.iter (withPath >> this.VisitSynPat)
        | SynPat.Record(fieldPats = fieldPats) ->
            fieldPats |> List.iter (fun (_, _, pat) -> pat |> withPath |> this.VisitSynPat)
        | SynPat.Null _ -> ()
        | SynPat.OptionalVal _ -> ()
        | SynPat.IsInst(pat = pat) -> pat |> withPath |> this.VisitSynType
        | SynPat.QuoteExpr(expr = expr) -> expr |> withPath |> this.VisitSynExpr
        | SynPat.DeprecatedCharRange _ -> ()
        | SynPat.InstanceMember _ -> ()
        | SynPat.FromParseError _ -> ()


    abstract member VisitSynSimplePat: node: SynSimplePat * path: UntypedAstNode list -> unit

    default this.VisitSynSimplePat(node: SynSimplePat, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynSimplePat node :: path

        match node with
        | SynSimplePat.Id _ -> ()
        | SynSimplePat.Typed(pat = pat; targetType = targetType) ->
            pat |> withPath |> this.VisitSynSimplePat
            targetType |> withPath |> this.VisitSynType
        | SynSimplePat.Attrib(pat = pat; attributes = attributes) ->
            pat |> withPath |> this.VisitSynSimplePat
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)


    abstract member VisitSynSimplePats: node: SynSimplePats * path: UntypedAstNode list -> unit

    default this.VisitSynSimplePats(node: SynSimplePats, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynSimplePats node :: path

        match node with
        | SynSimplePats.SimplePats(pats = pats) -> pats |> List.iter (withPath >> this.VisitSynSimplePat)
        | SynSimplePats.Typed(pats = pats; targetType = targetType) ->
            pats |> withPath |> this.VisitSynSimplePats
            targetType |> withPath |> this.VisitSynType


    abstract member VisitSynTupleTypeSegment: node: SynTupleTypeSegment * path: UntypedAstNode list -> unit

    default this.VisitSynTupleTypeSegment(node: SynTupleTypeSegment, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTupleTypeSegment node :: path

        match node with
        | SynTupleTypeSegment.Type(typeName = typeName) -> typeName |> withPath |> this.VisitSynType
        | SynTupleTypeSegment.Star _ -> ()
        | SynTupleTypeSegment.Slash _ -> ()


    abstract member VisitSynTypar: node: SynTypar * path: UntypedAstNode list -> unit

    default this.VisitSynTypar(_node: SynTypar, _path: UntypedAstNode list) = ()


    abstract member VisitSynTyparDecl: node: SynTyparDecl * path: UntypedAstNode list -> unit

    default this.VisitSynTyparDecl(node: SynTyparDecl, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTyparDecl node :: path

        match node with
        | SynTyparDecl.SynTyparDecl(attributes = attributes) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)


    abstract member VisitSynTyparDecls: node: SynTyparDecls * path: UntypedAstNode list -> unit

    default this.VisitSynTyparDecls(node: SynTyparDecls, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTyparDecls node :: path

        match node with
        | SynTyparDecls.PostfixList(decls = decls; constraints = constraints) ->
            decls |> List.iter (withPath >> this.VisitSynTyparDecl)
            constraints |> List.iter (withPath >> this.VisitSynTypeConstraint)
        | SynTyparDecls.PrefixList(decls = decls) -> decls |> List.iter (withPath >> this.VisitSynTyparDecl)
        | SynTyparDecls.SinglePrefix(decl = decl) -> decl |> withPath |> this.VisitSynTyparDecl


    abstract member VisitSynType: node: SynType * path: UntypedAstNode list -> unit

    default this.VisitSynType(node: SynType, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynType node :: path

        match node with
        | SynType.LongIdent _ -> ()
        | SynType.App(typeName = typeName; typeArgs = typeArgs) ->
            typeName |> withPath |> this.VisitSynType
            typeArgs |> List.iter (withPath >> this.VisitSynType)
        | SynType.LongIdentApp(typeName = typeName; typeArgs = typeArgs) ->
            typeName |> withPath |> this.VisitSynType
            typeArgs |> List.iter (withPath >> this.VisitSynType)
        | SynType.Tuple(path = path) -> path |> List.iter (withPath >> this.VisitSynTupleTypeSegment)
        | SynType.AnonRecd(fields = fields) -> fields |> List.iter (snd >> withPath >> this.VisitSynType)
        | SynType.Array(elementType = elementType) -> elementType |> withPath |> this.VisitSynType
        | SynType.Fun(argType = argType; returnType = returnType) ->
            argType |> withPath |> this.VisitSynType
            returnType |> withPath |> this.VisitSynType
        | SynType.Var(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynType.Anon _ -> ()
        | SynType.WithGlobalConstraints(typeName = typeName; constraints = constraints) ->
            typeName |> withPath |> this.VisitSynType
            constraints |> List.iter (withPath >> this.VisitSynTypeConstraint)
        | SynType.HashConstraint(innerType = innerType) -> innerType |> withPath |> this.VisitSynType
        | SynType.MeasurePower(baseMeasure = baseMeasure) -> baseMeasure |> withPath |> this.VisitSynType
        | SynType.MeasureDivide(dividend = dividend; divisor = divisor) ->
            dividend |> withPath |> this.VisitSynType
            divisor |> withPath |> this.VisitSynType
        | SynType.StaticConstant(constant = constant) -> constant |> withPath |> this.VisitSynConst
        | SynType.StaticConstantExpr(expr = expr) -> expr |> withPath |> this.VisitSynExpr
        | SynType.StaticConstantNamed(ident = ident; value = value) ->
            ident |> withPath |> this.VisitSynType
            value |> withPath |> this.VisitSynType
        | SynType.Paren(innerType = innerType) -> innerType |> withPath |> this.VisitSynType
        | SynType.SignatureParameter(attributes = attributes; usedType = usedType) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            usedType |> withPath |> this.VisitSynType


    abstract member VisitSynTypeConstraint: node: SynTypeConstraint * path: UntypedAstNode list -> unit

    default this.VisitSynTypeConstraint(node: SynTypeConstraint, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeConstraint node :: path

        match node with
        | SynTypeConstraint.WhereTyparIsValueType(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynTypeConstraint.WhereTyparIsReferenceType(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynTypeConstraint.WhereTyparIsUnmanaged(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynTypeConstraint.WhereTyparSupportsNull(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynTypeConstraint.WhereTyparIsComparable(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynTypeConstraint.WhereTyparIsEquatable(typar = typar) -> typar |> withPath |> this.VisitSynTypar
        | SynTypeConstraint.WhereTyparDefaultsToType(typar = typar; typeName = typeName) ->
            typar |> withPath |> this.VisitSynTypar
            typeName |> withPath |> this.VisitSynType
        | SynTypeConstraint.WhereTyparSubtypeOfType(typar = typar; typeName = typeName) ->
            typar |> withPath |> this.VisitSynTypar
            typeName |> withPath |> this.VisitSynType
        | SynTypeConstraint.WhereTyparSupportsMember(typars = typars; memberSig = memberSig) ->
            typars |> List.iter (withPath >> this.VisitSynType)
            memberSig |> withPath |> this.VisitSynMemberSig
        | SynTypeConstraint.WhereTyparIsEnum(typar = typar; typeArgs = typeArgs) ->
            typar |> withPath |> this.VisitSynTypar
            typeArgs |> List.iter (withPath >> this.VisitSynType)
        | SynTypeConstraint.WhereTyparIsDelegate(typar = typar; typeArgs = typeArgs) ->
            typar |> withPath |> this.VisitSynTypar
            typeArgs |> List.iter (withPath >> this.VisitSynType)
        | SynTypeConstraint.WhereSelfConstrained(selfConstraint = selfConstraint) ->
            selfConstraint |> withPath |> this.VisitSynType


    abstract member VisitSynTypeDefn: node: SynTypeDefn * path: UntypedAstNode list -> unit

    default this.VisitSynTypeDefn(node: SynTypeDefn, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefn node :: path

        match node with
        | SynTypeDefn.SynTypeDefn(
            typeInfo = typeInfo; typeRepr = typeRepr; members = members; implicitConstructor = implicitConstructor) ->
            typeInfo |> withPath |> this.VisitSynComponentInfo
            typeRepr |> withPath |> this.VisitSynTypeDefnRepr
            members |> List.iter (withPath >> this.VisitSynMemberDefn)
            implicitConstructor |> Option.iter (withPath >> this.VisitSynMemberDefn)


    abstract member VisitSynTypeDefnKind: node: SynTypeDefnKind * path: UntypedAstNode list -> unit

    default this.VisitSynTypeDefnKind(node: SynTypeDefnKind, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnKind node :: path

        match node with
        | SynTypeDefnKind.Unspecified -> ()
        | SynTypeDefnKind.Class -> ()
        | SynTypeDefnKind.Interface -> ()
        | SynTypeDefnKind.Struct -> ()
        | SynTypeDefnKind.Record -> ()
        | SynTypeDefnKind.Union -> ()
        | SynTypeDefnKind.Abbrev -> ()
        | SynTypeDefnKind.Opaque -> ()
        | SynTypeDefnKind.Augmentation _ -> ()
        | SynTypeDefnKind.IL -> ()
        | SynTypeDefnKind.Delegate(signature = signature; signatureInfo = signatureInfo) ->
            signature |> withPath |> this.VisitSynType
            signatureInfo |> withPath |> this.VisitSynValInfo


    abstract member VisitSynTypeDefnRepr: node: SynTypeDefnRepr * path: UntypedAstNode list -> unit

    default this.VisitSynTypeDefnRepr(node: SynTypeDefnRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnRepr node :: path

        match node with
        | SynTypeDefnRepr.ObjectModel(kind = kind; members = members) ->
            kind |> withPath |> this.VisitSynTypeDefnKind
            members |> List.iter (withPath >> this.VisitSynMemberDefn)
        | SynTypeDefnRepr.Simple(simpleRepr = simpleRepr) -> simpleRepr |> withPath |> this.VisitSynTypeDefnSimpleRepr
        | SynTypeDefnRepr.Exception(exnRepr = exnRepr) -> exnRepr |> withPath |> this.VisitSynExceptionDefnRepr


    abstract member VisitSynTypeDefnSig: node: SynTypeDefnSig * path: UntypedAstNode list -> unit

    default this.VisitSynTypeDefnSig(node: SynTypeDefnSig, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnSig node :: path

        match node with
        | SynTypeDefnSig.SynTypeDefnSig(typeInfo = typeInfo; typeRepr = typeRepr; members = members) ->
            typeInfo |> withPath |> this.VisitSynComponentInfo
            typeRepr |> withPath |> this.VisitSynTypeDefnSigRepr
            members |> List.iter (withPath >> this.VisitSynMemberSig)


    abstract member VisitSynTypeDefnSigRepr: node: SynTypeDefnSigRepr * path: UntypedAstNode list -> unit

    default this.VisitSynTypeDefnSigRepr(node: SynTypeDefnSigRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnSigRepr node :: path

        match node with
        | SynTypeDefnSigRepr.ObjectModel(kind = kind; memberSigs = memberSigs) ->
            kind |> withPath |> this.VisitSynTypeDefnKind
            memberSigs |> List.iter (withPath >> this.VisitSynMemberSig)
        | SynTypeDefnSigRepr.Simple(repr = repr) -> repr |> withPath |> this.VisitSynTypeDefnSimpleRepr
        | SynTypeDefnSigRepr.Exception(repr = repr) -> repr |> withPath |> this.VisitSynExceptionDefnRepr


    abstract member VisitSynTypeDefnSimpleRepr: node: SynTypeDefnSimpleRepr * path: UntypedAstNode list -> unit

    default this.VisitSynTypeDefnSimpleRepr(node: SynTypeDefnSimpleRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnSimpleRepr node :: path

        match node with
        | SynTypeDefnSimpleRepr.Union(unionCases = unionCases) ->
            unionCases |> List.iter (withPath >> this.VisitSynUnionCase)
        | SynTypeDefnSimpleRepr.Enum(cases = cases) -> cases |> List.iter (withPath >> this.VisitSynEnumCase)
        | SynTypeDefnSimpleRepr.Record(recordFields = recordFields) ->
            recordFields |> List.iter (withPath >> this.VisitSynField)
        | SynTypeDefnSimpleRepr.General(kind = kind; inherits = inherits; slotsigs = slotsigs; fields = fields) ->
            kind |> withPath |> this.VisitSynTypeDefnKind
            inherits |> List.iter (fun (t, _, _) -> t |> withPath |> this.VisitSynType)
            // TODO: Use 'slotsigs'? Docs says 'this is not a parse-tree form'
            slotsigs |> List.iter (fst >> withPath >> this.VisitSynValSig)
            fields |> List.iter (withPath >> this.VisitSynField)
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> ()
        | SynTypeDefnSimpleRepr.TypeAbbrev(detail = detail; rhsType = rhsType) ->
            match detail with
            | ParserDetail.Ok -> rhsType |> withPath |> this.VisitSynType
            | ParserDetail.ErrorRecovery -> ()
        | SynTypeDefnSimpleRepr.None _ -> ()
        | SynTypeDefnSimpleRepr.Exception(exnRepr = exnRepr) -> exnRepr |> withPath |> this.VisitSynExceptionDefnRepr


    abstract member VisitSynUnionCase: node: SynUnionCase * path: UntypedAstNode list -> unit

    default this.VisitSynUnionCase(node: SynUnionCase, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynUnionCase node :: path

        match node with
        | SynUnionCase.SynUnionCase(attributes = attributes; caseType = caseType) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            caseType |> withPath |> this.VisitSynUnionCaseKind


    abstract member VisitSynUnionCaseKind: node: SynUnionCaseKind * path: UntypedAstNode list -> unit

    default this.VisitSynUnionCaseKind(node: SynUnionCaseKind, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynUnionCaseKind node :: path

        match node with
        | SynUnionCaseKind.Fields(cases = cases) -> cases |> List.iter (withPath >> this.VisitSynField)
        | SynUnionCaseKind.FullType(fullType = fullType; fullTypeInfo = fullTypeInfo) ->
            fullType |> withPath |> this.VisitSynType
            fullTypeInfo |> withPath |> this.VisitSynValInfo


    abstract member VisitSynValData: node: SynValData * path: UntypedAstNode list -> unit

    default this.VisitSynValData(node: SynValData, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValData node :: path

        match node with
        | SynValData.SynValData(valInfo = valInfo) -> valInfo |> withPath |> this.VisitSynValInfo


    abstract member VisitSynValInfo: node: SynValInfo * path: UntypedAstNode list -> unit

    default this.VisitSynValInfo(node: SynValInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValInfo node :: path

        match node with
        | SynValInfo.SynValInfo(curriedArgInfos = curriedArgInfos) ->
            curriedArgInfos |> List.iter (List.iter (withPath >> this.VisitSynArgInfo))


    abstract member VisitSynValSig: node: SynValSig * path: UntypedAstNode list -> unit

    default this.VisitSynValSig(node: SynValSig, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValSig node :: path

        match node with
        | SynValSig.SynValSig(
            attributes = attributes
            explicitTypeParams = explicitTypeParams
            synType = synType
            arity = arity
            synExpr = synExpr) ->
            attributes |> List.iter (withPath >> this.VisitSynAttributeList)
            explicitTypeParams |> withPath |> this.VisitSynValTyparDecls
            synType |> withPath |> this.VisitSynType
            arity |> withPath |> this.VisitSynValInfo
            synExpr |> Option.iter (withPath >> this.VisitSynExpr)


    abstract member VisitSynValTyparDecls: node: SynValTyparDecls * path: UntypedAstNode list -> unit

    default this.VisitSynValTyparDecls(node: SynValTyparDecls, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValTyparDecls node :: path

        match node with
        | SynValTyparDecls.SynValTyparDecls(typars = typars) ->
            typars |> Option.iter (withPath >> this.VisitSynTyparDecls)
