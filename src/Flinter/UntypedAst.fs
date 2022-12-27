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


    abstract member Visit: node: ParsedHashDirective * path: UntypedAstNode list -> unit

    default this.Visit(node: ParsedHashDirective, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.ParsedHashDirective node :: path

        match node with
        | ParsedHashDirective.ParsedHashDirective(args = args) -> args |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: ParsedHashDirectiveArgument * path: UntypedAstNode list -> unit

    default this.Visit(_node: ParsedHashDirectiveArgument, _path: UntypedAstNode list) = ()


    abstract member Visit: node: ParsedImplFileInput * path: UntypedAstNode list -> unit

    default this.Visit(node: ParsedImplFileInput, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.ParsedImplFileInput node :: path

        match node with
        | ParsedImplFileInput(contents = contents) -> contents |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: ParsedInput * path: UntypedAstNode list -> unit

    default this.Visit(node: ParsedInput, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.ParsedInput node :: path

        match node with
        | ParsedInput.ImplFile input -> input |> withPath |> this.Visit
        // Ignore signature files, since the code they contain is a subset of the implementation files and therefore not
        // necessary to lint.
        | ParsedInput.SigFile _ -> ()


    abstract member Visit: node: SynArgInfo * path: UntypedAstNode list -> unit

    default this.Visit(node: SynArgInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynArgInfo node :: path

        match node with
        | SynArgInfo.SynArgInfo(attributes = attributes) -> attributes |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynArgPats * path: UntypedAstNode list -> unit

    default this.Visit(node: SynArgPats, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynArgPats node :: path

        match node with
        | SynArgPats.Pats(pats = pats) -> pats |> List.iter (withPath >> this.Visit)
        | SynArgPats.NamePatPairs(pats = pats) -> pats |> List.iter (fun (_, _, pat) -> pat |> withPath |> this.Visit)


    abstract member Visit: node: SynAttribute * path: UntypedAstNode list -> unit

    default this.Visit(node: SynAttribute, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynAttribute node :: path

        node.ArgExpr |> withPath |> this.Visit


    abstract member Visit: node: SynAttributeList * path: UntypedAstNode list -> unit

    default this.Visit(node: SynAttributeList, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynAttributeList node :: path

        node.Attributes |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynBinding * path: UntypedAstNode list -> unit

    default this.Visit(node: SynBinding, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynBinding node :: path

        match node with
        | SynBinding.SynBinding(
            attributes = attributes; valData = valData; headPat = headPat; expr = expr; returnInfo = returnInfo) ->
            attributes |> List.iter (withPath >> this.Visit)
            valData |> withPath |> this.Visit
            headPat |> withPath |> this.Visit
            expr |> withPath |> this.Visit
            returnInfo |> Option.iter (withPath >> this.Visit)


    abstract member Visit: node: SynBindingReturnInfo * path: UntypedAstNode list -> unit

    default this.Visit(node: SynBindingReturnInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynBindingReturnInfo node :: path

        match node with
        | SynBindingReturnInfo.SynBindingReturnInfo(typeName = typeName; attributes = attributes) ->
            typeName |> withPath |> this.Visit
            attributes |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynComponentInfo * path: UntypedAstNode list -> unit

    default this.Visit(node: SynComponentInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynComponentInfo node :: path

        match node with
        | SynComponentInfo.SynComponentInfo(attributes = attributes; typeParams = typeParams; constraints = constraints) ->
            attributes |> List.iter (withPath >> this.Visit)
            typeParams |> Option.iter (withPath >> this.Visit)
            constraints |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynConst * path: UntypedAstNode list -> unit

    default this.Visit(node: SynConst, path: UntypedAstNode list) =
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
            constant |> withPath |> this.Visit
            measure |> withPath |> this.Visit
        | SynConst.SourceIdentifier _ -> ()


    abstract member Visit: node: SynEnumCase * path: UntypedAstNode list -> unit

    default this.Visit(node: SynEnumCase, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynEnumCase node :: path

        match node with
        | SynEnumCase.SynEnumCase(attributes = attributes; value = value) ->
            attributes |> List.iter (withPath >> this.Visit)
            value |> withPath |> this.Visit


    abstract member Visit: node: SynExceptionDefn * path: UntypedAstNode list -> unit

    default this.Visit(node: SynExceptionDefn, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExceptionDefn node :: path

        match node with
        | SynExceptionDefn(exnRepr = exnRepr; members = members) ->
            exnRepr |> withPath |> this.Visit
            members |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynExceptionDefnRepr * path: UntypedAstNode list -> unit

    default this.Visit(node: SynExceptionDefnRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExceptionDefnRepr node :: path

        match node with
        | SynExceptionDefnRepr.SynExceptionDefnRepr(attributes = attributes; caseName = caseName) ->
            attributes |> List.iter (withPath >> this.Visit)
            caseName |> withPath |> this.Visit


    abstract member Visit: node: SynExpr * path: UntypedAstNode list -> unit

    default this.Visit(node: SynExpr, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynExpr node :: path

        match node with
        | SynExpr.Paren(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.Quote(operator = operator; quotedExpr = quotedExpr) ->
            operator |> withPath |> this.Visit
            quotedExpr |> withPath |> this.Visit

        | SynExpr.Const(constant = constant) -> constant |> withPath |> this.Visit

        | SynExpr.Typed(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.Visit
            targetType |> withPath |> this.Visit

        | SynExpr.Tuple(exprs = exprs) -> exprs |> List.iter (withPath >> this.Visit)

        | SynExpr.AnonRecd(copyInfo = copyInfo; recordFields = recordFields) ->
            copyInfo |> Option.iter (fst >> withPath >> this.Visit)
            recordFields |> List.iter (fun (_, _, expr) -> expr |> withPath |> this.Visit)

        | SynExpr.ArrayOrList(exprs = exprs) -> exprs |> List.iter (withPath >> this.Visit)

        | SynExpr.Record(baseInfo = baseInfo; copyInfo = copyInfo; recordFields = recordFields) ->
            baseInfo
            |> Option.iter (fun (ty, expr, _, _, _) ->
                ty |> withPath |> this.Visit
                expr |> withPath |> this.Visit)

            copyInfo |> Option.iter (fst >> withPath >> this.Visit)
            recordFields |> List.iter (withPath >> this.Visit)

        | SynExpr.New(targetType = targetType; expr = expr) ->
            targetType |> withPath |> this.Visit
            expr |> withPath |> this.Visit

        | SynExpr.ObjExpr(
            objType = objType; argOptions = argOptions; bindings = bindings; members = members; extraImpls = extraImpls) ->
            objType |> withPath |> this.Visit
            argOptions |> Option.iter (fst >> withPath >> this.Visit)
            bindings |> List.iter (withPath >> this.Visit)
            members |> List.iter (withPath >> this.Visit)
            extraImpls |> List.iter (withPath >> this.Visit)

        | SynExpr.While(whileExpr = whileExpr; doExpr = doExpr) ->
            whileExpr |> withPath |> this.Visit
            doExpr |> withPath |> this.Visit

        | SynExpr.For(identBody = identBody; toBody = toBody; doBody = doBody) ->
            identBody |> withPath |> this.Visit
            toBody |> withPath |> this.Visit
            doBody |> withPath |> this.Visit

        | SynExpr.ForEach(pat = pat; enumExpr = enumExpr; bodyExpr = bodyExpr) ->
            pat |> withPath |> this.Visit
            enumExpr |> withPath |> this.Visit
            bodyExpr |> withPath |> this.Visit

        | SynExpr.ArrayOrListComputed(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.IndexRange(expr1 = expr1; expr2 = expr2) ->
            expr1 |> Option.iter (withPath >> this.Visit)
            expr2 |> Option.iter (withPath >> this.Visit)

        | SynExpr.IndexFromEnd(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.ComputationExpr(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.Lambda(parsedData = parsedData) ->
            // TODO: Can parsedData be None?
            // TODO: Should we ever use 'args'/'body' (i.e., after transformation from 'function' to 'fun v -> match v with')?
            //   https://stackoverflow.com/questions/74924482/in-synexpr-lambda-what-is-the-usage-of-args-body-vs-parseddata
            parsedData
            |> Option.iter (fun (args, body) ->
                args |> List.iter (withPath >> this.Visit)
                body |> withPath |> this.Visit)

        | SynExpr.MatchLambda(matchClauses = matchClauses) -> matchClauses |> List.iter (withPath >> this.Visit)

        | SynExpr.Match(expr = expr; clauses = clauses) ->
            expr |> withPath |> this.Visit
            clauses |> List.iter (withPath >> this.Visit)

        | SynExpr.Do(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.Assert(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
            if isInfix then
                // Reverse the expressions to match source order (not sure this is important in the context of Flinter)
                argExpr |> withPath |> this.Visit
                funcExpr |> withPath |> this.Visit
            else
                funcExpr |> withPath |> this.Visit
                argExpr |> withPath |> this.Visit

        | SynExpr.TypeApp(expr = expr; typeArgs = typeArgs) ->
            expr |> withPath |> this.Visit
            typeArgs |> List.iter (withPath >> this.Visit)

        | SynExpr.LetOrUse(bindings = bindings; body = body) ->
            bindings |> List.iter (withPath >> this.Visit)
            body |> withPath |> this.Visit

        | SynExpr.TryWith(tryExpr = tryExpr; withCases = withCases) ->
            tryExpr |> withPath |> this.Visit
            withCases |> List.iter (withPath >> this.Visit)

        | SynExpr.TryFinally(tryExpr = tryExpr; finallyExpr = finallyExpr) ->
            tryExpr |> withPath |> this.Visit
            finallyExpr |> withPath |> this.Visit

        | SynExpr.Lazy(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.Sequential(expr1 = expr1; expr2 = expr2) ->
            expr1 |> withPath |> this.Visit
            expr2 |> withPath |> this.Visit

        | SynExpr.IfThenElse(ifExpr = ifExpr; thenExpr = thenExpr; elseExpr = elseExpr) ->
            ifExpr |> withPath |> this.Visit
            thenExpr |> withPath |> this.Visit
            elseExpr |> Option.iter (withPath >> this.Visit)

        | SynExpr.Typar(typar = typar) -> typar |> withPath |> this.Visit

        | SynExpr.Ident _ -> ()

        | SynExpr.LongIdent _ -> ()

        | SynExpr.LongIdentSet(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.DotGet(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.DotSet(targetExpr = targetExpr; rhsExpr = rhsExpr) ->
            targetExpr |> withPath |> this.Visit
            rhsExpr |> withPath |> this.Visit

        | SynExpr.Set(targetExpr = targetExpr; rhsExpr = rhsExpr) ->
            targetExpr |> withPath |> this.Visit
            rhsExpr |> withPath |> this.Visit

        | SynExpr.DotIndexedGet(objectExpr = objectExpr; indexArgs = indexArgs) ->
            objectExpr |> withPath |> this.Visit
            indexArgs |> withPath |> this.Visit

        | SynExpr.DotIndexedSet(objectExpr = objectExpr; indexArgs = indexArgs; valueExpr = valueExpr) ->
            objectExpr |> withPath |> this.Visit
            indexArgs |> withPath |> this.Visit
            valueExpr |> withPath |> this.Visit

        | SynExpr.NamedIndexedPropertySet(expr1 = expr1; expr2 = expr2) ->
            expr1 |> withPath |> this.Visit
            expr2 |> withPath |> this.Visit

        | SynExpr.DotNamedIndexedPropertySet(targetExpr = targetExpr; argExpr = argExpr; rhsExpr = rhsExpr) ->
            targetExpr |> withPath |> this.Visit
            argExpr |> withPath |> this.Visit
            rhsExpr |> withPath |> this.Visit

        | SynExpr.TypeTest(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.Visit
            targetType |> withPath |> this.Visit

        | SynExpr.Upcast(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.Visit
            targetType |> withPath |> this.Visit

        | SynExpr.Downcast(expr = expr; targetType = targetType) ->
            expr |> withPath |> this.Visit
            targetType |> withPath |> this.Visit

        | SynExpr.InferredUpcast(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.InferredDowncast(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.Null _ -> ()

        | SynExpr.AddressOf(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.TraitCall(supportTys = supportTys; traitSig = traitSig; argExpr = argExpr) ->
            supportTys |> List.iter (withPath >> this.Visit)
            traitSig |> withPath |> this.Visit
            argExpr |> withPath |> this.Visit

        | SynExpr.JoinIn(lhsExpr = lhsExpr; rhsExpr = rhsExpr) ->
            lhsExpr |> withPath |> this.Visit
            rhsExpr |> withPath |> this.Visit

        | SynExpr.ImplicitZero _ -> ()

        // TODO: Should this be ignored?
        | SynExpr.SequentialOrImplicitYield(expr1 = expr1; expr2 = expr2; ifNotStmt = ifNotStmt) ->
            expr1 |> withPath |> this.Visit
            expr2 |> withPath |> this.Visit
            // TODO: What is this?
            ifNotStmt |> withPath |> this.Visit

        | SynExpr.YieldOrReturn(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.YieldOrReturnFrom(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.LetOrUseBang(pat = pat; rhs = rhs; andBangs = andBangs; body = body) ->
            pat |> withPath |> this.Visit
            rhs |> withPath |> this.Visit
            andBangs |> List.iter (withPath >> this.Visit)
            body |> withPath |> this.Visit

        | SynExpr.MatchBang(expr = expr; clauses = clauses) ->
            expr |> withPath |> this.Visit
            clauses |> List.iter (withPath >> this.Visit)

        | SynExpr.DoBang(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.LibraryOnlyILAssembly _ -> ()

        | SynExpr.LibraryOnlyStaticOptimization _ -> ()

        | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> ()

        | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> ()

        | SynExpr.ArbitraryAfterError _ -> ()

        // TODO: Should this be ignored?
        | SynExpr.FromParseError(expr = expr) -> expr |> withPath |> this.Visit

        // TODO: Should this be ignored?
        | SynExpr.DiscardAfterMissingQualificationAfterDot(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.Fixed(expr = expr) -> expr |> withPath |> this.Visit

        | SynExpr.InterpolatedString(contents = contents) -> contents |> List.iter (withPath >> this.Visit)

        // TODO: What is this? Should it be ignored?
        | SynExpr.DebugPoint(innerExpr = innerExpr) -> innerExpr |> withPath |> this.Visit

        | SynExpr.Dynamic(funcExpr = funcExpr; argExpr = argExpr) ->
            funcExpr |> withPath |> this.Visit
            argExpr |> withPath |> this.Visit


    abstract member Visit: node: SynExprAndBang * path: UntypedAstNode list -> unit

    default this.Visit(node: SynExprAndBang, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExprAndBang node :: path

        match node with
        | SynExprAndBang.SynExprAndBang(pat = pat; body = body) ->
            pat |> withPath |> this.Visit
            body |> withPath |> this.Visit


    abstract member Visit: node: SynExprRecordField * path: UntypedAstNode list -> unit

    default this.Visit(node: SynExprRecordField, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynExprRecordField node :: path

        match node with
        | SynExprRecordField.SynExprRecordField(expr = expr) -> expr |> Option.iter (withPath >> this.Visit)


    abstract member Visit: node: SynField * path: UntypedAstNode list -> unit

    default this.Visit(node: SynField, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynField node :: path

        match node with
        | SynField.SynField(attributes = attributes; fieldType = fieldType) ->
            attributes |> List.iter (withPath >> this.Visit)
            fieldType |> withPath |> this.Visit


    abstract member Visit: node: SynInterfaceImpl * path: UntypedAstNode list -> unit

    default this.Visit(node: SynInterfaceImpl, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynInterfaceImpl node :: path

        match node with
        | SynInterfaceImpl.SynInterfaceImpl(interfaceTy = interfaceTy; bindings = bindings; members = members) ->
            interfaceTy |> withPath |> this.Visit
            bindings |> List.iter (withPath >> this.Visit)
            members |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynInterpolatedStringPart * path: UntypedAstNode list -> unit

    default this.Visit(node: SynInterpolatedStringPart, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynInterpolatedStringPart node :: path

        match node with
        | SynInterpolatedStringPart.String _ -> ()
        | SynInterpolatedStringPart.FillExpr(fillExpr = fillExpr) -> fillExpr |> withPath |> this.Visit


    abstract member Visit: node: SynMatchClause * path: UntypedAstNode list -> unit

    default this.Visit(node: SynMatchClause, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMatchClause node :: path

        match node with
        | SynMatchClause.SynMatchClause(pat = pat; whenExpr = whenExpr; resultExpr = resultExpr) ->
            pat |> withPath |> this.Visit
            whenExpr |> Option.iter (withPath >> this.Visit)
            resultExpr |> withPath |> this.Visit


    abstract member Visit: node: SynMeasure * path: UntypedAstNode list -> unit

    default this.Visit(node: SynMeasure, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMeasure node :: path

        match node with
        | SynMeasure.Named _ -> ()
        | SynMeasure.Product(measure1 = measure1; measure2 = measure2) ->
            measure1 |> withPath |> this.Visit
            measure2 |> withPath |> this.Visit
        | SynMeasure.Seq(measures = measures) -> measures |> List.iter (withPath >> this.Visit)
        | SynMeasure.Divide(measure1 = measure1; measure2 = measure2) ->
            measure1 |> withPath |> this.Visit
            measure2 |> withPath |> this.Visit
        | SynMeasure.Power(measure = measure) -> measure |> withPath |> this.Visit
        | SynMeasure.One -> ()
        | SynMeasure.Var(typar = typar) -> typar |> withPath |> this.Visit
        | SynMeasure.Anon _ -> ()
        | SynMeasure.Paren(measure = measure) -> measure |> withPath |> this.Visit


    abstract member Visit: node: SynMemberDefn * path: UntypedAstNode list -> unit

    default this.Visit(node: SynMemberDefn, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMemberDefn node :: path

        match node with
        | SynMemberDefn.Open(target = target) -> target |> withPath |> this.Visit
        | SynMemberDefn.Member(memberDefn = memberDefn) -> memberDefn |> withPath |> this.Visit
        | SynMemberDefn.GetSetMember(memberDefnForGet = memberDefnForGet; memberDefnForSet = memberDefnForSet) ->
            memberDefnForGet |> Option.iter (withPath >> this.Visit)
            memberDefnForSet |> Option.iter (withPath >> this.Visit)
        // TODO: Should this be ignored?
        | SynMemberDefn.ImplicitCtor(attributes = attributes; ctorArgs = ctorArgs) ->
            attributes |> List.iter (withPath >> this.Visit)
            ctorArgs |> withPath |> this.Visit
        | SynMemberDefn.ImplicitInherit(inheritType = inheritType; inheritArgs = inheritArgs) ->
            inheritType |> withPath |> this.Visit
            inheritArgs |> withPath |> this.Visit
        | SynMemberDefn.LetBindings(bindings = bindings) -> bindings |> List.iter (withPath >> this.Visit)
        | SynMemberDefn.AbstractSlot(slotSig = slotSig) -> slotSig |> withPath |> this.Visit
        | SynMemberDefn.Interface(interfaceType = interfaceType; members = members) ->
            interfaceType |> withPath |> this.Visit
            members |> Option.iter (List.iter (withPath >> this.Visit))
        | SynMemberDefn.Inherit(baseType = baseType) -> baseType |> withPath |> this.Visit
        | SynMemberDefn.ValField(fieldInfo = fieldInfo) -> fieldInfo |> withPath |> this.Visit
        | SynMemberDefn.NestedType(typeDefn = typeDefn) -> typeDefn |> withPath |> this.Visit
        | SynMemberDefn.AutoProperty(attributes = attributes; typeOpt = typeOpt; synExpr = synExpr) ->
            attributes |> List.iter (withPath >> this.Visit)
            typeOpt |> Option.iter (withPath >> this.Visit)
            synExpr |> withPath |> this.Visit


    abstract member Visit: node: SynMemberSig * path: UntypedAstNode list -> unit

    default this.Visit(node: SynMemberSig, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynMemberSig node :: path

        match node with
        | SynMemberSig.Member(memberSig = memberSig) -> memberSig |> withPath |> this.Visit
        | SynMemberSig.Interface(interfaceType = interfaceType) -> interfaceType |> withPath |> this.Visit
        | SynMemberSig.Inherit(inheritedType = inheritedType) -> inheritedType |> withPath |> this.Visit
        | SynMemberSig.ValField(field = field) -> field |> withPath |> this.Visit
        | SynMemberSig.NestedType(nestedType = nestedType) -> nestedType |> withPath |> this.Visit


    abstract member Visit: node: SynModuleDecl * path: UntypedAstNode list -> unit

    default this.Visit(node: SynModuleDecl, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynModuleDecl node :: path

        match node with
        | SynModuleDecl.Attributes(attributes = attributes) -> attributes |> List.iter (withPath >> this.Visit)
        | SynModuleDecl.Exception(exnDefn = exnDefn) -> exnDefn |> withPath |> this.Visit
        | SynModuleDecl.Expr(expr = expr) -> expr |> withPath |> this.Visit
        | SynModuleDecl.HashDirective(hashDirective = hashDirective) -> hashDirective |> withPath |> this.Visit
        | SynModuleDecl.Let(bindings = bindings) -> bindings |> List.iter (withPath >> this.Visit)
        | SynModuleDecl.ModuleAbbrev _ -> ()
        | SynModuleDecl.NamespaceFragment(fragment = fragment) -> fragment |> withPath |> this.Visit
        | SynModuleDecl.NestedModule(moduleInfo = moduleInfo; decls = decls) ->
            moduleInfo |> withPath |> this.Visit
            decls |> List.iter (withPath >> this.Visit)
        | SynModuleDecl.Open(target = target) -> target |> withPath |> this.Visit
        | SynModuleDecl.Types(typeDefns = typeDefns) -> typeDefns |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynModuleOrNamespace * path: UntypedAstNode list -> unit

    default this.Visit(node: SynModuleOrNamespace, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynModuleOrNamespace node :: path

        match node with
        | SynModuleOrNamespace.SynModuleOrNamespace(decls = decls; attribs = attribs) ->
            attribs |> List.iter (withPath >> this.Visit)
            decls |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynOpenDeclTarget * path: UntypedAstNode list -> unit

    default this.Visit(node: SynOpenDeclTarget, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynOpenDeclTarget node :: path

        match node with
        | SynOpenDeclTarget.ModuleOrNamespace _ -> ()
        | SynOpenDeclTarget.Type(typeName = typeName) -> typeName |> withPath |> this.Visit


    abstract member Visit: node: SynPat * path: UntypedAstNode list -> unit

    default this.Visit(node: SynPat, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynPat node :: path

        match node with
        | SynPat.Const(constant = constant) -> constant |> withPath |> this.Visit
        | SynPat.Wild _ -> ()
        | SynPat.Named _ -> ()
        | SynPat.Typed(pat = pat; targetType = targetType) ->
            pat |> withPath |> this.Visit
            targetType |> withPath |> this.Visit
        | SynPat.Attrib(pat = pat; attributes = attributes) ->
            pat |> withPath |> this.Visit
            attributes |> List.iter (withPath >> this.Visit)
        | SynPat.Or(lhsPat = lhsPat; rhsPat = rhsPat) ->
            lhsPat |> withPath |> this.Visit
            rhsPat |> withPath |> this.Visit
        | SynPat.Ands(pats = pats) -> pats |> List.iter (withPath >> this.Visit)
        | SynPat.As(lhsPat = lhsPat; rhsPat = rhsPat) ->
            lhsPat |> withPath |> this.Visit
            rhsPat |> withPath |> this.Visit
        | SynPat.LongIdent(typarDecls = typarDecls; argPats = argPats) ->
            typarDecls |> Option.iter (withPath >> this.Visit)
            argPats |> withPath |> this.Visit
        | SynPat.Tuple(elementPats = elementPats) -> elementPats |> List.iter (withPath >> this.Visit)
        | SynPat.Paren(pat = pat) -> pat |> withPath |> this.Visit
        | SynPat.ArrayOrList(elementPats = elementPats) -> elementPats |> List.iter (withPath >> this.Visit)
        | SynPat.Record(fieldPats = fieldPats) ->
            fieldPats |> List.iter (fun (_, _, pat) -> pat |> withPath |> this.Visit)
        | SynPat.Null _ -> ()
        | SynPat.OptionalVal _ -> ()
        | SynPat.IsInst(pat = pat) -> pat |> withPath |> this.Visit
        | SynPat.QuoteExpr(expr = expr) -> expr |> withPath |> this.Visit
        | SynPat.DeprecatedCharRange _ -> ()
        | SynPat.InstanceMember _ -> ()
        | SynPat.FromParseError _ -> ()


    abstract member Visit: node: SynSimplePat * path: UntypedAstNode list -> unit

    default this.Visit(node: SynSimplePat, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynSimplePat node :: path

        match node with
        | SynSimplePat.Id _ -> ()
        | SynSimplePat.Typed(pat = pat; targetType = targetType) ->
            pat |> withPath |> this.Visit
            targetType |> withPath |> this.Visit
        | SynSimplePat.Attrib(pat = pat; attributes = attributes) ->
            pat |> withPath |> this.Visit
            attributes |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynSimplePats * path: UntypedAstNode list -> unit

    default this.Visit(node: SynSimplePats, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynSimplePats node :: path

        match node with
        | SynSimplePats.SimplePats(pats = pats) -> pats |> List.iter (withPath >> this.Visit)
        | SynSimplePats.Typed(pats = pats; targetType = targetType) ->
            pats |> withPath |> this.Visit
            targetType |> withPath |> this.Visit


    abstract member Visit: node: SynTupleTypeSegment * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTupleTypeSegment, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTupleTypeSegment node :: path

        match node with
        | SynTupleTypeSegment.Type(typeName = typeName) -> typeName |> withPath |> this.Visit
        | SynTupleTypeSegment.Star _ -> ()
        | SynTupleTypeSegment.Slash _ -> ()


    abstract member Visit: node: SynTypar * path: UntypedAstNode list -> unit

    default this.Visit(_node: SynTypar, _path: UntypedAstNode list) = ()


    abstract member Visit: node: SynTyparDecl * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTyparDecl, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTyparDecl node :: path

        match node with
        | SynTyparDecl.SynTyparDecl(attributes = attributes) -> attributes |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynTyparDecls * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTyparDecls, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTyparDecls node :: path

        match node with
        | SynTyparDecls.PostfixList(decls = decls; constraints = constraints) ->
            decls |> List.iter (withPath >> this.Visit)
            constraints |> List.iter (withPath >> this.Visit)
        | SynTyparDecls.PrefixList(decls = decls) -> decls |> List.iter (withPath >> this.Visit)
        | SynTyparDecls.SinglePrefix(decl = decl) -> decl |> withPath |> this.Visit


    abstract member Visit: node: SynType * path: UntypedAstNode list -> unit

    default this.Visit(node: SynType, path: UntypedAstNode list) =
        let withPath n = n, UntypedAstNode.SynType node :: path

        match node with
        | SynType.LongIdent _ -> ()
        | SynType.App(typeName = typeName; typeArgs = typeArgs) ->
            typeName |> withPath |> this.Visit
            typeArgs |> List.iter (withPath >> this.Visit)
        | SynType.LongIdentApp(typeName = typeName; typeArgs = typeArgs) ->
            typeName |> withPath |> this.Visit
            typeArgs |> List.iter (withPath >> this.Visit)
        | SynType.Tuple(path = path) -> path |> List.iter (withPath >> this.Visit)
        | SynType.AnonRecd(fields = fields) -> fields |> List.iter (snd >> withPath >> this.Visit)
        | SynType.Array(elementType = elementType) -> elementType |> withPath |> this.Visit
        | SynType.Fun(argType = argType; returnType = returnType) ->
            argType |> withPath |> this.Visit
            returnType |> withPath |> this.Visit
        | SynType.Var(typar = typar) -> typar |> withPath |> this.Visit
        | SynType.Anon _ -> ()
        | SynType.WithGlobalConstraints(typeName = typeName; constraints = constraints) ->
            typeName |> withPath |> this.Visit
            constraints |> List.iter (withPath >> this.Visit)
        | SynType.HashConstraint(innerType = innerType) -> innerType |> withPath |> this.Visit
        | SynType.MeasurePower(baseMeasure = baseMeasure) -> baseMeasure |> withPath |> this.Visit
        | SynType.MeasureDivide(dividend = dividend; divisor = divisor) ->
            dividend |> withPath |> this.Visit
            divisor |> withPath |> this.Visit
        | SynType.StaticConstant(constant = constant) -> constant |> withPath |> this.Visit
        | SynType.StaticConstantExpr(expr = expr) -> expr |> withPath |> this.Visit
        | SynType.StaticConstantNamed(ident = ident; value = value) ->
            ident |> withPath |> this.Visit
            value |> withPath |> this.Visit
        | SynType.Paren(innerType = innerType) -> innerType |> withPath |> this.Visit
        | SynType.SignatureParameter(attributes = attributes; usedType = usedType) ->
            attributes |> List.iter (withPath >> this.Visit)
            usedType |> withPath |> this.Visit


    abstract member Visit: node: SynTypeConstraint * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTypeConstraint, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeConstraint node :: path

        match node with
        | SynTypeConstraint.WhereTyparIsValueType(typar = typar) -> typar |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparIsReferenceType(typar = typar) -> typar |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparIsUnmanaged(typar = typar) -> typar |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparSupportsNull(typar = typar) -> typar |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparIsComparable(typar = typar) -> typar |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparIsEquatable(typar = typar) -> typar |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparDefaultsToType(typar = typar; typeName = typeName) ->
            typar |> withPath |> this.Visit
            typeName |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparSubtypeOfType(typar = typar; typeName = typeName) ->
            typar |> withPath |> this.Visit
            typeName |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparSupportsMember(typars = typars; memberSig = memberSig) ->
            typars |> List.iter (withPath >> this.Visit)
            memberSig |> withPath |> this.Visit
        | SynTypeConstraint.WhereTyparIsEnum(typar = typar; typeArgs = typeArgs) ->
            typar |> withPath |> this.Visit
            typeArgs |> List.iter (withPath >> this.Visit)
        | SynTypeConstraint.WhereTyparIsDelegate(typar = typar; typeArgs = typeArgs) ->
            typar |> withPath |> this.Visit
            typeArgs |> List.iter (withPath >> this.Visit)
        | SynTypeConstraint.WhereSelfConstrained(selfConstraint = selfConstraint) ->
            selfConstraint |> withPath |> this.Visit


    abstract member Visit: node: SynTypeDefn * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTypeDefn, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefn node :: path

        match node with
        | SynTypeDefn.SynTypeDefn(
            typeInfo = typeInfo; typeRepr = typeRepr; members = members; implicitConstructor = implicitConstructor) ->
            typeInfo |> withPath |> this.Visit
            typeRepr |> withPath |> this.Visit
            members |> List.iter (withPath >> this.Visit)
            implicitConstructor |> Option.iter (withPath >> this.Visit)


    abstract member Visit: node: SynTypeDefnKind * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTypeDefnKind, path: UntypedAstNode list) =
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
            signature |> withPath |> this.Visit
            signatureInfo |> withPath |> this.Visit


    abstract member Visit: node: SynTypeDefnRepr * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTypeDefnRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnRepr node :: path

        match node with
        | SynTypeDefnRepr.ObjectModel(kind = kind; members = members) ->
            kind |> withPath |> this.Visit
            members |> List.iter (withPath >> this.Visit)
        | SynTypeDefnRepr.Simple(simpleRepr = simpleRepr) -> simpleRepr |> withPath |> this.Visit
        | SynTypeDefnRepr.Exception(exnRepr = exnRepr) -> exnRepr |> withPath |> this.Visit


    abstract member Visit: node: SynTypeDefnSig * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTypeDefnSig, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnSig node :: path

        match node with
        | SynTypeDefnSig.SynTypeDefnSig(typeInfo = typeInfo; typeRepr = typeRepr; members = members) ->
            typeInfo |> withPath |> this.Visit
            typeRepr |> withPath |> this.Visit
            members |> List.iter (withPath >> this.Visit)


    abstract member Visit: node: SynTypeDefnSigRepr * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTypeDefnSigRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnSigRepr node :: path

        match node with
        | SynTypeDefnSigRepr.ObjectModel(kind = kind; memberSigs = memberSigs) ->
            kind |> withPath |> this.Visit
            memberSigs |> List.iter (withPath >> this.Visit)
        | SynTypeDefnSigRepr.Simple(repr = repr) -> repr |> withPath |> this.Visit
        | SynTypeDefnSigRepr.Exception(repr = repr) -> repr |> withPath |> this.Visit


    abstract member Visit: node: SynTypeDefnSimpleRepr * path: UntypedAstNode list -> unit

    default this.Visit(node: SynTypeDefnSimpleRepr, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynTypeDefnSimpleRepr node :: path

        match node with
        | SynTypeDefnSimpleRepr.Union(unionCases = unionCases) -> unionCases |> List.iter (withPath >> this.Visit)
        | SynTypeDefnSimpleRepr.Enum(cases = cases) -> cases |> List.iter (withPath >> this.Visit)
        | SynTypeDefnSimpleRepr.Record(recordFields = recordFields) ->
            recordFields |> List.iter (withPath >> this.Visit)
        | SynTypeDefnSimpleRepr.General(kind = kind; inherits = inherits; slotsigs = slotsigs; fields = fields) ->
            kind |> withPath |> this.Visit
            inherits |> List.iter (fun (t, _, _) -> t |> withPath |> this.Visit)
            // TODO: Use 'slotsigs'? Docs says 'this is not a parse-tree form'
            slotsigs |> List.iter (fst >> withPath >> this.Visit)
            fields |> List.iter (withPath >> this.Visit)
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> ()
        | SynTypeDefnSimpleRepr.TypeAbbrev(detail = detail; rhsType = rhsType) ->
            match detail with
            | ParserDetail.Ok -> rhsType |> withPath |> this.Visit
            | ParserDetail.ErrorRecovery -> ()
        | SynTypeDefnSimpleRepr.None _ -> ()
        | SynTypeDefnSimpleRepr.Exception(exnRepr = exnRepr) -> exnRepr |> withPath |> this.Visit


    abstract member Visit: node: SynUnionCase * path: UntypedAstNode list -> unit

    default this.Visit(node: SynUnionCase, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynUnionCase node :: path

        match node with
        | SynUnionCase.SynUnionCase(attributes = attributes; caseType = caseType) ->
            attributes |> List.iter (withPath >> this.Visit)
            caseType |> withPath |> this.Visit


    abstract member Visit: node: SynUnionCaseKind * path: UntypedAstNode list -> unit

    default this.Visit(node: SynUnionCaseKind, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynUnionCaseKind node :: path

        match node with
        | SynUnionCaseKind.Fields(cases = cases) -> cases |> List.iter (withPath >> this.Visit)
        | SynUnionCaseKind.FullType(fullType = fullType; fullTypeInfo = fullTypeInfo) ->
            fullType |> withPath |> this.Visit
            fullTypeInfo |> withPath |> this.Visit


    abstract member Visit: node: SynValData * path: UntypedAstNode list -> unit

    default this.Visit(node: SynValData, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValData node :: path

        match node with
        | SynValData.SynValData(valInfo = valInfo) -> valInfo |> withPath |> this.Visit


    abstract member Visit: node: SynValInfo * path: UntypedAstNode list -> unit

    default this.Visit(node: SynValInfo, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValInfo node :: path

        match node with
        | SynValInfo.SynValInfo(curriedArgInfos = curriedArgInfos) ->
            curriedArgInfos |> List.iter (List.iter (withPath >> this.Visit))


    abstract member Visit: node: SynValSig * path: UntypedAstNode list -> unit

    default this.Visit(node: SynValSig, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValSig node :: path

        match node with
        | SynValSig.SynValSig(
            attributes = attributes
            explicitTypeParams = explicitTypeParams
            synType = synType
            arity = arity
            synExpr = synExpr) ->
            attributes |> List.iter (withPath >> this.Visit)
            explicitTypeParams |> withPath |> this.Visit
            synType |> withPath |> this.Visit
            arity |> withPath |> this.Visit
            synExpr |> Option.iter (withPath >> this.Visit)


    abstract member Visit: node: SynValTyparDecls * path: UntypedAstNode list -> unit

    default this.Visit(node: SynValTyparDecls, path: UntypedAstNode list) =
        let withPath n =
            n, UntypedAstNode.SynValTyparDecls node :: path

        match node with
        | SynValTyparDecls.SynValTyparDecls(typars = typars) -> typars |> Option.iter (withPath >> this.Visit)
