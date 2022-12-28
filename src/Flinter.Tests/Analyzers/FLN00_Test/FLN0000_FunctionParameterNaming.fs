module FLN0001_FunctionParameterNaming

open Flinter.Analyzers.FLN0000_FunctionParameterNaming
open FluentAssertions
open Xunit


[<Fact>]
let ``Returns correct message data for an invalid parameter`` () =
    """
    let f a Foo c = 0
            ^^^
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)
        .And
        .MatchRespectively(
            Msg(
                message = "The parameter name 'Foo' does not follow the rule for parameter names.",
                code = "FLN0000",
                type' = "FunctionParameterNaming"
            )
        )


[<Fact>]
let ``Does not trigger on valid function parameter names`` () =
    """
    let f a b c = 0
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on method parameter names`` () =
    """
    type A() =
        method _.A(Foo) = 1
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles multiple invalid parameter names in multiple top-level functions`` () =
    """
    let f1 a Foo BAR = 0
             ^^^ ^^^
    let f2 a b c = 0
    let f3 Baz b c = 0
           ^^^
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)



[<Fact>]
let ``Handles inner functions`` () =
    """
    let f1 () =
        let f2 A = ()
               ^
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)
