module FLN0101_BindDisposableWithUse

open Xunit
open FluentAssertions
open Flinter.Analyzers.FLN0101_BindDisposableWithUse

[<Fact>]
let ``Returns correct message data`` () =
    """
    let f () =
        let x = System.IO.StreamWriter("")
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)
        .And.MatchRespectively(
            Msg(
                message =
                    "This value has type 'System.IO.StreamWriter' which implements IDisposable, and should therefore be bound with 'use' instead of 'let'.",
                code = "FLN0101",
                type' = "BindDisposableWithUse"
            )
        )


[<Fact>]
let ``Handles multiple bindings`` () =
    """
    let f () =
        let x = System.IO.StreamWriter("")
            ^
        let y = System.IO.StreamWriter("")
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles multiple recursive bindings`` () =
    """
    let f () =
        let rec x = System.IO.StreamWriter("")
                ^
        and y = System.IO.StreamWriter("")
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles first of two bindings`` () =
    """
    let f () =
        let x1, x2 = System.IO.StreamWriter(""), 0
            ^^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles second of two bindings`` () =
    """
    let f () =
        let x1, x2 = 0, System.IO.StreamWriter("")
                ^^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles both of two bindings`` () =
    """
    let f () =
        let x1, x2 = System.IO.StreamWriter(""), System.IO.StreamWriter("")
            ^^  ^^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Constructor call with 'new'`` () =
    """
    let f () =
        let x = new System.IO.StreamWriter("")
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Constructor call without 'new'`` () =
    """
    let f () =
        let x = System.IO.StreamWriter("")
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Custom type constructor call`` () =
    """
    type Foo() =
        interface System.IDisposable with member _.Dispose() = ()


    let f () =
        let x = new Foo()
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Static method call`` () =
    """
    let f () =
        let x = System.IO.File.OpenText("")
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Instance method call`` () =
    """
    let f () =
        let x = System.IO.FileInfo("").OpenRead()
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Static property getter call`` () =
    """
    type Foo() =
        static member A = { new System.IDisposable with member _.Dispose() = () }

    let f () =
        let x = Foo.A
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Instance property getter call`` () =
    """
    type Foo() =
        member _.A = { new System.IDisposable with member _.Dispose() = () }

    let f () =
        let x = Foo().A
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Indexer call`` () =
    """
    type Foo() =
        member _.Item(_: int) = { new System.IDisposable with member _.Dispose() = () }

    let f () =
        let x = Foo()[0]
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Object expression`` () =
    """
    let f () =
        let x = { new System.IDisposable with member _.Dispose() = () }
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Function call`` () =
    """
    let g () = { new System.IDisposable with member _.Dispose() = () }

    let f () =
        let x = g ()
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Function-as-value call`` () =
    """
    let g = System.IO.File.OpenText

    let f () =
        let x = g ""
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles any RHS expression: Trait call`` () =
    """
    let inline f a =
        let x = (^a: (member A: System.IDisposable) a)
            ^
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
//
let ``Known limitation (change if fixed): Does not handle wildcard-bound value`` () =
    // Compiler seems to handle this as not being let-bound at all
    """
    let f () =
        let _ = System.IO.StreamWriter("")
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles bound value in nested expression`` () =
    """
    let f () =
        let a =
            let x = System.IO.StreamWriter("")
                ^
            ()
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles expression bound with 'let x' in CE`` () =
    """
    async {
        let x = System.IO.StreamWriter("")
            ^
        ()
    }
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles expression bound with 'let! x' in CE`` () =
    """
    async {
        let! x = async.Return (System.IO.StreamWriter(""))
             ^
        ()
    }
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Handles expression bound with 'and!' in CE`` () =
    """
    type ValidationBuilder() =
        member inline _.Return(x) = Ok x
        member inline _.ReturnFrom(x) = x
        member inline _.Bind(result, binder) = Result.bind binder result
        member inline this.Zero() = Ok()

        member inline _.MergeSources
            (
                _left: Result<'left, 'error list>,
                _right: Result<'right, 'error list>
            ) : Result<'left * 'right, 'error list> =
            failwith ""

    let validation = ValidationBuilder()

    validation {
        let! x = Ok(new System.IO.StreamWriter(""))
             ^
        and! y = Ok()
        ()
    }

    validation {
        let! x = Ok()
        and! y = Ok(new System.IO.StreamWriter(""))
             ^
        ()
    }

    validation {
        let! x = Ok(new System.IO.StreamWriter(""))
             ^
        and! y = Ok(new System.IO.StreamWriter(""))
             ^
        ()
    }
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Known limitation (change if fixed): Does not handle expression bound with 'let! _' in CE`` () =
    // Compiler seems to handle this as not being let-bound at all
    """
    async {
        let! _ = async.Return (System.IO.StreamWriter(""))
        ()
    }
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on non-disposable bindings`` () =
    """
    let f () =
        let x = 0
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger when expression is bound with 'use x'`` () =
    """
    let f () =
        use x = new System.IO.StreamWriter("")
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger when expression is bound with 'use _'`` () =
    """
    let f () =
        use _ = new System.IO.StreamWriter("")
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger when expression is bound with 'use! x' in CE`` () =
    """
    async {
        use! x = async.Return (new System.IO.StreamWriter(""))
        ()
    }
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger when expression is bound with 'use! __' in CE`` () =
    """
    async {
        use! __ = async.Return (System.IO.StreamWriter(""))
        ()
    }
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on non-bound expressions`` () =
    """
    let f () =
        System.IO.StreamWriter("")
        System.IO.StreamWriter("") |> ignore
        () |> System.IO.MemoryStream |> ignore
        () |> System.IO.MemoryStream
        ignore (System.IO.StreamWriter(""))
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on function values returning IDisposable`` () =
    """
    let f () =
        let g = System.IO.File.OpenText
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on System.IO.MemoryStream`` () =
    """
    let f () =
        let x = System.IO.MemoryStream()
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on System.IO.StringReader`` () =
    """
    let f () =
        let x = System.IO.StringReader("")
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on System.IO.StringWriter`` () =
    """
    let f () =
        let x = System.IO.StringWriter()
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)

[<Fact>]
let ``Does not trigger on System.IO.UnmanagedMemoryStream`` () =
    """
    let f () =
        let x = System.IO.UnmanagedMemoryStream(Unchecked.defaultof<System.Runtime.InteropServices.SafeBuffer>, 0L, 0L)
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)


[<Fact>]
let ``Does not trigger on System.IO.UnmanagedMemoryAccessor `` () =
    """
    let f () =
        let x = System.IO.UnmanagedMemoryAccessor(Unchecked.defaultof<System.Runtime.InteropServices.SafeBuffer>, 0L, 0L)
        ()
    """
        .Should()
        .ContainOnlyMarkedErrors(analyze)
