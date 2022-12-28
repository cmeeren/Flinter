[<AutoOpen>]
module Assertions

open System
open System.Runtime.CompilerServices
open System.Text.RegularExpressions

open FSharp.Analyzers.SDK

open FluentAssertions
open FluentAssertions.Collections
open FluentAssertions.Primitives


/// A helper used to assert on parts of the contents of a Message
type Msg(?start: int * int, ?end': int * int, ?message: string, ?code: string, ?type': string) =
    member _.Match(msg: Message) =
        start
        |> Option.iter (fun tup -> (msg.Range.Start.Line, msg.Range.Start.Column).Should().Be(tup, "") |> ignore)

        end'
        |> Option.iter (fun tup -> (msg.Range.End.Line, msg.Range.End.Column).Should().Be(tup, "") |> ignore)

        message |> Option.iter (fun m -> msg.Message.Should().Be(m, "") |> ignore)

        code |> Option.iter (fun m -> msg.Code.Should().Be(m, "") |> ignore)

        type' |> Option.iter (fun t -> msg.Type.Should().Be(t, "") |> ignore)


[<Extension>]
type ConcreteGenericExtensions() =

    [<Extension>]
    static member Should(this: Message list) =
        GenericCollectionAssertions<Message>(this)

    [<Extension; CustomAssertion>]
    static member MatchRespectively(this: GenericCollectionAssertions<Message>, [<ParamArray>] messages: Msg[]) =
        let actions = messages |> Array.map (fun msg -> Action<Message>(msg.Match))
        this.HaveCount(actions.Length, "").And.SatisfyRespectively(actions)


type StringAssertions with

    member this.ContainOnlyMarkedErrors(analyze: Analyzer) =
        let marker = '^'

        let isMarkerLine (s: string) =
            s.Contains(marker) && s |> Seq.forall (fun c -> c = ' ' || c = marker)

        let processedSourceWithMarkings = Context.processTestSource this.Subject
        let sourceLines = ResizeArray()
        let ranges = ResizeArray()
        let mutable sourceLineNo = 0

        for line in processedSourceWithMarkings.Split(Environment.NewLine) do
            if isMarkerLine line then
                for m in Regex.Matches(line, @"\^+") do
                    ranges.Add((sourceLineNo, m.Index), (sourceLineNo, m.Index + m.Length))
            else
                sourceLines.Add(line)
                sourceLineNo <- sourceLineNo + 1

        let source = String.concat Environment.NewLine sourceLines
        let msgs = ranges |> Seq.map (fun (start, end') -> Msg(start, end')) |> Seq.toArray
        let messages = source |> Context.fromProcessedTestSource |> analyze

        if Array.isEmpty msgs then
            messages.Should().BeEmpty("the source contained no marks")
        else
            messages
                .Should()
                .HaveCount(msgs.Length, "the source contained that many marks")
                .And.MatchRespectively(msgs)
