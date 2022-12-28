[<AutoOpen>]
module Utils

open System


module String =

    let removePrefix (prefix: string) (str: string) =
        if str.StartsWith(prefix, StringComparison.Ordinal) then
            str.Substring prefix.Length
        else
            str

    let deIndent (str: string) =
        let lines = str.Split(Environment.NewLine)

        let shortestRunOfSpacesForNonEmptyLines =
            lines
            |> Seq.filter (not << String.IsNullOrWhiteSpace)
            |> Seq.map (fun s -> s |> Seq.takeWhile ((=) ' ') |> Seq.length)
            |> Seq.min

        let spacesToRemove = String.replicate shortestRunOfSpacesForNonEmptyLines " "

        lines
        |> Seq.map (removePrefix spacesToRemove)
        |> String.concat Environment.NewLine
