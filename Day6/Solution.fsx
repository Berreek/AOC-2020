open System.IO
open System

let getData map data =
    let emptyLinesIndexes =
        data
        |> Seq.indexed
        |> Seq.filter (fun (_, line) -> String.IsNullOrEmpty line)
        |> Seq.map fst
        |> Seq.toArray

    emptyLinesIndexes.[..^1]
    |> Seq.pairwise
    |> Seq.map map

let input = File.ReadAllLines "./Day6/Input.txt"

let part1 =
    let mapForFlattenGroup (current, next) =
        String.Join("", input.[current + 1..next - 1])

    let getDistinctQuestionsCountForGroup flattenGroup =
        flattenGroup |> Seq.distinct |> Seq.length

    input
    |> getData mapForFlattenGroup
    |> Seq.sumBy getDistinctQuestionsCountForGroup

let part2 =
    let mapForEachPersonInGroup (current, next) = input.[current + 1..next - 1]

    let getAllAnsweredQuestionsCountForGroup group =
        let isQuestionAnsweredByAll question =
            group |> Seq.forall (Seq.contains question)

        Seq.head group
        |> Seq.filter isQuestionAnsweredByAll
        |> Seq.length

    input
    |> getData mapForEachPersonInGroup
    |> Seq.map getAllAnsweredQuestionsCountForGroup
    |> Seq.sum

printfn $"Answer for part 1 is {part1}"
printfn $"Answer for part 2 is {part2}"