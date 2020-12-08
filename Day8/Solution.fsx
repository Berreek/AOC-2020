open System
open System.IO

type Operation =
    { Id: int
      Command: string
      Value: int }

let mapToOperation (i, line: string) =
    let spliitedLine =
        line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    { Command = spliitedLine.[0]
      Value = int spliitedLine.[1]
      Id = i }

let input =
    File.ReadAllLines "./Day8/Input.txt"
    |> Seq.indexed
    |> Seq.map mapToOperation
    |> Seq.toArray

let rec executeOperation acc currentIndex seen (operations: Operation[]) =
    match currentIndex with
    | lastIndex when lastIndex >= operations.Length -> true, acc
    | _ ->
        match seen |> Set.contains currentIndex with
        | true -> false, acc
        | _ ->
            let updatedSet = seen |> Set.add currentIndex
            let currentOperation = operations.[currentIndex];
                        
            match currentOperation.Command with
            | "acc" -> executeOperation (acc + currentOperation.Value) (currentIndex + 1) updatedSet operations
            | "jmp" -> executeOperation acc (currentIndex + currentOperation.Value) updatedSet operations
            | _ -> executeOperation acc (currentIndex + 1) updatedSet operations

let part1 =
    executeOperation 0 0 Set.empty input
    |> snd

printfn $"Result for part 1 is {part1}"

let part2 =
    let replaceOperation operationToReplace =
        input
        |> Seq.map (fun operation -> if operation.Id = operationToReplace.Id then operationToReplace else operation)
        |> Seq.toArray

    input
    |> Seq.choose (fun operation ->
        match operation.Command with
        | "jmp" ->
            replaceOperation { operation with Command = "nop" }
            |> Some
        | "nop" ->
            replaceOperation { operation with Command = "jmp" }
            |> Some
        | _ -> None)
    |> Seq.map (executeOperation 0 0 Set.empty)
    |> Seq.find (fst)
    |> snd

printfn $"Result for part 2 is {part2}"