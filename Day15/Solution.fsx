open System.IO

let getInput nthElement =
    let fileInput =
        File.ReadAllText "./Day15/Input.txt"
        |> fun input ->
            input.Split ','
            |> Seq.map (fun v -> Some(int v))
            |> Seq.toArray

    let emptyList =
        Seq.init (nthElement - fileInput.Length) (fun _ -> None)

    emptyList |> Seq.append fileInput |> Seq.toArray

let processTurn possibleValue previousTurnValue currentTurnNumber (lastTurnsPerValue: Map<int, (Option<int> * Option<int>)>) =
    let updateTurnsForNumber number =
        match lastTurnsPerValue.TryFind number with
        | Some ((Some _, Some lastSpoken)) -> lastTurnsPerValue.Add(number, (Some(lastSpoken), Some(currentTurnNumber)))
        | Some ((Some lastSpoken, None)) -> lastTurnsPerValue.Add(number, (Some(lastSpoken), Some(currentTurnNumber)))
        | _ -> lastTurnsPerValue.Add(number, (Some(currentTurnNumber), None))

    match possibleValue with
    | Some value -> (lastTurnsPerValue.Add(value, (Some(currentTurnNumber), None)), value)
    | _ ->
        match lastTurnsPerValue.Item previousTurnValue with
        | (Some previousLastSpoken, Some lastSpoken) ->
            let difference = lastSpoken - previousLastSpoken
            ((updateTurnsForNumber difference), difference)
        | (Some _, None) -> ((updateTurnsForNumber 0), 0)
        | _ -> failwith "Not possible scenario"

let calculate input =
    input
    |> Seq.indexed
    |> Seq.fold (fun (lastTurnsPerValue, previousTurnValue) (turn, possibleValue) ->
        processTurn possibleValue previousTurnValue (turn + 1) lastTurnsPerValue) (Map.empty, 0)
    |> snd

let part1 = 2020 |> getInput |> calculate

printf $"Answer for part1 is {part1}"

let part2 = 30000000 |> getInput |> calculate

printf $"Answer for part2 is {part2}"