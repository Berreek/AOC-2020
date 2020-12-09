open System.IO

let input =
    File.ReadAllLines "./Day9/Input.txt"
    |> Seq.map int64
    |> Seq.indexed

let getInvalidNumber (index, valueToCheck) =
    let isValueValid value1 value2 =
        value1 <> value2 && value1 + value2 = valueToCheck

    let valuesForVerification =
        input
        |> Seq.skip (index - 25)
        |> Seq.take 25
        |> Seq.map snd

    valuesForVerification
    |> Seq.exists (fun value ->
        valuesForVerification
        |> Seq.exists (isValueValid value))
    |> not

let getMinAndMaxFromMatchingRange valueToCheck =
    let rangeValues =
        input
        |> Seq.map (fun (index, _) ->
            input
            |> Seq.map snd
            |> Seq.skip index
            |> Seq.scan (fun (cumulated, _) value -> (cumulated + value, value)) (int64 0, int64 0)
            |> Seq.takeWhile (fun (cumulated, _) -> cumulated <= valueToCheck)
            |> Seq.map snd
            |> Seq.skip 1)
        |> Seq.find (fun range -> range |> Seq.sum |> (=) valueToCheck)

    Seq.min rangeValues + Seq.max rangeValues

let part1 =
    input
    |> Seq.skip 25
    |> Seq.find getInvalidNumber
    |> snd

printfn $"Answer for part 1 is {part1}"

let part2 =
    part1
    |> getMinAndMaxFromMatchingRange

printfn $"Answer for part 2 is {part2}"