open System.IO

let (timestamp, buses) =
    let fileData =
        File.ReadAllLines "./Day13/Input.txt"
        |> Seq.toArray

    (int64 fileData.[0],
     fileData.[1].Split ','
     |> Seq.indexed
     |> Seq.filter (fun (i, id) -> id <> "x")
     |> Seq.map (fun (i, id) -> (int64 i, int64 id)))

let part1 =
    buses
    |> Seq.map (fun (_, id) -> (id, id - (timestamp % id)))
    |> Seq.minBy snd
    |> fun (busId, difference) -> busId * difference


printfn $"Answer for part1 is {part1}"

let getMatchingTimestamp (currentTimestamp, period) (index, busId) =
    Seq.initInfinite (fun n -> (int64 n) * period + currentTimestamp)
    |> Seq.find (fun timestamp -> (timestamp + index) % busId = 0L)
    |> fun timestamp -> timestamp, period * busId

let part2 = ((0L, 1L), buses) ||> Seq.fold getMatchingTimestamp |> fst
printfn $"Answer for part2 is {part2}"
