open System.IO

let joltDifferences =
    let fileValues =
        File.ReadAllLines "./Day10/Input.txt"
        |> Seq.map int

    fileValues
    |> Seq.append [ fileValues |> Seq.max |> (+) 3 ]
    |> Seq.append [ 0 ]
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.map (fun (curr, next) -> next - curr)

let part1 =
    let oneDifferenceJoltsCount =
        joltDifferences
        |> Seq.filter ((=) 1)
        |> Seq.length

    let threeDifferenceJoltsCount =
        joltDifferences
        |> Seq.filter ((=) 3)
        |> Seq.length

    oneDifferenceJoltsCount
    * threeDifferenceJoltsCount

printfn $"Answer for part1 is {part1}"

let part2 =
    let differencesAsString =
        joltDifferences
        |> Seq.map string
        |> String.concat ""

    let groups = differencesAsString.Split '3'

    let chainWith2 =
        groups
        |> Array.filter (fun c -> c.Length = 2)
        |> Array.length
        |> float

    let chainWith3 =
        groups
        |> Array.filter (fun c -> c.Length = 3)
        |> Array.length
        |> float

    let chainWith4 =
        groups
        |> Array.filter (fun c -> c.Length = 4)
        |> Array.length
        |> float

    (2.0 ** chainWith2
     * 4.0 ** chainWith3
     * 7.0 ** chainWith4)
    |> int64

printfn $"Answer for part2 is {part2}"