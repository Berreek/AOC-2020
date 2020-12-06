open System.IO

let input = File.ReadAllLines "./Day5/Input.txt"

let rowRange = seq { 0..127 }
let columnRange = seq { 0..7 }

let getPosition lowerPartIdentifier upperPartIdentifier values pattern  =
    let getNextPart values identifier =
        let mid = (values |> Seq.length) / 2;
        match identifier with
        | _ when lowerPartIdentifier = identifier -> values |> Seq.take mid
        | _ when upperPartIdentifier = identifier -> values |> Seq.skip mid
        | _ -> values
    pattern |> Seq.fold getNextPart values |> Seq.head
    
let getSeatId line =
    let row = line |> Seq.take 7 |> getPosition 'F' 'B' rowRange
    let column = line |> Seq.skip 7 |> getPosition 'L' 'R' columnRange
    row * 8 + column

let part1 = input |> Seq.map getSeatId |> Seq.max
printfn $"Result for part 1 is {part1}"

let part2  =
    input
    |> Seq.map getSeatId
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.find (fun (cur, next) -> next - cur <> 1)
    |> fst
    |> (+) 1
printfn $"Result for part 2 is {part2}"
