open System.IO

type Slope = { Right: int; Down: int }

let everyNth n seq =
    seq
    |> Seq.mapi (fun i el -> el, i)
    |> Seq.filter (fun (_, i) -> (i % n) = 0)
    |> Seq.map fst

let getInput () = File.ReadAllLines "./Day3/Input.txt"

let getTreesCount (values: seq<string>) slope =
    let maxLengthOfLine = (Seq.item 0 values).Length

    let isATree slope index (item: Option<string>) =
        match item with
        | Some l -> l.[(index * slope.Right) % maxLengthOfLine] = '#'
        | None -> false

    let findATree slope lines =
        let handleLine index _ =
            let nextIndex = index + 1

            lines
            |> Seq.tryItem nextIndex
            |> isATree slope nextIndex

        lines |> Seq.mapi handleLine

    values
    |> everyNth slope.Down
    |> findATree slope
    |> Seq.filter id
    |> Seq.length

let input = getInput ()

let part1 =
    getTreesCount input { Right = 3; Down = 1 }

printfn $"Result for part 1 is {part1}"

let slopes =
    [ { Right = 1; Down = 1 }
      { Right = 3; Down = 1 }
      { Right = 5; Down = 1 }
      { Right = 7; Down = 1 }
      { Right = 1; Down = 2 } ]

let part2 =
    slopes
    |> Seq.map (getTreesCount input)
    |> Seq.fold (fun init el -> init * el) 1

printfn $"Result for part 2 is {part2}"