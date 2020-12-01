open System.IO

let values =
    File.ReadAllLines "./Day1/Input.txt"
    |> Seq.map int

let printResult result = printfn $"Answer is {result}"

let part1 input =
    for value in input do
        for innerValue in input do
            if value + innerValue = 2020 then printResult (value * innerValue)

let part2 input =
    for value in input do
        for innerValue in input do
            for anotherInnerValue in input do
                if value + innerValue + anotherInnerValue = 2020
                then printResult (value * innerValue * anotherInnerValue)

part1 values
part2 values