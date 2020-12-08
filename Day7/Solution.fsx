open System
open System.IO

type NestedBag = { Amount: int; Name: string }

type Bag =
    { Name: string
      NestedBags: seq<NestedBag> }

let mapToBag (line: string) =
    let getNestedBags (bags: string) =
        match bags.TrimEnd('.') with
        | " no other bags" -> Seq.empty
        | trimmed ->
            trimmed.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun bag ->
                { Name = bag.[3..^4].TrimEnd()
                  Amount = int bag.[..1] })

    let splittedByContain =
        line.Split([| "contain" |], StringSplitOptions.RemoveEmptyEntries)

    { Name = splittedByContain.[0].[..^6]
      NestedBags = splittedByContain.[1] |> getNestedBags }

let allBags =
    File.ReadAllLines "./Day7/Input.txt"
    |> Seq.map mapToBag

let rec part1 bagName alreadyFoundBags =
    let bagsThatContain =
        allBags
        |> Seq.filter (fun bag ->
            bag.NestedBags
            |> Seq.map (fun nested -> nested.Name)
            |> Seq.contains bagName)

    bagsThatContain
    |> Seq.map (fun bag -> part1 bag.Name bagsThatContain)
    |> Seq.collect id
    |> Seq.append alreadyFoundBags

let result1 =
    part1 "shiny gold" Seq.empty
    |> Seq.distinctBy (fun bag -> bag.Name)
    |> Seq.length

printfn $"Answer for part 1 is {result1}"

let part2 =
    let rec getBagsCount bag =
        let nestedBagsName =
            bag.NestedBags
            |> Seq.map (fun nested -> nested.Name)

        let bagsToCheck =
            allBags
            |> Seq.filter (fun bagToCheck -> nestedBagsName |> Seq.contains bagToCheck.Name)
            |> Seq.map (fun bagToCheck -> (bagToCheck.Name, bagToCheck))
            |> Map.ofSeq

        bag.NestedBags
        |> Seq.map (fun nestedBag ->
            bagsToCheck.Item nestedBag.Name
            |> getBagsCount
            |> (*) nestedBag.Amount
            |> (+) nestedBag.Amount)
        |> Seq.sum

    allBags
    |> Seq.find (fun bag -> bag.Name = "shiny gold")
    |> getBagsCount

printfn $"Answer for part 2 is {part2}"