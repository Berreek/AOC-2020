open System.Linq
open System.IO

type PolicyPassword =
    { SecondNumber: int
      FirstNumber: int
      Letter: char
      Password: string }

let getPolicies () =
    let map (line: string) =
        let splittedValues = line.Split ' '
        let policyInfo = splittedValues.[0].Split '-'

        { Password = splittedValues.[2]
          Letter = splittedValues.[1].[0]
          SecondNumber = int policyInfo.[1]
          FirstNumber = int policyInfo.[0] }

    File.ReadAllLines "./Day2/Input.txt"
    |> Seq.map map

let isPolicyValidForPart1 policy =
    let charOccurrence =
        policy.Password.Count(fun x -> x = policy.Letter)

    charOccurrence <= policy.SecondNumber
    && charOccurrence >= policy.FirstNumber

let isPolicyValidForPart2 policy =
    match (policy.Password.[policy.FirstNumber - 1] = policy.Letter,
           policy.Password.[policy.SecondNumber - 1] = policy.Letter) with
    | (true, false) -> true
    | (false, true) -> true
    | _ -> false

let getResult policyCheck =
    getPolicies ()
    |> Seq.filter policyCheck
    |> Seq.length

let resultOfPart1 = getResult isPolicyValidForPart1
printfn $"Answer of part 1 is {resultOfPart1}"

let resultOfPart2 = getResult isPolicyValidForPart2
printfn $"Answer of part 2 is {resultOfPart2}"