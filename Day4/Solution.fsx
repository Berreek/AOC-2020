open System
open System.IO

type Passport =
    { BirthYear: Option<string>
      IssueYear: Option<string>
      ExpirationYear: Option<string>
      Height: Option<string>
      HairColor: Option<string>
      EyeColor: Option<string>
      PassportId: Option<string> }

let hairColorAllowedChars = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ]

let getPassports () =
    let mapToPassport (line: string) =
        let keyMaps =
            line.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries) 
            |> Seq.map (fun keyValue ->
                let splittedKeyValue = keyValue.Split [| ':' |]
                (splittedKeyValue.[0], splittedKeyValue.[1]))
            |> Map.ofSeq

        { BirthYear = keyMaps.TryFind "byr"
          IssueYear = keyMaps.TryFind "iyr"
          ExpirationYear = keyMaps.TryFind "eyr"
          Height = keyMaps.TryFind "hgt"
          HairColor = keyMaps.TryFind "hcl"
          EyeColor = keyMaps.TryFind "ecl"
          PassportId = keyMaps.TryFind "pid" }

    let input = File.ReadAllLines "./Day4/Input.txt"

    let emptyLinesIndexes =
        input
        |> Seq.indexed
        |> Seq.filter (fun (i, line) -> String.IsNullOrEmpty line)
        |> Seq.map fst
        |> Seq.toArray

    let mergedLines =
        emptyLinesIndexes
        |> Seq.take (emptyLinesIndexes.Length - 1)
        |> Seq.indexed
        |> Seq.map (fun (i, emptyIndex) ->
            String.Join
                (" ",
                 input
                 |> Seq.skip emptyIndex
                 |> Seq.take (emptyLinesIndexes.[i + 1] - emptyIndex)
                 |> Seq.toArray))

    mergedLines |> Seq.map mapToPassport

let validationForPart1 passport =
    passport.BirthYear.IsSome && passport.IssueYear.IsSome && passport.ExpirationYear.IsSome && passport.Height.IsSome && passport.HairColor.IsSome
    && passport.EyeColor.IsSome && passport.PassportId.IsSome
    
let validateBirthYear passport =
    match passport.BirthYear with
    | Some birthYear ->
        match int birthYear with
        | value when value >= 1920 && value <= 2002 -> Ok passport
        | _ -> Error "Wrong birth year"
    | _ -> Error "Wrong birth year"
    
let validateIssueYear passport =
    match passport.IssueYear with
    | Some issueYear ->
        match int issueYear with
        | value when value >= 2010 && value <= 2020 -> Ok passport
        | _ -> Error "Wrong issue year"
    | _ -> Error "Wrong issue year"
    
let validateExpirationYear passport =
    match passport.ExpirationYear with
    | Some expirationYear ->
        match int expirationYear with
        | value when value >= 2020 && value <= 2030 -> Ok passport
        | _ -> Error "Wrong issue year"
    | _ -> Error "Wrong issue year"

let validateHeight passport =
    match passport.Height with
        | Some height ->
            match (Int32.TryParse height.[..^2], height.[^1..]) with
                | ((true, value), "cm" ) when value >= 150 && value <=193 -> Ok passport
                | ((true, value), "in" ) when value >= 59 && value <=76 -> Ok passport
                | _ -> Error "Wrong height"
        | None -> Error "Wrong height"
        
let validateHairColor passport =
    let isHairColorValid (hairColor: string) =
        hairColor.StartsWith "#" && hairColor.[1..] |> String.forall(fun c -> hairColorAllowedChars |> Seq.contains(c))
    match passport.HairColor with
        | Some hairColor when isHairColorValid hairColor -> Ok passport
        | _ -> Error "Wrong hair color"
        
let validateEyeColor passport =
     match passport.EyeColor with
        | Some eyeColor ->
            match eyeColor with
            | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> Ok passport
            | _ -> Error "Wrong eye color"
        | None -> Error "Wrong eye color"
        
let validatePassportId passport =
    let isIdValid (id : string) =
        id.Length = 9 && id |> String.forall(Char.IsDigit)
    match passport.PassportId with
        | Some passportId when isIdValid passportId -> Ok passport
        | _ -> Error "Wrong passport id"

let validationForPart2 passport =
    let result = validateBirthYear passport
                 |> Result.bind validateIssueYear
                 |> Result.bind validateExpirationYear
                 |> Result.bind validateHeight
                 |> Result.bind validateHairColor
                 |> Result.bind validateEyeColor
                 |> Result.bind validatePassportId
    match result with
    | Ok _ -> true
    | _ -> false

let resultForPart1 = getPassports () |> Seq.filter validationForPart1 |> Seq.length
printfn $"Result for part 1 is {resultForPart1}"

let resultForPart2 = getPassports () |> Seq.filter validationForPart2 |> Seq.length
printfn $"Result for part 2 is {resultForPart2}"
