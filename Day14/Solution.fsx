open System
open System.IO

type Command =
    | ChangeMask of string
    | WriteToMemory of int64 * int64

let getCommand (line: string) =
    if line.StartsWith("mask") then
        ChangeMask(line.Split('=').[1].TrimStart())
    else
        let splitByEqual = line.Split('=')

        let index =
            splitByEqual.[0].Split('[').[1].Split(']').[0]
            |> int64

        let value = splitByEqual.[1].TrimStart() |> int64
        WriteToMemory(index, value)

let intToBinary (value: int64) =
    Convert.ToString(value, 2).PadLeft(36, '0')

let binaryToInt value =
    Convert.ToInt64(value, 2)

let input =
    File.ReadAllLines "./Day14/Input.txt"
    |> Seq.map getCommand
    |> Seq.toArray

let maskForPart1 (value: string) i char =
    match char with
    | '1' -> '1'
    | '0' -> '0'
    | _ -> value.[i]

let maskForPart2 (value: string) i char =
    match char with
    | '1' -> '1'
    | '0' -> value.[i]
    | _ -> 'x'

let writeToMemoryPart1 (memory: Map<int64, int64>) memoryIndex valueAfterUsingMask =
    memory.Add(memoryIndex, valueAfterUsingMask |> binaryToInt)

let writeToMemoryPart2 (memory: Map<int64, int64>) memoryIndex valueAfterUsingMask =
    let possibilities =
        valueAfterUsingMask
        |> Seq.filter ((=) 'x')
        |> Seq.length
        |> pown 2
    
    memory.Add(memoryIndex, valueAfterUsingMask |> binaryToInt)

let rec processCommand (memory: Map<int64, int64>)
                       currentIndex
                       (commands: Command array)
                       currentMask
                       maskFunction
                       writeToMemoryFunction
                       =
    let writeToMemory memoryIndex valueToWrite =
        let valueToWriteAsBinary = intToBinary valueToWrite

        let valueAfterUsingMask =
            currentMask
            |> Seq.mapi (maskFunction valueToWriteAsBinary)
            |> Seq.toArray
            |> String

        let memoryUpdated =
            writeToMemoryFunction memory memoryIndex valueAfterUsingMask

        processCommand memoryUpdated (currentIndex + 1) commands currentMask maskFunction writeToMemoryFunction

    if currentIndex = commands.Length then
        memory
    else
        match commands.[currentIndex] with
        | ChangeMask changeMask ->
            processCommand memory (currentIndex + 1) commands changeMask maskFunction writeToMemoryFunction
        | WriteToMemory (memoryIndex, valueToWrite) -> writeToMemory memoryIndex valueToWrite
//
//let rec processCommandForPart2 (memory: Map<int64, int64>) currentIndex (commands: Command array) currentMask =
//    let writeToAllPossibleMemories valueAfterUsingMask (valueToWrite: int64) =
//        let getMemIndex possibility possibilitiesCount =
//            valueAfterUsingMask
//            |> Seq.scan (fun (counter, _) c ->
//                match c with
//                | '1' -> (counter, '1')
//                | '0' -> (counter, '0')
//                | _ ->
//                    if counter < possibilitiesCount
//                    then
//                        printfn $"Counter : {counter} Possibility {possibility} count {possibilitiesCount}"
//                        let newValue = match ((possibility - counter) % 2) with
//                        | 1 -> '1'
//                        | _ -> '0'
//                        (counter + 1, newValue)
//                    else (counter + 1, '0')) (0, ' ')
//            |> Seq.map snd
//            |> Seq.toArray
//            |> String
//            |> fun value -> value.TrimStart() |> binaryToInt
//
//        let possibilities =
//            valueAfterUsingMask
//            |> Seq.filter ((=) 'x')
//            |> Seq.length
//
//        let possibilitiesCount = possibilities |> pown 2
//
//        Seq.init possibilitiesCount id
//        |> Seq.fold (fun (state : Map<int64,int64>) i ->
//            let memIndex = getMemIndex i possibilities
//            printfn $"For possibility count {i} value is {memIndex}"
//            state.Add (memIndex, valueToWrite)) memory
//
//    let writeToMemory memoryIndex =
//        let valueToWriteAsBinary = intToBinary memoryIndex
//
////        printfn $"Value before mask {valueToWriteAsBinary}"
////        printfn $"Mask {currentMask}"
////
//        let valueAfterUsingMask =
//            maskForPart2 valueToWriteAsBinary currentMask
//
////        printfn $"Value after using mask {valueAfterUsingMask}"
//        let memoryUpdated =
//            writeToAllPossibleMemories valueAfterUsingMask memoryIndex
//
//        processCommandForPart2 memoryUpdated (currentIndex + 1) commands currentMask
//
//    if currentIndex = commands.Length then
//        memory
//    else
//        match commands.[currentIndex] with
//        | ChangeMask changeMask -> processCommandForPart2 memory (currentIndex + 1) commands changeMask
//        | WriteToMemory (memoryIndex, _) -> writeToMemory memoryIndex


let calculate maskFunction writeToMemoryFunction =
    processCommand Map.empty 0 input "" maskFunction writeToMemoryFunction
    |> Map.toSeq
    |> Seq.sumBy snd

let part1 = calculate maskForPart1 writeToMemoryPart1

printfn $"Answer for part 1 is {part1}"

let part2 = calculate maskForPart2 writeToMemoryPart2

printfn $"Answer for part 2 is {part2}"