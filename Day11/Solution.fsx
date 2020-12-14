open System.IO

type Seat =
    | Taken
    | Empty
    | NotExisting

let input =
    File.ReadAllLines "./Day11/Input.txt"
    |> Seq.map (fun line ->
        line.ToCharArray()
        |> Seq.map (fun char ->
            match char with
            | 'L' -> Seat.Empty
            | _ -> Seat.NotExisting))
    |> array2D

let getSeatsAroundToCheck row column seats =
    let rowLength = seats |> Array2D.length1
    let columnLength = seats |> Array2D.length2

    [ (row - 1, column - 1)
      (row - 1, column)
      (row - 1, column + 1)
      (row, column - 1)
      (row, column + 1)
      (row + 1, column - 1)
      (row + 1, column)
      (row + 1, column + 1) ]
    |> Seq.filter (fun (r, c) ->
        r >= 0
        && r < rowLength
        && c >= 0
        && c < columnLength)
    |> Seq.map (fun (r, c) -> seats.[r, c])

let getSeatsAroundThatCanBeSeenToCheck row column seats =
    let rowLength = seats |> Array2D.length1
    let columnLength = seats |> Array2D.length2

    let rec getSeatThatCanBeSeen row column rowOffset columnOffset =
        match (row + rowOffset, column + columnOffset) with
        | (possibleNotExistingRow, possibleNotExistingColumn) when possibleNotExistingRow < 0
                                                                   || possibleNotExistingRow >= rowLength
                                                                   || possibleNotExistingColumn < 0
                                                                   || possibleNotExistingColumn >= columnLength ->
            Seat.NotExisting
        | (rowToCheck, columnToCheck) when seats.[rowToCheck, columnToCheck] = Seat.NotExisting ->
            getSeatThatCanBeSeen rowToCheck columnToCheck rowOffset columnOffset
        | _ -> seats.[row + rowOffset, column + columnOffset]

    [ getSeatThatCanBeSeen row column -1 -1
      getSeatThatCanBeSeen row column -1 0
      getSeatThatCanBeSeen row column -1 1
      getSeatThatCanBeSeen row column 0 -1
      getSeatThatCanBeSeen row column 0 1
      getSeatThatCanBeSeen row column 1 -1
      getSeatThatCanBeSeen row column 1 0
      getSeatThatCanBeSeen row column 1 1 ]

let revaluateSeatState maxNumberOfSeatsTaken seatsAroundProvider seats row column value =
    match value with
    | Seat.NotExisting -> Seat.NotExisting
    | Seat.Taken when seatsAroundProvider row column seats
                      |> Seq.filter (fun seatAround -> seatAround = Seat.Taken)
                      |> Seq.length
                      |> (<=) maxNumberOfSeatsTaken -> Seat.Empty
    | Seat.Empty when seatsAroundProvider row column seats
                      |> Seq.exists (fun seatAround -> seatAround = Seat.Taken)
                      |> not -> Seat.Taken
    | _ -> value

let getOccupiedSeats revaluateSeatFunction =
    let rec processSeats previousState currentState =
        let verifyIfSeatsAreTheSame =
            let previousStateAsSeq = previousState |> Seq.cast<Seat>
            let currentStateAsSeq = currentState |> Seq.cast<Seat>
            Seq.forall2 (=) previousStateAsSeq currentStateAsSeq

        if verifyIfSeatsAreTheSame then
            (previousState, currentState)
        else
            let copyOfCurrentState = currentState |> Array2D.copy

            let nextState =
                copyOfCurrentState
                |> Array2D.mapi (revaluateSeatFunction currentState)

            processSeats currentState nextState

    let initialPreviousState =
        (array2D [ [ Seat.NotExisting ]
                   [ Seat.NotExisting ] ])

    processSeats initialPreviousState input
    |> snd
    |> Seq.cast<Seat>
    |> Seq.filter ((=) Seat.Taken)
    |> Seq.length

let revaluateSeatStateForPart1 =
    revaluateSeatState 4 getSeatsAroundToCheck

let revaluateSeatStateForPart2 =
    revaluateSeatState 5 getSeatsAroundThatCanBeSeenToCheck

let part1 =
    getOccupiedSeats revaluateSeatStateForPart1

printfn $"Answer for part1 is {part1}"

let part2 =
    getOccupiedSeats revaluateSeatStateForPart2

printfn $"Answer for part1 is {part2}"