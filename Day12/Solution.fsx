open System
open System.IO

type Direction =
    | North
    | West
    | East
    | South

type Command =
    | ByDirection of Direction: Direction * Value: int
    | Forward of Value: int
    | Rotate of Value: int

    
type Waypoint =
    { X: int
      Y: int }

type Ship =
    { CurrentDirection: Direction
      X: int
      Y: int }


let input =
    let mapToCommand (line: string) =
        let value = int line.[1..]

        match line.[0] with
        | 'N' -> ByDirection(Direction.North, value)
        | 'S' -> ByDirection(Direction.South, value)
        | 'E' -> ByDirection(Direction.East, value)
        | 'W' -> ByDirection(Direction.West, value)
        | 'L' -> Rotate(value)
        | 'R' -> Rotate(-value)
        | _ -> Forward(value)

    File.ReadAllLines "./Day12/Input.txt"
    |> Seq.map mapToCommand
    
let processCommandForPart1 ship command =
    
    let processDirectionCommand direction value =
        match direction with
        | Direction.North -> { ship with Y = ship.Y + value }
        | Direction.South -> { ship with Y = ship.Y - value }
        | Direction.East -> { ship with X = ship.X + value }
        | Direction.West -> { ship with X = ship.X - value }
    
    let processForwardCommand value =
        match ship.CurrentDirection with
        | Direction.North -> { ship  with Y = ship.Y + value }
        | Direction.South -> { ship  with Y = ship.Y - value }
        | Direction.East -> { ship with X = ship.X + value }
        | Direction.West -> { ship with X = ship.X - value }
    
    let processRotateCommand value =
        let currentDegrees =
            match ship.CurrentDirection with
            | Direction.East -> 0
            | Direction.North -> 90
            | Direction.West -> 180
            | Direction.South -> 270
        match ((value + currentDegrees + 360) % 360) with
        | 0 ->  { ship with CurrentDirection = Direction.East }
        | 90 ->  { ship with CurrentDirection = Direction.North }
        | 180 ->  { ship with CurrentDirection = Direction.West }
        | 270 ->  { ship with CurrentDirection = Direction.South }
        | _ -> ship
        
    
    match command with
    | ByDirection (dir, value) -> processDirectionCommand dir value
    | Forward (value) -> processForwardCommand value
    | Rotate (value) -> processRotateCommand value
    
let processCommandForPart2 (ship : Ship, waypoint: Waypoint) command =
    
    let processDirectionCommand direction value =
        match direction with
        | Direction.North -> (ship, { waypoint with Y = waypoint.Y + value} )
        | Direction.South -> (ship, { waypoint with Y = waypoint.Y - value} )
        | Direction.East -> (ship, { waypoint with X = waypoint.X + value} )
        | Direction.West -> (ship, { waypoint with X = waypoint.X - value} )
    
    let processForwardCommand value =
        let movedShip = { ship with Y = ship.Y + waypoint.Y * value; X = ship.X + waypoint.X * value }
        (movedShip, waypoint)
    
    let processRotateCommand value =
        let newRotation = (value + 360) % 360
        match (newRotation) with
        | 180 ->  (ship, { waypoint with X = -waypoint.X; Y = -waypoint.Y } )  
        | 90 -> (ship, { waypoint with X = -waypoint.Y; Y = waypoint.X } )
        | 270 -> (ship, { waypoint with X = waypoint.Y; Y = -waypoint.X } )
        | _ -> (ship, waypoint)
    
    match command with
    | ByDirection (dir, value) -> processDirectionCommand dir value
    | Forward (value) -> processForwardCommand value  
    | Rotate (value) ->  processRotateCommand value 
      
let part1 =
    let ship = input |> Seq.fold processCommandForPart1 ( { CurrentDirection = Direction.East; X = 0; Y = 0; } )
    Math.Abs(ship.X) + Math.Abs(ship.Y)
    
printfn $"Result for part1 is {part1}"

let part2 =
    let ship = input |> Seq.fold processCommandForPart2 ( { CurrentDirection = Direction.East; X = 0; Y = 0; }, { X = 10; Y = 1 } ) |> fst
    Math.Abs(ship.X) + Math.Abs(ship.Y)
    
printfn $"Result for part2 is {part2}";