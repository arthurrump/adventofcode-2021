open System
open System.IO

type Cucumber = East | South
type CuMap = Set<(int * int) * Cucumber>

let parseLocation = function
    | '.' -> None
    | '>' -> Some East
    | 'v' -> Some South

let input =
    File.ReadAllLines("input.txt")
    |> Array.map (fun str -> str.ToCharArray())

let height = input.Length
let width = input[0].Length

let map: CuMap =
    input
    |> Array.indexed
    |> Array.collect (fun (y, row) ->
        row
        |> Array.indexed
        |> Array.choose (fun (x, c) -> parseLocation c |> Option.map (fun c -> ((y, x), c))))
    |> set

let printMap map =
    for y in 0..height-1 do
        for x in 0..width-1 do
            if Set.contains ((y, x), East) map then printf ">"
            else if Set.contains ((y, x), South) map then printf "v"
            else printf "."
        printfn ""

let nextPosition cucumber (y, x) =
    match cucumber with
    | East -> (y, (x + 1) % width)
    | South -> ((y + 1) % height, x)

let moveOfType cucumber (map: CuMap) : CuMap =
    map |> Set.map (fun ((y, x), c) -> 
        if c = cucumber then
            let (ny, nx) = nextPosition c (y, x)
            if map |> Set.contains ((ny, nx), East) || map |> Set.contains ((ny, nx), South) then
                ((y, x), c)
            else
                ((ny, nx), c)
        else ((y, x), c))

let step =
    moveOfType East >> moveOfType South

let rec loopUntilNotMoving counter (map: CuMap) =
    let newMap = step map
    if newMap = map
    then counter + 1
    else loopUntilNotMoving (counter + 1) newMap

loopUntilNotMoving 0 map
|> printfn "%d"
