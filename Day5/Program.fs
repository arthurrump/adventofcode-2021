open System
open System.IO

type Point = int * int
type Line = Point * Point
module Line =
    let inline toString ((startX, startY), (endX, endY)) = sprintf "%d,%d -> %d,%d" startX startY endX endY
    let inline isHorizontal (((startX, _), (endX, _)): Line) = startX = endX
    let inline isVertical (((_, startY), (_, endY)): Line) = startY = endY

type Grid = int[][]
module Grid =
    let incr (x, y) grid =
        grid |> Array.updateAt x (grid.[x] |> Array.updateAt y (grid.[x][y] + 1))

let vents: Line[] =
    File.ReadAllLines ("input.txt")
    |> Array.map (fun str -> 
        let [| start; end' |] = str.Split(" -> ")
        let [| startX; startY |] = start.Split(",")
        let [| endX; endY |] = end'.Split(",")
        ((int startX, int startY), (int endX, int endY)))

let (maxX, maxY) =
    vents
    |> Array.fold (fun (maxX, maxY) ((startX, startY), (endX, endY)) ->
        (max maxX (max startX endX), max maxY (max startY endY))) (0, 0)

let emptyGrid: Grid =
    Array.create (maxX + 1) (Array.create (maxY + 1) 0)

let resultGrid =
    vents
#if part1
    |> Array.filter (fun line -> Line.isHorizontal line || Line.isVertical line)
#endif
    |> Array.fold (fun grid line ->
        if Line.isHorizontal line then
            let ((x, startY), (_, endY)) = line
            [ (min startY endY)..(max startY endY) ] |> List.fold (fun grid y -> grid |> Grid.incr (x, y)) grid
        else if Line.isVertical line then
            let ((startX, y), (endX, _)) = line
            [ (min startX endX)..(max startX endX) ] |> List.fold (fun grid x -> grid |> Grid.incr (x, y)) grid
        else
            let ((startX, startY), (endX, endY)) = line
            List.zip [ startX..(if startX < endX then 1 else -1)..endX ] [ startY..(if startY < endY then 1 else -1)..endY ]
            |> List.fold (fun grid (x, y) -> grid |> Grid.incr (x, y)) grid) emptyGrid

// for line in resultGrid |> Array.transpose do
//     for cell in line do
//         if cell > 0 then printf "%d" cell else printf "."
//     printfn ""

resultGrid
|> Array.collect id
|> Array.filter (fun count -> count > 1)
|> Array.length
|> printfn "%d"
