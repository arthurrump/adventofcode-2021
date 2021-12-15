open System
open System.Collections.Generic
open System.IO

let riskmap =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line ->
        line.ToCharArray() 
        |> Array.map (fun c -> int c - int '0'))

let dijkstra (riskmap: int[][]) =
    let destination = (riskmap.Length - 1, riskmap[0].Length - 1)
    let unvisited = PriorityQueue()
    let distances: voption<int>[][] = [| for _ in 0..(riskmap.Length - 1) -> Array.create riskmap[0].Length ValueNone |]
    let mutable current = (0, 0)
    distances[0][0] <- ValueSome 0
    unvisited.Enqueue(current, 0)
    while current <> destination do
        current <- unvisited.Dequeue()
        let (y, x) = current
        let currentDistance = distances[y][x] |> ValueOption.get
        let neighbours = 
            [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ] 
            |> List.filter (fun (y, x) -> y >= 0 && y < riskmap.Length && x >= 0 && x < riskmap[y].Length)
        for (ny, nx) in neighbours do
            let edge = riskmap[ny][nx]
            let dist = currentDistance + edge
            match distances[ny][nx] with
            | ValueSome d when d > dist ->
                distances[ny][nx] <- ValueSome dist
                unvisited.Enqueue((ny, nx), dist)
            | ValueNone ->
                distances[ny][nx] <- ValueSome dist
                unvisited.Enqueue((ny, nx), dist)
            | _ -> 
                ()
    distances[fst destination][snd destination] |> ValueOption.get

#if part1

dijkstra riskmap
|> printfn "%d"

#else

let extendedRiskmap =
    [| for yi in 0..4 do
        for y in 0..(riskmap.Length - 1) ->
            [| for xi in 0..4 do
                for x in 0..(riskmap[0].Length - 1) ->
                    (riskmap[y][x] + yi + xi - 1) % 9 + 1 |] |]

// for row in extendedRiskmap do
//     for cell in row do
//         printf "%d" cell
//     printfn ""

dijkstra extendedRiskmap
|> printfn "%d"

#endif
