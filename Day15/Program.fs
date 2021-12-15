open System
open System.IO

let riskMap =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line ->
        line.ToCharArray() 
        |> Array.map (fun c -> int c - int '0'))

let dijkstra (riskmap: int[][]) =
    let rec step destination current visited distances =
        if destination = current then
            distances
        else
            let (y, x) = current
            let currentDistance = distances |> Map.find (y, x)
            let neighbours = 
                [ if y < riskmap.Length - 1 && not (visited |> Set.contains (y + 1, x)) then yield (y + 1, x)
                  if x < riskmap[0].Length - 1 && not (visited |> Set.contains (y, x + 1)) then yield (y, x + 1) ]
            let distances = 
                neighbours 
                |> List.fold (fun distances (y, x) -> 
                    let edge = riskmap[y][x]
                    match distances |> Map.tryFind (y, x) with
                    | Some d when d > currentDistance + edge -> 
                        distances |> Map.add (y, x) (currentDistance + edge)
                    | None ->
                        distances |> Map.add (y, x) (currentDistance + edge)
                    | _ ->
                        distances) distances
            let visited = visited |> Set.add current
            let current = 
                distances 
                |> Map.filter (fun coord _ -> not (visited |> Set.contains coord)) 
                |> Map.toSeq
                |> Seq.minBy snd 
                |> fst
            step destination current visited distances
    let current = (0, 0)
    let destination = (riskmap.Length - 1, riskmap[0].Length - 1)
    let distances = step destination current Set.empty (Map.empty |> Map.add current 0)
    distances |> Map.find destination

dijkstra riskMap
|> printfn "%d"
