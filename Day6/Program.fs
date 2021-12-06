open System
open System.IO

let initialFish =
    File.ReadAllText("input.txt").Split(",") |> Array.toList |> List.map int

let step fish =
    let newFishCount = fish |> List.filter (fun f -> f = 0) |> List.length
    fish 
    |> List.map (fun f -> if f = 0 then 6 else f - 1) 
    |> List.append (List.init newFishCount (fun _ -> 8))

let rec loop until day fish =
    if day < until then
        printfn "Day %d: %d fish" day (fish |> List.length)
        loop until (day + 1) (step fish)
    else
        fish

loop 80 0 initialFish
|> List.length
|> printfn "%d"
