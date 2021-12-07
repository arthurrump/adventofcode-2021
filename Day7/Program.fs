open System
open System.IO

let input =
    File.ReadAllText("input.txt").Split(",") 
    |> Array.map int

let minHeight = input |> Array.min
let maxHeight = input |> Array.max

let fuelCost height target =
#if part1
    abs (height - target)
#else
    let n = abs (height - target)
    n * (n + 1) / 2
#endif

[ minHeight..maxHeight ]
|> List.map (fun target -> input |> Array.map (fun height -> fuelCost height target) |> Array.sum)
|> List.min
|> printfn "%d"
