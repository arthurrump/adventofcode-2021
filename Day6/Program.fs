open System
open System.IO

module Array =
    let updateWith index f (array: 't[]) =
        Array.updateAt index (f (array[index])) array

let initialFish =
    File.ReadAllText("input.txt").Split(",") 
    |> Array.map int
    |> Array.fold (fun arr i -> arr |> Array.updateAt i (arr.[i] + 1I)) (Array.create 9 0I)

let step (fish: bigint[]) =
    let newFishCount = fish[0]
    Array.append (fish |> Array.tail) [| newFishCount |]
    |> Array.updateWith 6 (fun i -> i + newFishCount)

let rec loop until day fish =
    if day < until then
        loop until (day + 1) (step fish)
    else
        fish

#if part1
loop 80 0 initialFish
#else
loop 256 0 initialFish
#endif
|> Array.sum
|> printfn "%O"
