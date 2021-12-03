open System
open System.IO

let input = File.ReadAllLines ("input.txt") |> Seq.ofArray

let parseBin str = Convert.ToInt32(str, 2)

#if part1

let ge selector (input: string seq) =
    input
    |> Seq.map (fun str -> str.ToCharArray())
    |> Seq.transpose
    |> Seq.map (fun nthBits -> nthBits |> Seq.countBy id |> selector |> fst)
    |> (Seq.toArray >> String)
    |> parseBin

let gamma input = ge (Seq.maxBy snd) input
let epsilon input = ge (Seq.minBy snd) input

printfn "%d" ((gamma input) * (epsilon input))

#else

let countAtIndex numbers index =
    numbers |> Seq.transpose |> Seq.item index |> Seq.countBy id |> Map.ofSeq

let runProcess selector (numbers: string seq) =
    let mutable result = numbers |> Seq.toList
    let mutable index = 0
    while result.Length > 1 do
        let selected = selector result index
        result <- result |> List.filter (fun x -> x.[index] = selected)
        index <- index + 1
    result |> List.exactlyOne |> parseBin

let maxSelector numbers index =
    let counts = countAtIndex numbers index
    if counts.['0'] = counts.['1'] 
    then '1'
    else if counts.['0'] > counts.['1']
    then '0'
    else '1'

let minSelector numbers index =
    let counts = countAtIndex numbers index
    if counts.['0'] = counts.['1'] 
    then '0'
    else if counts.['0'] < counts.['1']
    then '0'
    else '1'

let oxygenGenRating = runProcess maxSelector input
let carbonDioxideScrubRating = runProcess minSelector input

printfn "%d" (oxygenGenRating * carbonDioxideScrubRating)

#endif