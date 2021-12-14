open System
open System.IO

let input =
    File.ReadAllLines("input.txt")

type Polymer = ((char * char) * bigint) list

let template: Polymer = 
    ("^" + input[0] + "^").ToCharArray() 
    |> Array.pairwise
    |> Array.countBy id
    |> Array.map (fun (pair, count) -> (pair, bigint count))
    |> Array.toList

let rules =
    input[2..]
    |> Array.map (fun str -> 
        let [| pair; insert |] = str.Split(" -> ")
        let [| left; right |] = pair.ToCharArray()
        ((left, right), char insert))
    |> Map.ofArray

let step rules (polymer: Polymer) : Polymer =
    [ for ((left, right), count) in polymer do
        match rules |> Map.tryFind (left, right) with
        | Some insert ->
            yield ((left, insert), count)
            yield ((insert, right), count)
        | None ->
            yield ((left, right), count) ]
    |> List.groupBy fst
    |> List.map (fun (pair, values) -> (pair, values |> List.map snd |> List.sum))

let rec runProcess n rules polymer =
    if n = 0 then polymer
    else runProcess (n-1) rules (step rules polymer)    

#if part1
runProcess 10 rules template
#else
runProcess 40 rules template
#endif
|> List.collect (fun ((left, right), count) -> [ left, count; right, count ])
|> List.groupBy fst
|> List.filter (fun (ch, _) -> ch <> '^')
|> List.map (fun (_, values) -> (values |> List.map snd |> List.sum) / 2I)
|> List.sort
|> fun l -> List.last l - List.head l
|> printfn "%A"
