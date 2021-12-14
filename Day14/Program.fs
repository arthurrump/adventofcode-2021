open System
open System.IO

let input =
    File.ReadAllLines("input.txt")

let template = input[0].ToCharArray() |> Array.toList
let rules =
    input[2..]
    |> Array.map (fun str -> 
        let [| pair; insert |] = str.Split(" -> ")
        let [| left; right |] = pair.ToCharArray()
        ((left, right), char insert))
    |> Map.ofArray

let insertElements polymer insertions =
    let rec insertElements result polymer insertions =
        match polymer, insertions with
        | p::polymer, i::insertions -> insertElements (i::p::result) polymer insertions
        | [ p ], [] -> List.rev (p::result)
    insertElements [] polymer insertions

let step rules (polymer: char list) =
    polymer 
    |> List.pairwise 
    |> List.map (fun pair -> rules |> Map.find pair)
    |> insertElements polymer

let rec runProcess n rules polymer =
    if n = 0 then polymer
    else runProcess (n-1) rules (step rules polymer)    

runProcess 10 rules template
|> List.countBy id
|> List.map snd
|> List.sort
|> fun l -> List.last l - List.head l
|> printfn "%d"
