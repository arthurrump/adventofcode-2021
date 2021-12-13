open System
open System.IO

let input =
    File.ReadAllLines("input.txt")

let printPoints points =
    let maxX = points |> List.map fst |> List.max
    let maxY = points |> List.map snd |> List.max
    for y in 0..maxY do
        for x in 0..maxX do
            if points |> List.contains (x, y) 
            then printf "#"
            else printf "."
        printfn ""
    printfn ""

let tee f x = f x; x 

let points =
    input 
    |> Array.takeWhile (not << String.IsNullOrWhiteSpace)
    |> Array.map (fun str -> let [| x; y |] = str.Split(',') in (int x, int y))
    |> Array.toList

let folds =
    input
    |> Array.skipWhile (fun str -> not (str.StartsWith("fold along")))
    |> Array.map (fun str -> 
        let [| dir; line |] = str.Substring("fold along ".Length).Split('=')
        (dir, int line))
    |> Array.toList

let foldUp line points =
    points
    |> List.map (fun (x, y) ->
        if y >= line
        then (x, line - (y - line))
        else (x, y))
    |> List.distinct

let foldLeft line points =
    points
    |> List.map (fun (x, y) ->
        if x >= line
        then (line - (x - line), y)
        else (x, y))
    |> List.distinct

let fold (dir, line) points =
    if dir = "y"
    then foldUp line points
    else foldLeft line points

#if part1

points
|> fold folds[0]
|> List.length
|> printfn "%d"

#else

folds
|> List.fold (fun points folding -> fold folding points) points
|> printPoints

#endif