open System
open System.IO

let heightmap =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line ->
        line.ToCharArray() 
        |> Array.map (fun c -> int c - int '0'))

let lowPoints =
    [ for y in 0..(heightmap.Length - 1) do
        for x in 0..(heightmap[y].Length - 1) ->
            let surrounding = heightmap[y-1..y+1] |> Array.collect (fun row -> row[x-1..x+1])
            let lowestInSurrounding = surrounding |> Array.min
            if heightmap[y][x] = lowestInSurrounding 
            then Some (y, x)
            else None ]
    |> List.choose id

#if part1

lowPoints
|> List.map (fun (y, x) -> heightmap[y][x])
|> List.sum
|> printfn "%d"

#else

let height = heightmap.Length
let width = heightmap[0].Length

type Basin = Basin of int | None | Unknown
let basinmap = [| for _ in 0..(height - 1) do Array.create width Unknown |]

for (i, (y, x)) in lowPoints |> Seq.indexed do
    basinmap[y][x] <- Basin i

for y in 0..(height - 1) do
    for x in 0..(width - 1) do
        if heightmap[y][x] = 9 then
            basinmap[y][x] <- None

while basinmap |> Array.exists (Array.exists (fun b -> b = Unknown)) do
    for y in 0..(height - 1) do
        for x in 0..(width - 1) do
            match basinmap[y][x] with
            | Basin b ->
                for (y, x) in [ (y-1, x); (y+1, x); (y, x-1); (y, x+1) ] do
                    if y >= 0 && y < height && x >= 0 && x < width && basinmap[y][x] = Unknown then
                        basinmap[y][x] <- Basin b
            | _ -> ()

// for row in basinmap do
//     for cell in row do
//         match cell with
//         | Basin b -> printf "%d" b
//         | None -> printf "#"
//         | Unknown -> printf "?"
//     printfn ""

basinmap
|> Seq.collect id
|> Seq.choose (function Basin b -> Some b | _ -> Option.None)
|> Seq.countBy id
|> Seq.map snd
|> Seq.sortDescending
|> Seq.take 3
|> Seq.reduce (*)
|> printfn "%d"

#endif