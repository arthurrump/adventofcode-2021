open System
open System.IO

let octomap =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun line ->
        line.ToCharArray() 
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toList)
    |> Seq.toList

let printOctomap octomap =
    for row in octomap do
        for level in row do
            printf "%d" level
        printfn ""
    printfn ""

let increment octomap =
    octomap |> List.map (List.map (fun i -> i + 1))

let flashCount octomap =
    octomap |> List.collect id |> List.filter (fun level -> level > 9) |> List.length

let singleFlash octomap =
    octomap 
    |> List.mapi (fun y row -> 
        row
        |> List.mapi (fun x octolevel ->
            if octolevel = 0 || octolevel > 9 then // A level is only zero if it already flashed this round
                0
            else 
                let adjacentFlashes = 
                    octomap[y-1..y+1] 
                    |> List.collect (fun row -> row[x-1..x+1]) 
                    |> List.filter (fun level -> level > 9) 
                    |> List.length
                octolevel + adjacentFlashes
        )
    )

let flash octomap =
    let rec flash count octomap =
        let c = flashCount octomap
        if c = 0 then
            octomap, count
        else
            flash (count + c) (singleFlash octomap)
    flash 0 octomap

let step =
    increment >> flash

#if part1

let iterate n octomap =
    let rec iterate n flashCount octomap =
        if n = 0 then 
            octomap, flashCount
        else
            let octomap, newFlashes = step octomap
            iterate (n - 1) (flashCount + newFlashes) octomap
    iterate n 0 octomap

let result, flashes = iterate 100 octomap
printOctomap result
printfn "%d" flashes

#else

let iterateUntilAllFlash octomap =
    let rec iterate n octomap =
        if octomap |> List.collect id |> List.forall (fun level -> level = 0) then
            n
        else
            iterate (n + 1) (fst (step octomap))
    iterate 0 octomap

iterateUntilAllFlash octomap
|> printfn "%d"

#endif