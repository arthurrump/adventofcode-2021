// | N | # | Segments |
// |---|---|----------|
// | 0 | 6 | abcefg   |
// | 1 | 2 | cf       |
// | 2 | 5 | acdeg    |
// | 3 | 5 | acdfg    |
// | 4 | 4 | bcdf     |
// | 5 | 5 | abdfg    |
// | 6 | 6 | abdefg   |
// | 7 | 3 | acf      |
// | 8 | 7 | abcdefg  |
// | 9 | 6 | abcdfg   |

open System
open System.IO

let input =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line ->
        let [| patterns; output |] = line.Split(" | ")
        (patterns.Split(" "), output.Split(" ")))

let segmentsToNumber (segments: char[]) =
    match segments |> Array.sort with
    | [| 'a'; 'b'; 'c'; 'e'; 'f'; 'g' |] -> 0
    | [| 'c'; 'f' |] -> 1
    | [| 'a'; 'c'; 'd'; 'e'; 'g' |] -> 2
    | [| 'a'; 'c'; 'd'; 'f'; 'g' |] -> 3
    | [| 'b'; 'c'; 'd'; 'f' |] -> 4
    | [| 'a'; 'b'; 'd'; 'f'; 'g' |] -> 5
    | [| 'a'; 'b'; 'd'; 'e'; 'f'; 'g' |] -> 6
    | [| 'a'; 'c'; 'f' |] -> 7
    | [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' |] -> 8
    | [| 'a'; 'b'; 'c'; 'd'; 'f'; 'g' |] -> 9

let getPotentialMappings (segments: char[]) =
    match segments.Length with
    | 2 -> // 1: cf 
        segments |> Array.map (fun s -> s, set [ 'c'; 'f' ])
    | 3 -> // 7: acf
        segments |> Array.map (fun s -> s, set [ 'a'; 'c'; 'f' ])
    | 4 -> // 4: bcdf
        segments |> Array.map (fun s -> s, set [ 'b'; 'c'; 'd'; 'f' ])
    | 5    // 2: acdeg, 3: acdfg, 5: abdfg
    | 6    // 0: abcefg, 6: abdefg, 9: abcdfg
    | 7 -> // 8: abcdefg
        segments |> Array.map (fun s -> s, set [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ])
    |> Map.ofArray

let getSpecial5SegMapping (segmentedNumbers: char[][]) =
    let adg =
        segmentedNumbers
        |> Array.filter (fun segments -> segments.Length = 5)
        |> Array.map set
        |> Array.reduce (Set.intersect)
    [ for s in adg -> s, set [ 'a'; 'd'; 'g' ] ] |> Map.ofList

let getSpecial6SegMapping (segmentedNumbers: char[][]) =
    let abfg =
        segmentedNumbers
        |> Array.filter (fun segments -> segments.Length = 6)
        |> Array.map set
        |> Array.reduce (Set.intersect)
    [ for s in abfg -> s, set [ 'a'; 'b'; 'f'; 'g' ] ] |> Map.ofList

let combineMappings m1 m2 =
    let allKeys = [ yield! m1 |> Map.keys; yield! m2 |> Map.keys ] |> List.distinct
    [ for key in allKeys ->
        let newSet =
            match Map.tryFind key m1, Map.tryFind key m2 with
            | Some v1, None -> v1
            | None, Some v2 -> v2
            | Some v1, Some v2 -> Set.intersect v1 v2
        key, newSet ]
    |> Map.ofList

let simplifyMapping mapping =
    let reserved = mapping |> Map.filter (fun _ v -> Set.count v = 1) |> Map.values |> Set.unionMany
    mapping |> Map.map (fun _ mapped -> if Set.count mapped > 1 then Set.difference mapped reserved else mapped)

let rec getFinalMapping mapping =
    if mapping |> Map.exists (fun _ v -> Set.count v > 1) then
        getFinalMapping (simplifyMapping mapping)
    else
        mapping |> Map.map (fun _ set -> set |> Set.toList |> List.exactlyOne)

let findMapping (segmentedNumbers: string[]) =
    let segmentedNumbers = segmentedNumbers |> Array.map (fun s -> s.ToCharArray())
    segmentedNumbers
    |> Array.map (fun segments -> getPotentialMappings segments)
    |> Array.append [| getSpecial5SegMapping segmentedNumbers; getSpecial6SegMapping segmentedNumbers |]
    |> Array.reduce combineMappings
    |> getFinalMapping

let mapSegments mapping segments =
    segments |> Array.map (fun s -> mapping |> Map.find s)

let digitArrayToNumber digits =
    digits |> Array.reduce (fun acc s -> acc * 10 + s)

#if part1

input
|> Array.collect snd
|> Array.map (String.length)
|> Array.filter (fun l -> l = 2 || l = 4 || l = 3 || l = 7)
|> Array.length
|> printfn "%d"

#else

input
|> Array.map (fun (patterns, output) ->
    let mapping = findMapping patterns
    output 
    |> Array.map (fun s -> s.ToCharArray() |> mapSegments mapping |> segmentsToNumber)
    |> digitArrayToNumber)
|> Array.sum
|> printfn "%d"

#endif