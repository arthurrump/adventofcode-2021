open System
open System.IO

module Map =
    let updateFromDefault key f def map =
        let newValue = map |> Map.tryFind key |> Option.defaultValue def |> f
        map |> Map.add key newValue

type String with
    member this.IsLower() =
        this.ToCharArray() |> Array.forall Char.IsLower

    member this.IsUpper() =
        this.ToCharArray() |> Array.forall Char.IsUpper

type CaveSystem = Map<string,Set<string>>

let caveSystem: CaveSystem =
    File.ReadAllLines("input.txt")
    |> Array.map (fun str -> let [| u; v |] = str.Split('-') in (u, v))
    |> Array.fold (fun system (u, v) -> 
        system 
        |> Map.updateFromDefault u (Set.add v) Set.empty 
        |> Map.updateFromDefault v (Set.add u) Set.empty) Map.empty

let noLowercaseDuplicates (list: string list) =
    let onlyLowercase = list |> List.filter (fun str -> str.IsLower())
    (onlyLowercase |> List.length) = (onlyLowercase |> List.distinct |> List.length)

let getContinuedPaths (caveSystem: CaveSystem) (currentCave::startPath) =
    let nextCaves = caveSystem |> Map.find currentCave
    [ for cave in nextCaves do
    #if part1
        if cave.IsUpper() || not (startPath |> List.contains cave) then
    #else
        if cave.IsUpper() 
            || (not (startPath |> List.contains cave))
            || (cave <> "start" && cave <> "end" && (currentCave::startPath) |> noLowercaseDuplicates) then
    #endif
            yield (cave::currentCave::startPath) ]

let getAllPaths (caveSystem: CaveSystem) =
    let rec getAllPaths finishedPaths exploringPaths =
        if exploringPaths |> List.isEmpty then
            finishedPaths
        else
            let newPaths = exploringPaths |> List.collect (getContinuedPaths caveSystem)
            let newFinishedPaths, newExploringPaths = 
                newPaths |> List.partition (fun (cur::_) -> cur = "end")
            getAllPaths (finishedPaths |> List.append newFinishedPaths) newExploringPaths
    getAllPaths [] [ [ "start" ] ]

getAllPaths caveSystem
|> List.length
|> printfn "%d"
