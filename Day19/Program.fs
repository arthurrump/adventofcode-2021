open System
open System.IO

module Set =
    let addAll from set =
        from |> Seq.fold (fun set elem -> set |> Set.add elem) set

module List =
    let rec allCombinations = function
        | [] -> seq { [] }
        | item::rest ->
            let combinations = allCombinations rest
            seq {
                yield! combinations
                yield! combinations |> Seq.map (fun rest -> item::rest)
            }

type BeaconMeasurement = BeaconMeasurement of x: int * y: int * z: int

module BeaconMeasurement =
    let rotate rotation (BeaconMeasurement (x, y, z)) =
        BeaconMeasurement (rotation (x, y, z))

type Scanner =
    { Id: int
      Measurements: Set<BeaconMeasurement> }

let scanners =
    File.ReadAllLines("input.txt")
    |> Array.fold (fun scanners line ->
        if line.StartsWith("---") then
            let scannerId = int (line.Substring("--- scanner ".Length).TrimEnd([| '-'; ' ' |]))
            { Id = scannerId; Measurements = Set.empty }::scanners
        else if String.IsNullOrWhiteSpace(line) then
            scanners
        else
            let [| x; y; z |] = line.Split(',')
            let m = BeaconMeasurement (int x, int y, int z)
            let s::scanners = scanners
            { s with Measurements = Set.add m s.Measurements }::scanners ) []
    |> List.rev

let distanceSquared (BeaconMeasurement (ox, oy, oz)) (BeaconMeasurement (x, y, z)) =
    float (x - ox) ** 2 + float (y - oy) ** 2 + float (z - oz) ** 2

let direction (BeaconMeasurement (ox, oy, oz)) (BeaconMeasurement (x, y, z)) =
    (x - ox), (y - oy), (z - oz)

let sub (x, y, z) (BeaconMeasurement (x', y', z')) =
    BeaconMeasurement (x' - x, y' - y, z' - z)

let rotationFunctions =
    let flips =
        [ fun (x, y, z) -> (-x, y, z)
          fun (x, y, z) -> (x, -y, z)
          fun (x, y, z) -> (x, y, -z) ]
        |> List.allCombinations
        |> Seq.map (List.fold (fun agg f -> agg >> f) id)
    [ fun (x, y, z) -> (x, y, z)
      fun (x, y, z) -> (x, z, y)
      fun (x, y, z) -> (y, x, z)
      fun (x, y, z) -> (y, z, x)
      fun (x, y, z) -> (z, x, y)
      fun (x, y, z) -> (z, y, x) ]
    |> Seq.collect (fun f -> flips |> Seq.map (fun flip -> flip >> f))

let beaconDistances originBeacon otherMeasurements =
    otherMeasurements |> Set.map (direction originBeacon)

let beaconDistanceMap measurements =
    seq { for origin in measurements -> (origin, beaconDistances origin (measurements |> Set.remove origin)) }

let findOverlap m1 m2 =
    let m1BeaconDistanceMap = beaconDistanceMap m1
    let m2BeaconDistanceMap = beaconDistanceMap m2
    Seq.allPairs m1BeaconDistanceMap m2BeaconDistanceMap
    |> Seq.collect (fun maps -> seq { for r in rotationFunctions -> (r, maps)})
    |> Seq.tryPick (fun (rotation, ((origin1, distances1), (origin2, distances2))) ->
        if Set.intersect distances1 (distances2 |> Set.map rotation) |> Set.count >= 11
        then Some (origin1, origin2, rotation)
        else None)

let unifyScanners (scanners: Scanner list) =
    let rec unifyScanners unifiedMeasurements scannersTodo =
        if scannersTodo |> Set.isEmpty then
            unifiedMeasurements
        else
            let scanner, (unifiedBeaconCoords, scannerBeaconCoords, rotation) =
                scannersTodo
                |> Seq.pick (fun s -> 
                    findOverlap unifiedMeasurements s.Measurements
                    |> Option.map (fun mapping -> s, mapping))
            printfn "Unifying %d: %A to %A" scanner.Id scannerBeaconCoords unifiedBeaconCoords
            let diff = direction unifiedBeaconCoords (BeaconMeasurement.rotate rotation scannerBeaconCoords)
            let unifiedMeasurements = unifiedMeasurements |> Set.addAll (scanner.Measurements |> Set.map (BeaconMeasurement.rotate rotation >> sub diff))
            printfn "Found %d unified beacons" (unifiedMeasurements |> Set.count)
            unifyScanners unifiedMeasurements (scannersTodo |> Set.remove scanner)
    unifyScanners scanners[0].Measurements (set scanners[1..])

unifyScanners scanners
|> Set.count
|> printfn "%d"
