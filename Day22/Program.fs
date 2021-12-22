open FsCheck
open System
open System.IO

type Cuboid =
    { MinX: int; MaxX: int
      MinY: int; MaxY: int
      MinZ: int; MaxZ: int }
    
    override this.ToString() =
        $"x={this.MinX}..{this.MaxX},y={this.MinY}..{this.MaxY},z={this.MinZ}..{this.MaxZ}"

type Step =
    { State: string
      Cuboid: Cuboid }

    override this.ToString() =
        $"{this.State} {this.Cuboid.ToString()}"

let rebootSteps =
    File.ReadAllLines("input.txt")
    |> Array.map (fun str ->
        let [| state; _; minX; maxX; _; minY; maxY; _; minZ; maxZ |] = 
            str.Split([| ' '; '='; '.'; ',' |], StringSplitOptions.RemoveEmptyEntries)
        { State = state 
          Cuboid = 
            { MinX = int minX; MaxX = int maxX
              MinY = int minY; MaxY = int maxY
              MinZ = int minZ; MaxZ = int maxZ } })
#if part1

let initialize cuboidOfInterest =
    seq {
        for x in cuboidOfInterest.MinX..cuboidOfInterest.MaxX do
            for y in cuboidOfInterest.MinY..cuboidOfInterest.MaxY do
                for z in cuboidOfInterest.MinZ..cuboidOfInterest.MaxZ ->
                    rebootSteps 
                    |> Array.tryFindBack (fun step ->
                        step.Cuboid.MinX <= x && x <= step.Cuboid.MaxX &&
                        step.Cuboid.MinY <= y && y <= step.Cuboid.MaxY &&
                        step.Cuboid.MinZ <= z && z <= step.Cuboid.MaxZ)
                    |> Option.map (fun step -> step.State)
                    |> Option.defaultValue "off" }

initialize { MinX = -50; MaxX = 50; MinY = -50; MaxY = 50; MinZ = -50; MaxZ = 50 }
|> Seq.filter (fun state -> state = "on")
|> Seq.length
|> printfn "%d"

#else

module Cuboid =
    let isValid c =
        c.MinX < c.MaxX && c.MinY < c.MaxY && c.MinZ < c.MaxZ

    let fullyContains smallC largeC =
        largeC.MinX <= smallC.MinX && smallC.MaxX <= largeC.MaxX &&
        largeC.MinY <= smallC.MinY && smallC.MaxY <= largeC.MaxY &&
        largeC.MinZ <= smallC.MinZ && smallC.MaxZ <= largeC.MaxZ

    let overlaps c1 c2 =
        c1.MinX <= c2.MaxX && c2.MinX <= c1.MaxX &&
        c1.MinY <= c2.MaxY && c2.MinY <= c1.MaxY &&
        c1.MinZ <= c2.MaxZ && c2.MinZ <= c1.MaxZ

    let isEmpty c =
        c.MinX >= c.MaxX || c.MinY >= c.MaxY || c.MinZ >= c.MaxZ

    let remove from c =
        if overlaps from c then
            [ if from.MinX < c.MinX then
                yield { from with MaxX = min from.MaxX (c.MinX - 1) }
              if from.MaxX > c.MaxX then
                  yield { from with MinX = max from.MinX (c.MaxX + 1) }
              let from = 
                  { from with 
                      MinX = if from.MinX <= c.MaxX && c.MinX <= from.MaxX then max from.MinX c.MinX else from.MinX
                      MaxX = if from.MinX <= c.MaxX && c.MinX <= from.MaxX then min from.MaxX c.MaxX else from.MaxX }
              if from.MinY < c.MinY then
                  yield { from with MaxY = min from.MaxY (c.MinY - 1) }
              if from.MaxY > c.MaxY then
                  yield { from with MinY = max from.MinY (c.MaxY + 1) }
              let from = 
                  { from with 
                      MinY = if from.MinY <= c.MaxY && c.MinY <= from.MaxY then max from.MinY c.MinY else from.MinY
                      MaxY = if from.MinY <= c.MaxY && c.MinY <= from.MaxY then min from.MaxY c.MaxY else from.MaxY }
              if from.MinZ < c.MinZ then
                  yield { from with MaxZ = min from.MaxZ (c.MinZ - 1) }
              if from.MaxZ > c.MaxZ then
                  yield { from with MinZ = max from.MinZ (c.MaxZ + 1) } ]
            |> List.distinct
        else
            [ from ]

    let count c =
        bigint (c.MaxX - c.MinX + 1) * bigint (c.MaxY - c.MinY + 1) * bigint (c.MaxZ - c.MinZ + 1)

let turnOn onCuboids cuboid =
    let _, overlappingOrSeparate = onCuboids |> List.partition (fun c -> cuboid |> Cuboid.fullyContains c)
    let overlapping, separate = overlappingOrSeparate |> List.partition (Cuboid.overlaps cuboid)
    cuboid::separate @ (overlapping |> List.collect (fun c -> Cuboid.remove c cuboid))

let turnOff onCuboids cuboid =
    let overlaps, unaffected = onCuboids |> List.partition (Cuboid.overlaps cuboid)
    overlaps 
    |> List.collect (fun on -> Cuboid.remove on cuboid) 
    |> List.append unaffected

let executeStep onCuboids step =
    printfn "Running step %O" step
    if step.State = "on"
    then turnOn onCuboids step.Cuboid
    else turnOff onCuboids step.Cuboid

rebootSteps
|> Array.fold executeStep []
|> List.sumBy Cuboid.count
|> printfn "%A"

#endif
