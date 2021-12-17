open System
open System.IO

type Target =
    { MinX: int
      MaxX: int
      MinY: int
      MaxY: int }

let isInTarget target (x, y) =
    x >= target.MinX
    && x <= target.MaxX
    && y >= target.MinY
    && y <= target.MaxY

let overshootsTarget target (x, y) (vx, vy) =
    (vy < 0 && y < target.MinY) || (vx <= 0 && x < target.MinX) || (vx >= 0 && x > target.MaxX)

let target =
    let [| xRange; yEquals |] = File.ReadAllText("input.txt").Trim().Substring("target area: x=".Length).Split(", ")
    let [| tx1; tx2 |] = xRange.Split("..") |> Array.map int
    let [| ty1; ty2 |] = yEquals.Substring("y=".Length).Split("..") |> Array.map int
    { MinX = min tx1 tx2; MaxX = max tx1 tx2; MinY = min ty1 ty2; MaxY = max ty1 ty2 }

let step (vx, vy) (x, y) =
    (((if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else vx), vy - 1), (x + vx, y + vy))

let stepUntilTarget target (vx, vy) =
    let rec stepUntilTarget steps (vx, vy) (x, y) =
        if isInTarget target (x, y) then
            Some steps
        else if overshootsTarget target (x, y) (vx, vy) then
            None
        else
            let (vx, vy), (x, y) = step (vx, vy) (x, y)
            stepUntilTarget ((x, y)::steps) (vx, vy) (x, y)
    stepUntilTarget [] (vx, vy) (0, 0)

let minVx = int (floor (sqrt (float target.MinX * 2.)))
let maxVx = target.MaxX
let maxVy = abs target.MinY
let minVy = - maxVy

seq { minVy..maxVy }
|> Seq.map (fun vy -> seq { for vx in minVx..maxVx -> (vx, vy) })
|> Seq.map (Seq.map (fun (vx, vy) -> stepUntilTarget target (vx, vy)))
|> Seq.collect (Seq.choose id)
#if part1
|> Seq.collect id
|> Seq.map snd
|> Seq.max
|> printfn "%d"
#else
|> Seq.length
|> printfn "%d"
#endif
