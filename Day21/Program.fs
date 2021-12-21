open System
open System.IO

let startPositions =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.Substring("Player x starting position: ".Length) |> int)

let step (players: (int * int)[]) turn nextDie =
    let (position, score) = players[turn]
    let roll = nextDie * 3 + 3
    let position = (position + roll - 1) % 10 + 1
    let score = score + position
    let players = players |> Array.updateAt turn (position, score)
    let turn = (turn + 1) % players.Length
    let nextDie = (nextDie + 3 - 1) % 100 + 1
    // printfn "Player %d rolls %d and moves to space %d for a total score of %d." (turn + 1) roll position score
    (players, turn, nextDie)

let game startPositions =
    let rec game round players turn nextDie =
        let players, turn, nextDie = step players turn nextDie
        if players |> Array.exists (fun (_, score) -> score >= 1000) 
        then players, round
        else game (round + 1) players turn nextDie
    game 1 (startPositions |> Array.map (fun pos -> pos, 0)) 0 1

let finalPlayers, rounds = game startPositions
let losingScore = finalPlayers |> Array.minBy snd |> snd
printfn "%d" (losingScore * rounds * 3)
