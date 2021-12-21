open System
open System.Collections.Generic
open System.IO

let startPositions =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.Substring("Player x starting position: ".Length) |> int)

#if part1

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

#else

type [<Struct>] Player = Player of position: int * score: int
type Universe =
    { Players: Player[]
      Turn: int }

module Universe =
    let hasWinner universe =
        universe.Players |> Array.exists (fun (Player (_, score)) -> score >= 21)
    let getWinnerId universe =
        universe.Players |> Array.findIndex (fun (Player (_, score)) -> score >= 21)

type Universes = (Universe * bigint) list

let rollUniverseCounts = [ (3, 1I); (4, 3I); (5, 6I); (6, 7I); (7, 6I); (8, 3I); (9, 1I) ]

let playUniverseRound (state: Universe, count: bigint) : Universes =
    rollUniverseCounts
    |> List.map (fun (roll, universeCount) ->
        let (Player (position, score)) = state.Players[state.Turn]
        let position = (position + roll - 1) % 10 + 1
        let score = score + position
        { Players = state.Players |> Array.updateAt state.Turn (Player (position, score))
          Turn = (state.Turn + 1) % state.Players.Length }, count * universeCount)

let playRound (universes: Universes) : Universes =
    universes
    |> List.collect playUniverseRound
    |> List.groupBy fst
    |> List.map (fun (u, list) -> u, list |> List.sumBy snd)

let playGame startPositions =
    let rec playGame winCounts universes =
        let won, universes =
            playRound universes 
            |> List.partition (fun (u, _) -> Universe.hasWinner u)
        let winCounts =
            won
            |> List.map (fun (u, count) -> Universe.getWinnerId u, count)
            |> List.append winCounts
            |> List.groupBy fst
            |> List.map (fun (id, list) -> id, list |> List.sumBy snd)
        if universes |> List.isEmpty
        then winCounts
        else playGame winCounts universes
    playGame [] [ { Players = startPositions |> Array.map (fun pos -> Player (pos, 0)); Turn = 0 }, 1I ]

playGame startPositions
|> List.sortByDescending snd
|> printfn "%A" 

#endif