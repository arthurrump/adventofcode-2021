open System
open System.IO

type Board = (int * bool) list list

module Board =
    let markNumber n (board: Board) : Board =
        [ for row in board ->
            [ for (i, marked) in row ->
                if i = n then (i, true) else (i, marked)
            ] 
        ]

    let hasMarkedRow (board: Board) =
        board |> List.exists (List.forall (fun (_, marked) -> marked))

    let hasMarkedColumn (board: Board) =
        board |> List.transpose |> hasMarkedRow

    let hasWon board = hasMarkedRow board || hasMarkedColumn board

    let unmarkedSum (board: Board) =
        board 
        |> List.collect id 
        |> List.filter (fun (_, marked) -> not marked)
        |> List.sumBy (fun (i, _) -> i)

let input = File.ReadAllLines("input.txt")

let numbers = input.[0].Split(',') |> Array.map (Int32.Parse) |> Array.toList

let boards: Board list =
    input
    |> List.ofArray
    |> List.skip 2
    |> List.map (fun str -> 
        str.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map (Int32.Parse) 
        |> Array.map (fun i -> (i, false))
        |> Array.toList)
    |> List.fold (fun (currentBoard::boards) line -> 
        if List.isEmpty line
        then [] :: currentBoard :: boards
        else (currentBoard @ [ line ]) :: boards) [ [] ]
    |> List.filter (not << List.isEmpty)

#if part1

let mutable markedBoards = boards
let mutable lastNumber = 0
let mutable nextNumbers = numbers
while not (markedBoards |> List.exists Board.hasWon) do
    markedBoards <- markedBoards |> List.map (Board.markNumber nextNumbers.Head)
    lastNumber <- nextNumbers.Head
    nextNumbers <- nextNumbers.Tail

markedBoards 
|> List.find (Board.hasWon)
|> fun board -> Board.unmarkedSum board * lastNumber
|> printfn "%d"

#else

let rec findLastWinningBoard boards (numbers: int list) =
    let boards = boards |> List.map (Board.markNumber numbers.Head)
    if boards |> List.forall Board.hasWon then
        (boards |> List.exactlyOne, numbers.Head)
    else
        findLastWinningBoard (boards |> List.filter (not << Board.hasWon)) (numbers.Tail)

findLastWinningBoard boards numbers 
|> fun (board, lastNumber) -> Board.unmarkedSum board * lastNumber
|> printfn "%d"

#endif