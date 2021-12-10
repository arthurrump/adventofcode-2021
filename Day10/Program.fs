open System
open System.IO

module Array =
    let median arr =
        arr 
        |> Array.sort
        |> Array.item (Array.length arr / 2)

let lines =
    File.ReadAllLines("input.txt")

let isOpen ch = ch = '(' || ch = '[' || ch = '{' || ch = '<'
let isClose ch = ch = ')' || ch = ']' || ch = '}' || ch = '>'

let matches open' close =
    open' = '(' && close = ')' ||
    open' = '[' && close = ']' ||
    open' = '{' && close = '}' ||
    open' = '<' && close = '>'

let closing = function
    | '(' -> ')' | '[' -> ']' | '{' -> '}' | '<' -> '>'

let illegalCharacterScore = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137

let autocompleteCharacterScore = function
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4

let autocompleteScore = 
    List.map (autocompleteCharacterScore >> bigint)
    >> List.reduce (fun score charScore -> 5I * score + charScore)

type LineCorrect =
    | Correct
    | Corrupted of found: char
    | ClosingUnopened of found: char
    | Incomplete of remainingStack: char list

let checkCorrect (line: string) =
    let rec checkCorrect stack line =
        match line with
        | [] -> 
            if stack = [] 
            then Correct 
            else Incomplete stack
        | ch::rest ->
            if isOpen ch then
                checkCorrect (ch::stack) rest
            else // isClose ch
                if stack = [] then 
                    ClosingUnopened ch
                else
                    if matches (List.head stack) ch then
                        checkCorrect (List.tail stack) rest
                    else
                        Corrupted ch
    checkCorrect [] (line.ToCharArray() |> Array.toList)

let rec autocomplete openStack =
    match openStack with
    | [] -> []
    | ch::rest -> (closing ch) :: autocomplete rest

#if part1

lines
|> Array.map checkCorrect
// |> fun arr -> printfn "%A" arr; arr
|> Array.choose (function Corrupted ch -> Some ch | _ -> None)
|> Array.map illegalCharacterScore
|> Array.sum
|> printfn "%d"

#else

lines
|> Array.map checkCorrect
|> Array.choose (function Incomplete stack -> Some stack | _ -> None)
|> Array.map (autocomplete >> autocompleteScore)
|> Array.median
|> printfn "%A"

#endif