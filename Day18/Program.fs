open System
open System.IO

type SnailfishNumber = 
    | Number of int
    | Pair of SnailfishNumber * SnailfishNumber

let rec parse (numberString: string) : SnailfishNumber * string =
    if numberString[0] = '[' then
        let left, rest = parse numberString[1..]
        if rest[0] <> ',' then failwith "expected ','"
        let right, rest = parse rest[1..]
        if rest[0] <> ']' then failwith "expected ']'"
        Pair (left, right), rest[1..]
    else
        let number = int numberString[0] - int '0'
        Number number, numberString[1..]

let input = 
    File.ReadAllLines("input.txt")
    |> Array.map (parse >> fst)

let add n m =
    Pair (n, m)

let reduceNested n =
    let rec addToRightMost value = function
        | Number n -> Number (n + value)
        | Pair (l, r) -> Pair (l, addToRightMost value r)
    let rec addToLeftMost value = function
        | Number n -> Number (n + value)
        | Pair (l, r) -> Pair (addToLeftMost value l, r)
    let rec reduceNested level = function
        | Number n -> 
            Number n, (None, None)
        | Pair (Number l, Number r) when level = 4 ->
            Number 0, (Some l, Some r)
        | Pair (l, r) ->
            let newL, (addLeft, addRight) = reduceNested (level + 1) l
            if newL <> l then
                Pair (newL, match addRight with Some addR -> addToLeftMost addR r | None -> r), (addLeft, None)
            else
                let newR, (addLeft, addRight) = reduceNested (level + 1) r
                Pair ((match addLeft with Some addL -> addToRightMost addL l | None -> l), newR), (None, addRight)
    fst (reduceNested 0 n)

let rec splitRegular = function
    | Number n when n >= 10 ->
        Pair (Number (int (floor (float n / 2.))), Number (int (ceil (float n / 2.))))
    | Number n ->
        Number n
    | Pair (l, r) ->
        let newL = splitRegular l
        if newL <> l then
            Pair (newL, r)
        else
            let newR = splitRegular r
            Pair (l, newR)

let reduceStep n =
    let newN = reduceNested n
    if newN <> n
    then newN
    else splitRegular n

let rec reduce n =
    let next = reduceStep n
    if next = n
    then n
    else reduce next

let rec magnitude = function
    | Number n -> n
    | Pair (l, r) -> 3 * (magnitude l) + 2 * (magnitude r)

#if part1
input
|> Array.reduce (fun n m -> reduce (add n m))
|> magnitude
|> printfn "%d"
#else
Seq.allPairs input input
|> Seq.map (fun (n, m) -> magnitude (reduce (add n m)))
|> Seq.max
|> printfn "%d"
#endif