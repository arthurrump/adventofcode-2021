open System
open System.IO

#if part1

File.ReadAllLines ("input.txt")
|> Seq.ofArray
|> Seq.fold (fun (x, d) cmd ->
    let [| cmd; arg |] = cmd.Split(' ')
    let arg = Int32.Parse(arg)
    match cmd with
    | "forward" -> (x + arg, d)
    | "down" -> (x, d + arg)
    | "up" -> (x, d - arg)
) (0, 0)
|> fun (x, d) -> x * d
|> printfn "%d"

#else

File.ReadAllLines ("input.txt")
|> Seq.ofArray
|> Seq.fold (fun (x, d, aim) cmd ->
    let [| cmd; arg |] = cmd.Split(' ')
    let arg = Int32.Parse(arg)
    match cmd with
    | "forward" -> (x + arg, d + aim * arg, aim)
    | "down" -> (x, d, aim + arg)
    | "up" -> (x, d, aim - arg)
) (0, 0, 0)
|> fun (x, d, _) -> x * d
|> printfn "%d"

#endif