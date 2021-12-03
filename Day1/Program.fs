open System
open System.IO

#if part1

File.ReadAllLines("input.txt") 
|> Seq.ofArray
|> Seq.map (Int32.Parse)
|> Seq.pairwise
|> Seq.filter (fun (x, y) -> y > x)
|> Seq.length
|> printfn "%d"

#else

File.ReadAllLines("input.txt") 
|> Seq.ofArray
|> Seq.map (Int32.Parse)
|> Seq.windowed 3
|> Seq.map (Array.sum)
|> Seq.pairwise
|> Seq.filter (fun (x, y) -> y > x)
|> Seq.length
|> printfn "%d"

#endif
